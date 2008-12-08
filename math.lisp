;;; math.lisp --- math and geometry routines

;; Copyright (C) 2008  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(in-package :rlx)

(defun roll (rolls &optional (sides 6) (adds 0))
  "Total ROLLS rolls of a SIDES-sided die, then add ADDS.
So 2d6+2 would be (roll 2 6 2)."
  (let ((total 0))
    (+ adds
       (dotimes (r rolls total)
	 (incf total (+ 1 (random sides)))))))

(defun distance (x1 y1 x2 y2)
  "Compute the distance between the points X1,Y1 and X2,Y2."
  (let ((delta-x (- x2 x1))
	(delta-y (- y2 y1)))
    (sqrt (+ (* delta-x delta-x) (* delta-y delta-y)))))

(defvar *compass-directions* (list :north :south :east :west
				   :northeast :southeast
				   :northwest :southwest :here)
  "List of keywords representing the eight compass directions, plus :here.")

(defvar *compass-opposites* (list :north :south
				  :south :north
				  :east :west
				  :west :east
				  :northeast :southwest
				  :southwest :northeast
				  :southeast :northwest
				  :northwest :southeast
				  :here :here)
  "Property list mapping direction keywords to their 180-degree
opposites.")

(defun opposite-direction (direction)
  "Return the direction keyword that is the opposite direction from
DIRECTION."
  (getf *compass-opposites* direction))

(defun random-direction ()
  (nth (random (length *compass-directions*))
       *compass-directions*))

(defun step-in-direction (row column direction)
  "Return the point ROW, COLUMN moved by one square in DIRECTION."
  (ecase direction
    (:here (values row column))
    (:north (values (- row 1) column))
    (:south (values (+ row 1) column))
    (:east  (values row (+ column 1)))
    (:west  (values row (- column 1)))
    (:northeast (values (- row 1) (+ column 1)))
    (:northwest (values (- row 1) (- column 1)))
    (:southeast (values (+ row 1) (+ column 1)))
    (:southwest (values (+ row 1) (- column 1)))))

(defun direction-to (r1 c1 r2 c2)
  "Return general direction of the ray from R1,C1 to R2,C2."
  (if (or (some #'null (list r1 c1 r2 c2))
	  (and (= r1 r2) (= c1 c2)))
      :here
      (if (< r1 r2) ; definitely to the south
	  (if (< c1 c2)
	      :southeast
	      (if (> c1 c2)
		  :southwest
		  :south))
	  (if (> r1 r2) ;; definitely to the north
	      (if (< c1 c2)
		  :northeast
		  (if (> c1 c2)
		      :northwest
		      :north))
	      ;; rows are equal; it's either east or west
	      (if (< c1 c2)
		  :east
		  :west)))))

;;; Functions that trace out shapes

(defun trace-rectangle (trace-function row column height width &optional fill)
  "Call TRACE-FUNCTION for each point on the rectangle of HEIGHT and
WIDTH with top left corner at ROW COLUMN. When FILL is non-nil, fill
the rectangle."
  (block tracing
    (dotimes (r height)
      ;; Are we painting a full horizontal? (always the case when filling)
      (if (or fill (equal r 0) (equal r (- height 1)))
	  (dotimes (c width)
	    (if (funcall trace-function (+ r row) (+ c column))
		(return-from tracing)))
	  ;; no, it's a row with only verticals. just paint the left and right.
	  (if (or (funcall trace-function (+ r row) column)
		  (funcall trace-function (+ r row) (+ width column -1)))
	      (return-from tracing))))))

(defun trace-octagon (trace-function center-row center-column radius &optional thicken)
  "Call TRACE-FUNCTION for each point on the octagon of radius RADIUS
centered at row ROW, column COLUMN. When THICKEN is non-nil, thicken
the diagonals of the rectangle in order to facilitate raycasting.
It's an ugly hack, but it helps reduce artifacts."
  ;; calculate
  (let* ((origin-row (- center-row radius))
	 (origin-column (- center-column radius))
	 (side-length radius)
	 (angle-length (floor (/ (float radius) 2.0)))
	 (starting-x (+ 1 angle-length)))
    ;; draw top line
    (dotimes (i side-length)
      (funcall trace-function
	       origin-row
	       (+ origin-column starting-x i)))
    ;; draw top angles
    (dotimes (i angle-length)
      ;; left side
      (funcall trace-function
	       (+ 1 origin-row i)
	       (- center-column angle-length i 1))
      ;; right side
      (funcall trace-function
	       (+ 1 origin-row i)
	       (+ center-column angle-length i 1))
      ;;
      (when thicken
	;; left side
	(funcall trace-function
		 (+ 1 origin-row i)
		 (- center-column angle-length i))
	;; right side
	(funcall trace-function
		 (+ 1 origin-row i)
		 (+ center-column angle-length i))))
    ;; fill in diagonal points that are along the sides
    (when thicken
      ;; left side
      (funcall trace-function
	       (+ 1 origin-row angle-length)
	       (+ origin-column 1))
      ;; right side
      (funcall trace-function
	       (+ 1 origin-row angle-length)
	       (+ center-column side-length -1)))
    ;; draw side lines
    (dotimes (i side-length)
      ;; leftside
      (funcall trace-function
	       (+ 1 origin-row angle-length i)
	       origin-column)
      ;; right side
      (funcall trace-function
	       (+ 1 origin-row angle-length i)
	       (+ origin-column (* 2 side-length))))
    ;; fill in diagonal points that are along the sides
    (when thicken
	  ;; left side
	  (funcall trace-function
		   (+ origin-row side-length angle-length)
		   (+ origin-column 1))
	  ;; right side
	  (funcall trace-function
		   (+ origin-row side-length angle-length)
		   (+ center-column side-length -1)))
    ;; draw bottom angles
    (dotimes (i angle-length)
      ;; left side
      (funcall trace-function
	       (+ 1 origin-row angle-length side-length i)
	       (- center-column angle-length (- angle-length i)))
      ;; right side
      (funcall trace-function
	       (+ 1 origin-row angle-length side-length i)
	       (+ center-column angle-length (- angle-length i)))
      (when thicken
	;; left side
	(funcall trace-function
		 (+ 1 origin-row angle-length side-length i)
		 (+ 1 (- center-column angle-length (- angle-length i))))
	;; right side
	(funcall trace-function
		 (+ 1 origin-row angle-length side-length i)
		 (+ center-column angle-length (- angle-length i 1)))))
    ;; draw bottom line
    (dotimes (i side-length)
      (funcall trace-function
	       (+ 1 origin-row side-length (* 2 angle-length))
	       (+ origin-column starting-x i)))))

;;; Line of sight and lighting

;; :. lighting >

;; We use Bresenham's line algorithm to trace out the player's field
;; of vision and determine which squares are lit.

;; See also http://en.wikipedia.org/wiki/Bresenham's\_line\_algorithm

(defun trace-vertical-line (trace-function x y0 y1)
  (dotimes (n (abs (- y1 y0)))
    (funcall trace-function x (+ y0 n))))

(defun trace-line (trace-function x0 y0 x1 y1)
  "Trace a line between X0,Y0 and X1,Y1.
calling TRACE-FUNCTION at each point of the line.

Returns non-nil if tracing was successful, and nil if failed."
  ;; analyze coordinates and prepare them for bresenham's
  (let ((steep (> (abs (- y1 y0))
		  (abs (- x1 x0))))
	(flipped (> x0 x1)))
    ;; reflect steep lines through line y=x
    (when steep
      (rotatef x0 y0)
      (rotatef x1 y1))
    ;; swap points if line is backwards
    (when flipped
      (rotatef x0 x1)
      (rotatef y0 y1))
    (prog1 flipped
      (if (= x1 x0)
	  ;; just trace a vertical line.
	  (if flipped
	      (trace-vertical-line trace-function x1 y0 y1)
	      (trace-vertical-line trace-function x1 y1 y0))
	  ;; ok, use bresenham's
	  (let* ((delta-x (- x1 x0))
		 (delta-y (abs (- y1 y0)))
		 (err 0.0)
		 (delta-err (/ (float delta-y) (float delta-x)))
		 (y y0)
		 (x x0)
		 (step-y (if (< y0 y1) 1 -1)))
	    ;; main loop
	    (block tracing
	      (loop while (/= x x1) do
		 ;; call the supplied trace function.
		 ;; note that trace functions get args in order (row column).
		 ;; terminate with result = nil if it returns non-nil.
		   (if (if steep
			   (funcall trace-function x y)
			   (funcall trace-function y x))
		       (return-from tracing))
		   (incf err delta-err)
		   (when (>= err 0.5)
		     (incf y step-y)
		     (decf err 1.0))
		 ;; for next iteration
		   (incf x))))))))

;;; Random midpoint displacement fractals

;; The following routines create random midpoint displacement fractals
;; on a grid. This is useful for natural-looking world generation.
;; See also http://en.wikipedia.org/wiki/Diamond-square_algorithm

;; My implementation is slow and needs to be improved.

;; First comes the midpoint formula.
;; http://en.wikipedia.org/wiki/Midpoint

(defun midpoint (A B)
  (list (truncate (/ (+ (first A) (first B)) 2))
	(truncate (/ (+ (second A) (second B)) 2))))

;; We need an representation for a rectangle that is appropriate to
;; our problem. Then we must allow recursive subdivision of
;; rectangles.

(defstruct plasma-rect
  A B C D)

(defun subdivide-rect (R)
  "Subdivide rectangle R into four rectangles joined at the
center point of the original R, and return the list of four
rectangles, or NIL if they would be smaller than one pixel."
  (let* ((A (plasma-rect-A R))
	 (B (plasma-rect-B R))
	 (C (plasma-rect-C R))
	 (D (plasma-rect-D R)))
    ;; are they too small?
    (if (> 2 (abs (- (first C) (first A))))
	nil
	(let
	    ((R1 (make-plasma-rect :A A
				   :B (midpoint A B)
				   :C (midpoint A C)
				   :D (midpoint A D)))
	     ;;
	     (R2 (make-plasma-rect :A (midpoint A B)
				   :B B
				   :C (midpoint B C)
				   :D (midpoint B D)))
	     ;;
	     (R3 (make-plasma-rect :A (midpoint A C)
				   :B (midpoint B C)
				   :C C
				   :D (midpoint C D)))
	     ;;
	     (R4 (make-plasma-rect :A (midpoint A D)
				   :B (midpoint B D)
				   :C (midpoint C D)
				   :D D)))
	  (list R1 R2 R3 R4)))))

(defun render-plasma (height width &key (graininess 1.0) array)
  (let* ((grid (or array (make-array (list height width))))
	 (A (list 0 0))
	 (B (list 0 (- height 1)))
	 (C (list (- width 1) 0))
	 (D (list (- width 1) (- height 1)))
	 (Rs (list (make-plasma-rect :A A :B B :C C :D D)))
	 (Ss nil)
	 (S nil)
	 (R nil)
	 (rect-width nil))
    ;; assign random values to corners of grid to prime the algorithm
    (dolist (P (list A B C D))
      (setf (aref grid (second P) (first P)) (random graininess)))
    ;; begin processing rectangles and painting plasma
    (loop while (setf R (pop Rs))
       do
       ;; subdivide rectangle R and push results onto the rectangle list Rs
	 (setf Ss (subdivide-rect R))
	 (if Ss
	     (loop while (setf S (pop Ss)) do
		  (push S Rs)))
       ;; calculate values for midpoints and center of current rectangle R
	 (setf A (plasma-rect-A R))
	 (setf B (plasma-rect-B R))
	 (setf C (plasma-rect-C R))
	 (setf D (plasma-rect-D R))
	 (setf rect-width (abs (- -1 (first C) (first A))))
       ;; do for all edge midpoints and center:
	 (dolist (pair (list (list A B) (list A C)
			     (list B D) (list C D) (list A D)))
	   (let* ((P1 (first pair))
		  (P2 (second pair))
		  (M (midpoint P1 P2))
		  (V (+
		      ;; average value of values at P1 and P2
		      (* 0.5
			 (+ (aref grid (second P1) (first P1))
			    (aref grid (second P2) (first P2))
			    ;; random part smaller as rects get smaller
			    (* graininess (- 0.5 (random 1.0))
			       (sqrt (float rect-width))))))))
	     ;; paint the point
	     (setf (aref grid (second M) (first M)) V))))
    grid))

;;; Pathfinding
;; :. pathfinding >

;; ;;
;; ;; What follows is an implementation of the well-known A* pathfinding
;; ;; algorithm\footnote{http://en.wikipedia.org/wiki/A-star\_search\_algorithm}
;; ;; on a rectangular grid.
;; ;;
;; ;; The nodes are implemented as structures with the following slots: 

;; (defstruct rlx-node 
;;   row 
;;   column
;;   parent ; previous node along generated path
;;   F ; node score, equal to G + H
;;   G ; movement cost to move from starting point
;;     ; to (row, column) along generated path
;;   old-G ; previous value of G
;;   H ; heuristic cost to reach goal from (row, column)
;;   closed ; equal to world's path-turn-number when on closed list
;;   open ; equal to world's path-turn-number when on open list
;;   )

;; (defun rlx-print-heap (heap heap-end)
;;   (let ((output "HEAP: "))
;;     (dotimes (i heap-end)
;;       (setf output (concat output (format " %S" (rlx-node-F (aref heap (+ 1 i)))))))
;;     output))

;; ;; The following routines maintain the open and closed sets. We
;; ;; use a minheap to store the open set.

;; (defun rlx-open-node (world node)
;;   (let* ((path-heap-end (if (null (rlx-world-path-heap-end world))
;; 			    (setf (rlx-world-path-heap-end world) 1)
;; 			  (incf (rlx-world-path-heap-end world))))
;; 	 (path-heap (rlx-world-path-heap world))
;;  	 (ptr path-heap-end)
;; 	 (parent nil)
;; 	 (finished nil))
;;     ;;
;;     ;; make it easy to check whether node is open
;;     (setf (rlx-node-open node) (rlx-world-path-turn-number world))
;;     ;;
;;     ;; add node to end of heap 
;;     (setf (aref path-heap path-heap-end) node)
;;     ;;
;;     ;; let node rise to appropriate place in heap
;;     (while (and (not finished) (< 1 ptr))
;;       (setf parent (/ ptr 2))
;;       ;; should it rise? 
;;       (if (< (rlx-node-F node) (rlx-node-F (aref path-heap parent)))
;; 	  ;;
;; 	  ;; yes. swap parent and node
;; 	  (progn 
;; 	    (setf (aref path-heap ptr) (aref path-heap parent))
;; 	    (setf ptr parent))
;; 	;;
;; 	;; no. we're done.
;; 	(setf finished t)
;; 	(setf (aref path-heap ptr) node)))
;;     ;;
;;     ;; do we need to set node as the new root? 
;;     (if (and (not finished) (equal 1 ptr))
;; 	(setf (aref path-heap 1) node))))

;; (defun rlx-close-node (world)
;;   (let* ((path-heap (rlx-world-path-heap world))
;; 	 ;; save root of heap to return to caller
;; 	 (node (aref path-heap 1))
;; 	 (last nil)
;; 	 (path-heap-end (rlx-world-path-heap-end world))
;; 	 (ptr 1)
;; 	 (left 2)
;; 	 (right 3)
;; 	 (finished nil))
;;     ;; is there only one node?
;;     (if (equal 1 path-heap-end)
;; 	(setf (rlx-world-path-heap-end world) nil)
;;       (if (null path-heap-end)
;; 	  nil
;; 	;;
;; 	;; remove last node of heap and install as root of heap
;; 	(setf last (aref path-heap path-heap-end))
;; 	(setf (aref path-heap 1) last)
;; 	;; 
;; 	;; shrink heap
;; 	(decf (rlx-world-path-heap-end world))
;; 	(decf path-heap-end)
;; 	;;
;; 	(setf (rlx-node-closed node) (rlx-world-path-turn-number world))
;; 	;;
;; 	;; figure out where former last element should go
;; 	;;
;; 	(while (and (not finished) (>= path-heap-end right))
;; ;	  (message "HEAPING /// %s" (rlx-print-heap path-heap path-heap-end))
;; 	  ;;
;; 	  ;; does it need to sink? 
;; 	  (if (and (< (rlx-node-F last) (rlx-node-F (aref path-heap left)))
;; 		   (< (rlx-node-F last) (rlx-node-F (aref path-heap right))))
;; 	      ;;
;; 	      ;; no. we're done
;; 	      (progn 
;; 		(setf finished t)
;; 		(setf (aref path-heap ptr) last))
;; 	    ;;
;; 	    ;; does it need to sink rightward?
;; 	    (if (>= (rlx-node-F (aref path-heap left)) 
;; 		    (rlx-node-F (aref path-heap right)))
;; 		;;
;; 		;; yes
;; 		(progn
;; 		  (setf (aref path-heap ptr) (aref path-heap right))
;; 		  (setf ptr right))
;; 	      ;;
;; 	      ;; no, sink leftward
;; 	      (setf (aref path-heap ptr) (aref path-heap left))
;; 	      (setf ptr left)))
;; 	  (setf left (* 2 ptr))
;; 	  (setf right (+ 1 left)))
;; 	;;
;; 	;; 
;; 	(if (and (equal left path-heap-end)
;; 		 (> (rlx-node-F last)
;; 		    (rlx-node-F (aref path-heap left))))
;; 	    (setf ptr left))))
;;     ;;
;;     ;; save former last element in its new place
;;     (setf (aref path-heap ptr) last)
;;     node))

;; ;; The ordinary distance algorithm is used to score nodes.
    
;; (defun rlx-score-node (world node path-turn-number new-parent-node goal-row goal-column)
;;   "Update scores for NODE. Update heap position if necessary."
;;   (let* ((direction (rlx-direction-to (rlx-node-row new-parent-node)
;; 				      (rlx-node-column new-parent-node)
;; 				      (rlx-node-row node)
;; 				      (rlx-node-column node)))
;; 	 (G (+ 1 (rlx-node-G new-parent-node)))
	       
;;  	 (H (* (max (abs (- (rlx-node-row node) goal-row))
;; 		    (abs (- (rlx-node-column node) goal-column)))
;; 	       1.001))
;; 	 (F (+ G H)))
;;     ;; 
;;     ;; is this a new node, i.e. not on the open list? 
;;     (if (not (equal path-turn-number (rlx-node-open node)))
;; 	;;
;; 	;; yes, update its scores and parent
;; 	(progn 
;; 	  (setf (rlx-node-G node) G)
;; 	  (setf (rlx-node-H node) H)
;; 	  (setf (rlx-node-F node) F)
;; 	  (setf (rlx-node-parent node) new-parent-node))
;;       ;;
;;       ;; no, it's already open. is the path through NEW-PARENT-NODE
;;       ;; better than through the old parent?
;;       (if (and (rlx-node-G node)
;; 	       (< G (rlx-node-G node)))
;; 	  ;;
;; 	  ;; yes. update scores and re-heap.
;; 	  (let ((heap (rlx-world-path-heap world))
;; 		(heap-end (rlx-world-path-heap-end world))
;; 		(ptr 1)
;; 		(par nil)
;; 		(finished nil))
;; 	    (setf (rlx-node-G node) G)
;; 	    (setf (rlx-node-H node) H)
;; 	    (setf (rlx-node-F node) F)
;; 	    (setf (rlx-node-parent node) new-parent-node)
;; 	    ;;
;; 	    (message "Better score found.")
;; 	    ;; 
;; 	    ;; find current location of node in heap
;; 	    (while (and (not finished) (< ptr heap-end))
;; 	      (when (equal node (aref heap ptr))
;; 		(message "Found node.")
;; 		;;
;; 		;; its score could only go down, so move it up in the
;; 		;; heap if necessary.
;; 		(while (and (not finished) (< 1 ptr))
;; 		  (setf par (/ ptr 2))
;; 		  ;;
;; 		  ;; should it rise? 
;; 		  (if (< (rlx-node-F node) (rlx-node-F (aref heap par)))
;; 		      ;;
;; 		      ;; yes. swap it with its parent
;; 		      (progn
;; 			(setf (aref heap ptr) (aref heap par))
;; 			(setf ptr par))
;; 		    ;;
;; 		    ;; no, we are done. put node in its new place.
;; 		    (setf finished t)
;; 		    (setf (aref heap ptr) node)))
;; 		;;
;; 		;; do we need to install the new node as heap root?
;; 		(when (and (not finished) (equal 1 ptr))
;; 		  (setf (aref heap 1) node)))
;; 	      ;;
;; 	      ;; keep scanning heap for the node
;; 	      (incf ptr)))
;; 	;;
;; 	;; new score is not better. do nothing.
;; 	;(setf (rlx-node-parent node) new-parent-node)
;; 	))))
	      
;; (defun rlx-node-successors (world node path-turn-number goal-row goal-column)
;;   (delq nil 
;; 	(mapcar 
;; 	 (lambda (direction)
;; 	   (let* ((grid (rlx-world-grid world))
;; 		  (path-map (rlx-world-path-map world))
;; 		  (new-G (+ 1 (rlx-node-G node)))
;; 		  (step (rlx-step-in-direction 
;; 			(rlx-node-row node)
;; 			(rlx-node-column node)
;; 			direction))
;; 		  (r (first step))
;; 		  (c (second step))
;; 		  (successor nil))
;; 	     ;; 
;; 	     (if (rlx-bounds-check grid r c)
;; 		 (progn 
;; 		   (setf successor (rlx-grid-get path-map r c))
		   
;; 		   (if (or 
;; 			;; always allow the goal square even when it's an obstacle.
;; 			(and (equal r goal-row) (equal c goal-column))
;; 			;; ignore non-walkable squares and closed squares,
;; 			(and (not (rlx-first-in-category (rlx-grid-get grid r c)
;; 							 :obstacle))
;; 			     (not (equal path-turn-number (rlx-node-closed successor)))))
;; 		       ;; if successor is open and existing path is better
;; 		       ;; or as good as new path, discard the successor
;; 		       ;; if successor is not open, proceed 
;; 		       (if (equal path-turn-number (rlx-node-open successor))
;; 			   (if (< new-G (rlx-node-G successor))
;; 			       successor
;; 			     nil)
;; 			 successor)
;; 		     nil))
;; 	       nil)))
;; 	 rlx-compass-directions)))
	
;; ;; Now we come to the pathfinding algorithm itself. 

;; (defun rlx-path (world starting-row starting-column goal-row goal-column)
;;   "Generate a path from the starting point to the goal in WORLD.
;; Returns a list of directional keywords an AI can follow to reach
;; the goal."
;;   (let ((selected-node nil)
;; 	(path-turn-number (incf (rlx-world-path-turn-number world)))
;; 	(pos nil)
;; 	(found nil)
;; 	(target-node nil)
;; 	(path nil)
;; 	(F 0) (G 0) (H 0))
;;     ;;
;;     ;; reset the pathfinding heap
;;     (setf (rlx-world-path-heap-end world) nil)

;;     ;;
;;     ;; add the starting node to the open set
;;     (setf G 0)
;;     (setf H (max (abs (- starting-row goal-row))
;; 		 (abs (- starting-column goal-column))))
;;     (setf F (+ G H))
;;     (setf selected-node (make-rlx-node :row starting-row 
;; 				       :column starting-column
;; 				       :old-G 0
;; 				       :parent nil :G G :F F :H H))
;;     ;;
;;     (rlx-open-node world selected-node)
;;     ;;
;;     ;; start pathfinding
;;     (setq target-node
;; 	  (block finding
;; 	    ;;
;; 	    ;; select and close the node with smallest F score
;; 	    (while (setf selected-node (rlx-close-node world))
;; 	      ;;
;; 	      ;; did we fail to reach the goal? 
;; 	      (when (null selected-node)
;; 		(return-from finding nil))
;; 	      ;;
;; 	      ;; are we at the goal square?
;; 	      (when (and (equal goal-row (rlx-node-row selected-node))
;; 			 (equal goal-column (rlx-node-column selected-node)))
;; 		(return-from finding selected-node))
;; 	      ;;
;; 	      ;; process adjacent walkable non-closed nodes
;; 	      (mapc (lambda (node)
;; 		      ;;
;; 		      ;; is this cell already on the open list?
;; 		      (if (equal path-turn-number (rlx-node-open node))
;; 			  ;;
;; 			  ;; yes. update scores if needed
;; 			  (rlx-score-node world node path-turn-number
;; 					  selected-node goal-row goal-column)
			
;; 			;;
;; 			;; it's not on the open list. add it to the open list
;; 			(rlx-score-node world node path-turn-number selected-node
;; 					goal-row goal-column)
;; 			(rlx-open-node world node)))
;; 		    ;;
;; 		    ;; map over adjacent nodes
;; 		    (rlx-node-successors world selected-node 
;; 					 path-turn-number
;; 					 goal-row goal-column)))))
;;     ;;
;;     ;; did we find a path? 
;;     (if (rlx-node-p target-node)
;; 	;;
;; 	;; save the path by walking backwards from the target
;; 	(let ((previous-node target-node)
;; 	      (current-node nil))
;; 	  (while (setf current-node (rlx-node-parent previous-node))
;; 	    ;;
;; 	    ;; what direction do we travel to get from current to previous? 
;; 	    (push (rlx-direction-to (rlx-node-row current-node)
;; 				    (rlx-node-column current-node)
;; 				    (rlx-node-row previous-node)
;; 				    (rlx-node-column previous-node))
;; 		  path)
;; 	    (setf previous-node current-node))
;; 	  ;;
;; 	  ;; return the finished path
;; 	  path)
;;       ;;
;;       ;; return nil
;;       nil)))
	    
		  
    
;; (defsubst rlx-path-to (grid from to)
;;   (rlx-path grid 
;; 	    (getf from :row)
;; 	    (getf from :column)
;; 	    (getf to :row)
;;     (getf to :column)))

;;; math.lisp ends here
