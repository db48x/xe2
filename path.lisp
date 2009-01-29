;;; path.lisp --- A* pathfinding for RLX

;; Copyright (C) 2009  David O'Toole

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

;; This file contains unported Emacs Lisp code and will not compile.

;;; Code:

;; What follows is an implementation of the well-known A* pathfinding
;; algorithm on a rectangular grid.

;; http://en.wikipedia.org/wiki/A-star_search_algorithm

;; The nodes are implemented as structures with the following slots: 

(defstruct rlx-node 
  row 
  column
  parent ; previous node along generated path
  F ; node score, equal to G + H
  G ; movement cost to move from starting point
    ; to (row, column) along generated path
  old-G ; previous value of G
  H ; heuristic cost to reach goal from (row, column)
  closed ; equal to world's path-turn-number when on closed list
  open ; equal to world's path-turn-number when on open list
  )

;; The following routines maintain the open and closed sets. We
;; use a minheap to store the open set.

(defun rlx-open-node (world node)
  (let* ((path-heap-end (if (null (rlx-world-path-heap-end world))
			    (setf (rlx-world-path-heap-end world) 1)
			  (incf (rlx-world-path-heap-end world))))
	 (path-heap (rlx-world-path-heap world))
 	 (ptr path-heap-end)
	 (parent nil)
	 (finished nil))
    ;;
    ;; make it easy to check whether node is open
    (setf (rlx-node-open node) (rlx-world-path-turn-number world))
    ;;
    ;; add node to end of heap 
    (setf (aref path-heap path-heap-end) node)
    ;;
    ;; let node rise to appropriate place in heap
    (while (and (not finished) (< 1 ptr))
      (setf parent (/ ptr 2))
      ;; should it rise? 
      (if (< (rlx-node-F node) (rlx-node-F (aref path-heap parent)))
	  ;;
	  ;; yes. swap parent and node
	  (progn 
	    (setf (aref path-heap ptr) (aref path-heap parent))
	    (setf ptr parent))
	;;
	;; no. we're done.
	(setf finished t)
	(setf (aref path-heap ptr) node)))
    ;;
    ;; do we need to set node as the new root? 
    (if (and (not finished) (equal 1 ptr))
	(setf (aref path-heap 1) node))))

(defun rlx-close-node (world)
  (let* ((path-heap (rlx-world-path-heap world))
	 ;; save root of heap to return to caller
	 (node (aref path-heap 1))
	 (last nil)
	 (path-heap-end (rlx-world-path-heap-end world))
	 (ptr 1)
	 (left 2)
	 (right 3)
	 (finished nil))
    ;; is there only one node?
    (if (equal 1 path-heap-end)
	(setf (rlx-world-path-heap-end world) nil)
      (if (null path-heap-end)
	  nil
	;;
	;; remove last node of heap and install as root of heap
	(setf last (aref path-heap path-heap-end))
	(setf (aref path-heap 1) last)
	;; 
	;; shrink heap
	(decf (rlx-world-path-heap-end world))
	(decf path-heap-end)
	;;
	(setf (rlx-node-closed node) (rlx-world-path-turn-number world))
	;;
	;; figure out where former last element should go
	;;
	(while (and (not finished) (>= path-heap-end right))
;	  (message "HEAPING /// %s" (rlx-print-heap path-heap path-heap-end))
	  ;;
	  ;; does it need to sink? 
	  (if (and (< (rlx-node-F last) (rlx-node-F (aref path-heap left)))
		   (< (rlx-node-F last) (rlx-node-F (aref path-heap right))))
	      ;;
	      ;; no. we're done
	      (progn 
		(setf finished t)
		(setf (aref path-heap ptr) last))
	    ;;
	    ;; does it need to sink rightward?
	    (if (>= (rlx-node-F (aref path-heap left)) 
		    (rlx-node-F (aref path-heap right)))
		;;
		;; yes
		(progn
		  (setf (aref path-heap ptr) (aref path-heap right))
		  (setf ptr right))
	      ;;
	      ;; no, sink leftward
	      (setf (aref path-heap ptr) (aref path-heap left))
	      (setf ptr left)))
	  (setf left (* 2 ptr))
	  (setf right (+ 1 left)))
	;;
	;; 
	(if (and (equal left path-heap-end)
		 (> (rlx-node-F last)
		    (rlx-node-F (aref path-heap left))))
	    (setf ptr left))))
    ;;
    ;; save former last element in its new place
    (setf (aref path-heap ptr) last)
    node))

;; The ordinary distance algorithm is used to score nodes.
    
(defun rlx-score-node (world node path-turn-number new-parent-node goal-row goal-column)
  "Update scores for NODE. Update heap position if necessary."
  (let* ((direction (rlx-direction-to (rlx-node-row new-parent-node)
				      (rlx-node-column new-parent-node)
				      (rlx-node-row node)
				      (rlx-node-column node)))
	 (G (+ 1 (rlx-node-G new-parent-node)))
	       
 	 (H (* (max (abs (- (rlx-node-row node) goal-row))
		    (abs (- (rlx-node-column node) goal-column)))
	       1.001))
	 (F (+ G H)))
    ;; 
    ;; is this a new node, i.e. not on the open list? 
    (if (not (equal path-turn-number (rlx-node-open node)))
	;;
	;; yes, update its scores and parent
	(progn 
	  (setf (rlx-node-G node) G)
	  (setf (rlx-node-H node) H)
	  (setf (rlx-node-F node) F)
	  (setf (rlx-node-parent node) new-parent-node))
      ;;
      ;; no, it's already open. is the path through NEW-PARENT-NODE
      ;; better than through the old parent?
      (if (and (rlx-node-G node)
	       (< G (rlx-node-G node)))
	  ;;
	  ;; yes. update scores and re-heap.
	  (let ((heap (rlx-world-path-heap world))
		(heap-end (rlx-world-path-heap-end world))
		(ptr 1)
		(par nil)
		(finished nil))
	    (setf (rlx-node-G node) G)
	    (setf (rlx-node-H node) H)
	    (setf (rlx-node-F node) F)
	    (setf (rlx-node-parent node) new-parent-node)
	    ;;
	    (message "Better score found.")
	    ;; 
	    ;; find current location of node in heap
	    (while (and (not finished) (< ptr heap-end))
	      (when (equal node (aref heap ptr))
		(message "Found node.")
		;;
		;; its score could only go down, so move it up in the
		;; heap if necessary.
		(while (and (not finished) (< 1 ptr))
		  (setf par (/ ptr 2))
		  ;;
		  ;; should it rise? 
		  (if (< (rlx-node-F node) (rlx-node-F (aref heap par)))
		      ;;
		      ;; yes. swap it with its parent
		      (progn
			(setf (aref heap ptr) (aref heap par))
			(setf ptr par))
		    ;;
		    ;; no, we are done. put node in its new place.
		    (setf finished t)
		    (setf (aref heap ptr) node)))
		;;
		;; do we need to install the new node as heap root?
		(when (and (not finished) (equal 1 ptr))
		  (setf (aref heap 1) node)))
	      ;;
	      ;; keep scanning heap for the node
	      (incf ptr)))
	;;
	;; new score is not better. do nothing.
	;(setf (rlx-node-parent node) new-parent-node)
	))))
	      
(defun rlx-node-successors (world node path-turn-number goal-row goal-column)
  (delq nil 
	(mapcar 
	 (lambda (direction)
	   (let* ((grid (rlx-world-grid world))
		  (path-map (rlx-world-path-map world))
		  (new-G (+ 1 (rlx-node-G node)))
		  (step (rlx-step-in-direction 
			(rlx-node-row node)
			(rlx-node-column node)
			direction))
		  (r (first step))
		  (c (second step))
		  (successor nil))
	     ;; 
	     (if (rlx-bounds-check grid r c)
		 (progn 
		   (setf successor (rlx-grid-get path-map r c))
		   
		   (if (or 
			;; always allow the goal square even when it's an obstacle.
			(and (equal r goal-row) (equal c goal-column))
			;; ignore non-walkable squares and closed squares,
			(and (not (rlx-first-in-category (rlx-grid-get grid r c)
							 :obstacle))
			     (not (equal path-turn-number (rlx-node-closed successor)))))
		       ;; if successor is open and existing path is better
		       ;; or as good as new path, discard the successor
		       ;; if successor is not open, proceed 
		       (if (equal path-turn-number (rlx-node-open successor))
			   (if (< new-G (rlx-node-G successor))
			       successor
			     nil)
			 successor)
		     nil))
	       nil)))
	 rlx-compass-directions)))
	
;; Now we come to the pathfinding algorithm itself. 

(defun rlx-path (world starting-row starting-column goal-row goal-column)
  "Generate a path from the starting point to the goal in WORLD.
Returns a list of directional keywords an AI can follow to reach
the goal."
  (let ((selected-node nil)
	(path-turn-number (incf (rlx-world-path-turn-number world)))
	(pos nil)
	(found nil)
	(target-node nil)
	(path nil)
	(F 0) (G 0) (H 0))
    ;;
    ;; reset the pathfinding heap
    (setf (rlx-world-path-heap-end world) nil)

    ;;
    ;; add the starting node to the open set
    (setf G 0)
    (setf H (max (abs (- starting-row goal-row))
		 (abs (- starting-column goal-column))))
    (setf F (+ G H))
    (setf selected-node (make-rlx-node :row starting-row 
				       :column starting-column
				       :old-G 0
				       :parent nil :G G :F F :H H))
    ;;
    (rlx-open-node world selected-node)
    ;;
    ;; start pathfinding
    (setq target-node
	  (block finding
	    ;;
	    ;; select and close the node with smallest F score
	    (while (setf selected-node (rlx-close-node world))
	      ;;
	      ;; did we fail to reach the goal? 
	      (when (null selected-node)
		(return-from finding nil))
	      ;;
	      ;; are we at the goal square?
	      (when (and (equal goal-row (rlx-node-row selected-node))
			 (equal goal-column (rlx-node-column selected-node)))
		(return-from finding selected-node))
	      ;;
	      ;; process adjacent walkable non-closed nodes
	      (mapc (lambda (node)
		      ;;
		      ;; is this cell already on the open list?
		      (if (equal path-turn-number (rlx-node-open node))
			  ;;
			  ;; yes. update scores if needed
			  (rlx-score-node world node path-turn-number
					  selected-node goal-row goal-column)
			
			;;
			;; it's not on the open list. add it to the open list
			(rlx-score-node world node path-turn-number selected-node
					goal-row goal-column)
			(rlx-open-node world node)))
		    ;;
		    ;; map over adjacent nodes
		    (rlx-node-successors world selected-node 
					 path-turn-number
					 goal-row goal-column)))))
    ;;
    ;; did we find a path? 
    (if (rlx-node-p target-node)
	;;
	;; save the path by walking backwards from the target
	(let ((previous-node target-node)
	      (current-node nil))
	  (while (setf current-node (rlx-node-parent previous-node))
	    ;;
	    ;; what direction do we travel to get from current to previous? 
	    (push (rlx-direction-to (rlx-node-row current-node)
				    (rlx-node-column current-node)
				    (rlx-node-row previous-node)
				    (rlx-node-column previous-node))
		  path)
	    (setf previous-node current-node))
	  ;;
	  ;; return the finished path
	  path)
      ;;
      ;; return nil
      nil)))
	    
		  
    
(defsubst rlx-path-to (grid from to)
  (rlx-path grid 
	    (getf from :row)
	    (getf from :column)
	    (getf to :row)
    (getf to :column)))

;;; path.lisp ends here
