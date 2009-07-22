
;;; Infested oxygenless ships 

(define-prototype freighter-world (:parent rlx:=world=)
  (width :initform 48)
  (height :initform 300)
  (ambient-light :initform :total)
  (pallet-size :initform 10))

(define-method generate freighter-world (&optional parameters)
  (declare (ignore parameters))
  (setf *ankh-generated-p* nil)
  (clon:with-field-values (height width pallet-size) self
    ;; create airless corridor space
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =vaccuum=) i j]))
    ;; create walls
    (labels ((drop-wall (x y)
	       (prog1 nil
		 [drop-cell self (clone =wall=) y x]))
	     (drop-box (x y)
	       (prog1 nil 
		 [drop-cell self (clone =tech-box=) y x :no-collisions t])))
      ;; create border around world
      (trace-rectangle #'drop-wall
		       0 0 width height)
      ;; drop wall blocks ("pallets")
      (let ((imax (1+ (truncate (/ width pallet-size))))
	    (jmax (1+ (truncate (/ height pallet-size)))))
	(dotimes (i imax)
	  (dotimes (j jmax)
	    ;; don't wall in the player
	    (when (not (= 0 i j))
	      (trace-rectangle #'drop-wall
			       (+ (random 3)
				  (* pallet-size i))
			       (+ (random 4) 
				  (* pallet-size j))
			       (random pallet-size)
			       (random pallet-size)
			       :fill)))))
      ;; drop groups of boxes
      (dotimes (i 34)
	(trace-rectangle #'drop-box (random height) (random width)
		       (+ 16 (random 14)) (+ 4 (random 3)) :fill)))
    ;; drop enemies
    (dotimes (i 40)
      (let ((row (random height))
	    (column (random width)))
	[drop-cell self (clone =berserker=) row column :loadout t :no-collisions t]))
    (dotimes (i 30) 
      [drop-cell self (clone =biclops=) (+ 60 (random (- height 80)))
		 (random width) :loadout t :no-collisions t])
    (dotimes (i 30)
      [drop-cell self (clone =scanner=) (+ 100 (random (- height 100)))
		 (random width) :loadout t :no-collisions t])
    (dotimes (i 20)
      [drop-cell self (clone =rook=) (+ 150 (random (- height 150))) (random width) :loadout t :no-collisions t])
    ;; drop dead crewmembers to ransack
    (dotimes (i 60) 
      [drop-cell self (clone =crew-member=) (random height) (random width) :loadout t :no-collisions t])
    ;; drop other stuff
    (dotimes (n 35)
      [drop-cell self (clone =med-hypo=) (random height) (random height) :no-collisions t])
    (dotimes (i 25)
      [drop-cell self (clone =oxygen-tank=) (random height) (random width) :no-collisions t])
    ;; 
    (setf *station-base-count* 0)
    (loop do (paint-station-piece self (+ 150 (random 140)) (random width) 10)
       while (< *station-base-count* 5))

    (dotimes (i 20)
      [drop-cell self (clone =energy=) (random height) (random width) :no-collisions t])
    (dotimes (i 4)
      [drop-cell self (clone =med-pack=) (random height) (random width) :no-collisions t])
    (dotimes (i 4)
      [drop-cell self (clone =level-up=) (random height) (random width) :no-collisions t])
    (dotimes (i 4)
      [drop-cell self (clone =defense-up=) (random height) (random width) :no-collisions t])
    (dotimes (i 4)
      [drop-cell self (clone =speed-up=) (random height) (random width) :no-collisions t])
    (dotimes (i 5)
      [drop-cell self (clone =muon-pistol=) (random height) (random width) :no-collisions t])
    (dotimes (i 5)
      [drop-cell self (clone =ion-shield=) (random height) (random width) :no-collisions t])
;;    [drop-cell self (clone =rusty-wrench=) 3 2 :no-collisions nil]
    (dotimes (i 20) 
      [drop-cell self (clone =mine=) (random height) (random width) :no-collisions t])))
