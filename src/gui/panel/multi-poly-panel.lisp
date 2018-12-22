
(in-package :rq)

;;; Multi-poly Panel
;;; All the functions related to the right panel

(defclass multi-poly-panel (om::polyPanel) 
  ())

; When the poly panel is clicked, updates the chord-seq panel in order to update the grid 
; (when a tempo is specified and the chord-seq panel is clicked, display the position of the beats, otherwise, display the quantization grid.)
(defmethod om-view-click-handler :after ((self multi-poly-panel) where)
  "Updates the chord-seq panel when the poly panel is clicked"
  (when (parent (om::editor self))
    (om::update-panel (om::panel (chord-seq-editor (parent (om::editor self)))))))

;Double-click on a voice to select it
(defmethod om-view-doubleclick-handler ((self multi-poly-panel) where)
  "Select a voice when double-clicking it."
  (when (parent (om::editor self))
    (let* ((y-click (om-point-y where))
           (voices (om::inside (om::graphic-obj self)))
           (y-voices (mapcar #'(lambda (x) (second (om::rectangle x))) voices))
           (selected (or (position-if #'(lambda (x) (<= x y-click)) y-voices :from-end t) 0)))
      (change-selected-solution (om::editor self) selected)
      )))


(defmethod om::draw-object :after ((self om::grap-voice) (view multi-poly-panel) x y zoom minx maxx miny maxy slot size linear? staff grille-p chnote)
  "Draws additional information (distance, number of grace-notes) for a multi-poly panel. Also highlights the selected voice on the current segment."
    (let* ((mp (wp (om::value (om::ref (om::editor view)))))
           (poly (om::parent (om::reference self)))
           (distlist (distances mp))
           (gracenotes (gracenotes mp))
           (current-voice (position (om::reference self) (om::voices poly)))
           (current-dist (nth current-voice (nth (current-poly mp) distlist)))
           (current-grace (nth current-voice (nth (current-poly mp) gracenotes))))
      
      ;Display distance
      (om-with-fg-color view (distance-to-color current-dist)
			(om-with-font *om-default-font1b*
				      (om-draw-string (+ (om::x view) 50 (om-h-scroll-position view)) (- (second (om::rectangle self)) 5) (format nil "Error = ~A%" current-dist))))
      ;Display number of gracenotes
      (unless
	  (or (null current-grace) (zerop current-grace))
	(om-with-fg-color view *om-gray-color*
			  (om-with-dashline '(2 2) 
					    (om-draw-line (+ (third (om::rectangle self)) 40) (- (second (om::rectangle self)) 10)
							  (+ (third (om::rectangle self)) 40) (+ (fourth (om::rectangle self))  5)))
			  (om-draw-string  (+ (third (om::rectangle self)) 50) (+ (second (om::rectangle self)) 25)  
					   (make-string current-grace :initial-element #\N))))
      ;Highlights selected segment
      (when (= (nth (current-poly mp) (selections mp)) current-voice)
        (om-with-fg-color view (om-make-color-alpha 0.6 0.4 0.4 0.4) 
			  (om-fill-rect (nth 0 (om::rectangle self)) (nth 1 (om::rectangle self)) 
					(- (nth 2 (om::rectangle self)) (nth 0 (om::rectangle self)))
					(- (nth 3 (om::rectangle self)) (nth 1 (om::rectangle self))))))))

