
(in-package :rq)

;;; Multi-poly Editor
;;; All the functions related to the editor in the right panel
;;; It is built as a poly editor, but the poly displayed can be changed (one of the poly of the polys list of the multi-poly object)



(defclass! multi-poly-editor (om::polyEditor) 
  ((parent :accessor parent :initval nil)
   (scroll-pos-list :accessor scroll-pos-list :initval '(0)) ;a list that holds the scroll position for all polys. Initialised with init-scroll-pos-list
   )) 

(defmethod om::get-score-class-panel ((self multi-poly-editor)) 'multi-poly-panel)


;; Two functions to save the zoom level for each panel (chord-seq, voice and multi-poly) independently (and not one zoom level for all)

(defmethod om::set-edit-param  ((self multi-poly-editor) param newval)
  "To set a zoom level for the poly panel only"
  (if (equal param 'om::zoom)
      (om::set-edit-param (om::ref self) 'poly-zoom newval)
    (call-next-method)
))

(defmethod om::get-edit-param  ((self multi-poly-editor) param)
  "To get the zoom level of the poly panel"
  (if (equal param 'om::zoom)
      (om::get-edit-param (om::ref self) 'poly-zoom)
    (call-next-method)
))


;; Key handler for right panel.
;  Left-right : change segment (change displayed poly)
;  Up-down : change selected solution.
(defmethod om::handle-key-event ((self multi-poly-editor) char)
  "Handles key events for a `multi-poly-editor'."
  (cond ((equal char :om-key-right)
         (change-poly self 1))
	    ((equal char :om-key-left) 
         (change-poly self -1))
        ((equal char :om-key-up)
         (let ((mp (wp (om::object (parent self)))))
           (change-selected-solution self (max (1- (nth (current-poly mp) (selections mp))) 0))))
        ((equal char :om-key-down)
	 (let ((mp (wp (om::object (parent self)))))
	   (change-selected-solution self (min (1+ (nth (current-poly mp) (selections mp))) (1- (length (om::voices (nth (current-poly mp)  (polys mp) ))))))))
        (t (call-next-method)))
  )


;; Initialises the scroll positions in order for the selected solutions to be displayed when changing poly.
;  The scroll positions are computed from fixed values.
;  It would be better if they were computed from real, on-the-go computed values 
;  (the real height of the grap-voice objects inside the grap-poly for exemple)
;  but initialization problems make it difficult.
(defmethod init-scroll-pos-list ((self multi-poly-editor))
  "Initialises the scroll position list for all polys"
  (let* ((qs (om::object (parent self)))
         (mp (wp qs))
         (selections (selections mp))
         (n-sols (get-list-n-solution qs))
         )
    (setf (scroll-pos-list self) (loop for select in selections
                                       for n-sol in n-sols
                                       collect 
                                       ;;workaround to display the selected voice
                                       ;Spacing between 2 voices : 73 ; Spacing between 2 inputs : 22
                                       (max 0 (+ -100 (* select 73) (* (floor select n-sol) 22)))  
                                       )))
)

(defmethod update-scroll ((self multi-poly-editor))
  "Scroll the right panel to the correct position (from the scroll-pos-list) to display the selected solution"
  (init-scroll-pos-list self)
  (om-set-scroll-position (om::panel self) (om-make-point (om-h-scroll-position (om::panel self)) (nth (current-poly (wp (om::object (parent self)))) (scroll-pos-list self)))))

(defmethod update-title ((self multi-poly-editor))
  "Displays the selected segment (and the selected voice in this segment) in the title bar of the multi-poly-editor's panel."
  (let ((mp (wp (om::object (parent self)))))
    (om::change-text (om::title-bar self) 
                 (format nil "SEGMENT ~A - SELEC= ~A"
                         (current-poly mp)
                         (nth (current-poly mp) (selections mp))))))

(defmethod update-poly ((self multi-poly-editor))
  "To update the poly-panel (redraw the poly, update the title bar)"
  (let ((mp (wp (om::object (parent self)))))
    (setf (om::object self) (nth (current-poly mp) (polys mp)))
    (setf (om::staff-sys (om::panel self)) (om::get-staff-system (om::correct-staff (om::panel self) (om::get-edit-param self 'om::staff))))
    (let* ((poly (om::object self))
           (n-solution (or (nth (current-poly mp) (list-n-solution mp)) 1))
           (ntempo  (floor (length (om::voices poly)) n-solution )))
      (om::score-system-space (om::panel self)  (loop for i from 1 to ntempo append (append (make-list (1- n-solution) :initial-element 2) (list 3)))))
  (update-title self)
  (om::update-panel (om::panel self))
))
                         
(defmethod change-poly ((self multi-poly-editor) increment)
  "Swiches the displayed poly to another (current + increment). Also focuses the view on the selected voice, and stores the previous scroll position."
  (let ((mp (wp (om::object (parent self))))
        (panel (om::panel self)))
    (when (> (length (polys mp)) 1)
      ;each time the poly is changed, we store the current scroll position in the scroll-pos-list to retrieve it when combing back to that poly
      (setf (nth (current-poly mp) (scroll-pos-list self)) (om-v-scroll-position panel)) 
      (setf (current-poly mp) (mod (+ (current-poly mp) increment) (length (polys mp))))
      (update-poly self)
      (om::update-subviews  (parent self))
      (focus-view (chord-seq-editor (parent self)) (current-poly mp))
      (focus-view (voice-editor (parent self)) (current-poly mp))
      ;The scroll position is retrieved from the list.
      (om-set-scroll-position panel (om-make-point (om-h-scroll-position panel) (nth (current-poly mp) (scroll-pos-list self)))) 
      )))

(defmethod change-selected-solution ((self multi-poly-editor) selected)
  "Changes the selected solution in the current poly. Also updates it in the solution (bottom panel)"
  (let* ((qs (om::object (parent self)))
         (mp (wp qs))
         (mes (current-poly mp)))
    ;Dialog box to ask for confirmation : when the selected solution is changed, it is directly replaced in the bottom panel
    ;and thus, all the modifications done on the corresponding measure are lost.
    (when (and (nth mes (choices (solution qs)))
               (or (not (modified (nth mes (choices (solution qs)))))
                   (om-y-or-n-dialog "The modifications done on this segment will be lost. Continue ?" :default-button :yes)))
      (setf (nth mes (selections mp)) selected)
      (update-title self)
      (update-ks-indexes (solution qs) mp)
      (init-one-choice (solution qs) mes)
      (set-tree (solution qs) (update-tree (solution qs) (not (editflag (solution qs)))))
      (update-grids (solution qs))
      (om::update-subviews (parent self))
      (focus-view self mp))))


;Focus view only works when changing the selected solution
;When changing poly, initialization problems in the positions of the voices causes it not to work (always scrolls back to top)
;When changing poly, the scroll-pos-list is used instead.
(defmethod focus-view ((self multi-poly-editor) (wp quant-multi-poly))
  "Makes the scroll follow the selection."
  (let* ((panel (om::panel self))
         (selected-index  (nth (current-poly wp) (selections wp)))
         (voices (om::inside (om::graphic-obj panel)))
         (selected-voice (nth selected-index voices))
         (v-scroll (om-v-scroll-position panel))
         (h-scroll (om-h-scroll-position panel)))
    (cond ((< (second (om::rectangle selected-voice)) v-scroll)
           (om-set-scroll-position panel (om-make-point h-scroll (- (second (om::rectangle selected-voice)) 30))))
          ((> (fourth (om::rectangle selected-voice)) (+ v-scroll (om::h panel)))
           (om-set-scroll-position panel (om-make-point h-scroll (+ (- (fourth (om::rectangle selected-voice)) (om::h panel)) 30)))))))

