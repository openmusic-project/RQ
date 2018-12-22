
(in-package :rq)

;;; Quant-chord-seq Editor
;;; All the functions related to the editor in the bottom panel

(defclass! quant-voice-editor (om::voiceEditor)
  ((parent :accessor parent :initval nil)))

(defmethod om::get-score-class-panel ((self quant-voice-editor)) 'quant-voice-panel)

;; Two functions to save the zoom level for each panel (chord-seq, voice and multi-poly) independently (and not one zoom level for all)

(defmethod om::set-edit-param  ((self quant-voice-editor) param newval)
  "To set a zoom level for the voice panel only"
  (if (equal param 'om::zoom)
      (om::set-edit-param (om::ref self) 'voice-zoom newval)
    (call-next-method)
))

(defmethod om::get-edit-param  ((self quant-voice-editor) param)
  "To get the zoom level of the voice panel"
  (if (equal param 'om::zoom)
      (om::get-edit-param (om::ref self) 'voice-zoom)
    (call-next-method)
))


;; Key handler for top panel.
;  e : to open the dialog box that allows to type manually a subtree
(defmethod om::handle-key-event ((self quant-voice-editor) char)
  "Handles key events for a `quant-voice-editor'."
  (cond ((equal char #\e)
         (when (om::selection? (om::panel self))
           (when (open-user-edit-panel (om::panel self) (car (om::selection? (om::panel self))))
             (set-tree (om::object self) (update-tree (om::object self) (not (editflag (om::object self)))))
           )))
        (t (call-next-method))))



(defmethod focus-view ((self quant-voice-editor) n)
  "To make the scroll of the voice panel go to the Nth measure"
  (let* ((panel (om::panel self))
         (v-scroll (om-v-scroll-position panel))
         (h-scroll (om-h-scroll-position panel))
         (measures (om::inside (om::graphic-obj panel)))
         (current-measure (nth n measures))
         (begin (first (om::rectangle current-measure)))
         (end (third (om::rectangle current-measure))))
    (cond ((< begin (+ h-scroll 50))
           (om-set-scroll-position panel (om-make-point (max (- begin 150) 0) v-scroll)))
          ((> end (+ h-scroll (om::w panel) -50))
           (om-set-scroll-position panel (om-make-point (+ (- end (om::w panel)) 250) v-scroll))))))
