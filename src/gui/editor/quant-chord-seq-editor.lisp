(in-package :rq)

;;; Quant-chord-seq Editor
;;; All the functions related to the editor in the top panel

(defclass! quant-chord-seq-editor (om::ChordSeqEditor)
           ((parent :accessor parent :initval nil)))

(defmethod om::get-score-class-panel ((self quant-chord-seq-editor)) 'quant-chord-seq-panel)

;; Two functions to save the zoom level for each panel (chord-seq, voice and multi-poly) independently (and not one zoom level for all)

(defmethod om::set-edit-param  ((self quant-chord-seq-editor) param newval)
  "To set a zoom level for the chord-seq panel only"
  (if (equal param 'om::zoom)
      (om::set-edit-param (om::ref self) 'chord-seq-zoom newval)
    (call-next-method)
))

(defmethod om::get-edit-param  ((self quant-chord-seq-editor) param)
  "To get the zoom level of the chord-seq panel"
  (if (equal param 'om::zoom)
      (om::get-edit-param (om::ref self) 'chord-seq-zoom)
    (call-next-method)
))

;; Key handler for top panel.
;  q : to quantify the selected segments
(defmethod om::handle-key-event ((self quant-chord-seq-editor) char)
  "Handles key events for a `quant-chord-seq-editor'."
  (cond ((equal char #\q) ;To quantify only the selected segments
         (when (om::selected-segments (get-k-best-analysis (om::object self)))
           (let* ((k-best-analysis (get-k-best-analysis (om::object self)))
		  (select-segment (om::selected-segments k-best-analysis))
		  (indexes (mapcar #'(lambda (segment) (position segment (analysis-segments k-best-analysis))) select-segment))
                  (qs (om::object (parent self))))
             (mapcar #'compute-one-k-best (make-list (length select-segment) :initial-element qs) select-segment)
             (mapcar #'replace-one-poly (make-list (length select-segment) :initial-element  qs) indexes)
             (update-ks-indexes (solution qs) (wp qs))
             (update-scroll (poly-editor (parent self)))
             (mapcar #'init-one-choice  (make-list (length select-segment) :initial-element  (solution qs)) indexes)
             (set-tree (solution qs) (update-tree (solution qs) (not (editflag (solution qs)))))
             (update-poly (parent self))
             (om-invalidate-view (om::panel self))
	     )))
        (t (call-next-method))))

(defmethod focus-view ((self quant-chord-seq-editor) n)
  "To make the scroll of the chord-seq panel go to the Nth segment"
  (let* ((panel (om::panel self))
         (v-scroll (om-v-scroll-position panel))
         (h-scroll (om-h-scroll-position panel))
         (chord-seq (om::object self))
         (segments (analysis-segments (get-k-best-analysis (om::object self))))
         (current-segment (nth n segments))
         (begin (om::time-to-pixels panel (om::segment-begin current-segment)))
         (end (om::time-to-pixels panel (om::segment-end current-segment))))
    (cond ((< begin (+ h-scroll 50))
           (om-set-scroll-position panel (om-make-point (max (- begin 150) 0) v-scroll)))
          ((> end (+ h-scroll (om::w panel)))
           (om-set-scroll-position panel (om-make-point (+ (- end (om::w panel)) 250) v-scroll))))))
