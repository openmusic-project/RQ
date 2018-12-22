
(in-package :rq)

;;; Quant-chord-seq Panel
;;; All the functions related to the top panel
;;; No "draw-object" redifinition here, the additional info of the top panel are included in k-best-analysis.lisp (function draw-segment-data)

(defclass! quant-chord-seq-panel (om::ChordSeqPanel) ())

;Double-click on a segment in the top panel to go to that segment in the right panel
(defmethod om-view-doubleclick-handler ((self quant-chord-seq-panel) where)
  "When double-clicking on a segment in the chord-seq panel, display this segment in the k-best panel."
  (when (plusp (om::pixels-to-time self (om-point-x where)))
    (let* ((time (om::pixels-to-time self (om-point-x where)))
           (k-best-analysis (get-k-best-analysis (om::object (om::editor self))))
           (segments (analysis-segments k-best-analysis))
           (begins (mapcar #'om::segment-begin segments))
           (selected (position-if #'(lambda (x) (<= x time)) begins :from-end t))
           (wp (wp (parent (om::object (om::editor self))))))
      (when selected
        (change-poly (poly-editor (parent (om::editor self))) (- selected (current-poly wp))))
      (om-invalidate-view self)))
  (call-next-method))
