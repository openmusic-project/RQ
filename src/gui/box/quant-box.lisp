
(in-package :om)

;;; Quant box
;;; A new class of box has been implemented to allow special treatments
;;; - Closing the GUI before evaluating the box
;;; - Setting the right scroll position to display the selected segments/solutions when opening the GUI



(defclass quant-box (OMBoxEditCall) ())

(defmethod get-type-of-ed-box ((self rq::rq))  'quant-box)

(defmethod omng-box-value :before ((self quant-box) &optional (numout 0))
  "To close the editor bevore re-evaluating the box"
 (when (and (editorframe self) (not (equal (allow-lock self) "x")))
    (om-close-window (om-view-window (editorframe self)))))



(defmethod OpenObjectEditor :after ((self quant-box))
  "To focus on the selected segment, measure and poly in the three panels when opening the editor"
  (let ((quant-editor (editorframe self)))
    (rq::update-scroll (rq::poly-editor quant-editor))
    (rq::focus-view (rq::chord-seq-editor quant-editor) (rq::current-poly (rq::wp (object quant-editor))))
    (rq::focus-view (rq::voice-editor quant-editor) (rq::current-poly (rq::wp (object quant-editor))))))
