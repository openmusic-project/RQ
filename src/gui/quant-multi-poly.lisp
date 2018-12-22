

(in-package :rq)

;;;; Quant-multi-poly
;;;; Contains all the functions relative to the multi-poly object (the "k-best" view of the rq, right panel).
;; A special 'multi-poly' allowing to choose a current solution for each segment.
(defclass! quant-multi-poly ()
  ((polys :accessor polys :initform (list (make-instance 'poly :voices (list (make-instance 'voice :tree '(1 (((4 4) (-1)))))))) :initarg :polys)
   (current-poly :accessor current-poly :initform 0 :initarg :current-poly) ;; current poly in editor
   (selections :accessor selections :initform '(0) :initarg :selections) ;;; there is one selected voice for each poly/segment
   (distances :accessor distances :initform (list (list 0)) :initarg :distances) ;List of list holding the distance value for each proposition (one list of size n-sol) for each segment
   (gracenotes :accessor gracenotes :initform '((0)) :initarg :gracenotes)  ;List of list holding the number of gracenotes at the end of the solution for each proposition (one list of size n-sol) for each segment
   (list-n-solution :accessor list-n-solution :initform '(1) :initarg :list-n-solution))) ;list holding the n-solution for each poly

(defmethod class-has-editor-p  ((self quant-multi-poly)) t)
(defmethod get-editor-class ((self quant-multi-poly)) 'multi-poly-editor)

;;; NOT USED ANYMORE : TEMPO RECONSTRUCTED FROM CHOICE-TABLES IN VOICE, NOT FROM THE SELECTED TRANSCRIPTIONS.
(defmethod get-tempo ((self quant-multi-poly))
  "Returns a list of tempi compatible with a voice corresponding to the tempi of the selected transcriptions."
  (let* ((selections (selections self))
         (select-voices (loop for poly in (polys self)
                              for select in selections collect
                              (nth select (om::voices poly))))
         (tempi (loop for voice in select-voices collect (om::tempo voice)))
         (tempo1 (car tempi))
         (prev-temp tempo1)
         (tempo2 nil)
         )

    (when (rest tempi)
      (setq tempo2 (loop for temp in (rest tempi)
                                 for i = 1 then (1+ i)
                                 unless (equalp prev-temp temp)
                                 collect (list (list i 0) (car temp))
                                 do (setq prev-temp temp))))
    (append (list (car tempo1)) (list tempo2))
    ))


