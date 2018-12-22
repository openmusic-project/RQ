(in-package :rq)

;;Quant-chord-seq
;;special chord-seq to allow different treatments when inside a RQ system

(defclass! quant-chord-seq (chord-seq)
           ((parent :accessor parent :initval nil)
            (pulse-flag :accessor pulse-flag :initarg :pulse-flag :initform nil)))


(defmethod chord-seq->quant-chord-seq ((chord-seq chord-seq))
  "Translates a `chord-seq' into a `quant-chord-seq'."
  ;; ensure input is time-ordered
  (let ((cs (if (apply #'<= (om::lonset chord-seq))
		chord-seq
		(let ((cs (om::clone chord-seq)))
		  (om-beep-msg "rq: re-ordering input chord-seq to ascending time")
		  (setf cs (om::temporal-sort cs))
		  cs)))) 
    (make-instance 'quant-chord-seq 
		   :lmidic (om::lmidic cs)
		   :lonset (om::lonset cs)
		   :ldur (om::ldur cs)
		   :lvel (om::lvel cs)
		   :loffset (om::loffset cs)
		   :lchan (om::lchan cs)
		   :legato (om::legato cs))))

(defmethod chord-seq->quant-chord-seq ((quant-chord-seq quant-chord-seq))
  quant-chord-seq)

(defmethod get-segment-chords ((self quant-chord-seq) (segment om::segment))
  "Returns the list of all the notes (in midic) inside the segment (including the last of the previous segment when it overlaps the current segment)"
  (let ((begin (om::segment-begin segment))
        (end (om::segment-end segment))
        (onsets (om::lonset self))
        (chords (om::lmidic self))
        (slur (slur (segment-data segment)))
        out)
    (setq out (loop for on in onsets for chord in chords when (and (>= on begin)  (< on end)) collect chord ))
    ;When the last note of the previous segment overlaps the current segment
    (when slur 
      (let* ((prev-segment (om::previous-segment segment))
             (prev-chords (get-segment-chords self prev-segment)))
        (setq out (append (last prev-chords) out ))))
    out))

(defmethod get-k-best-segment-chords ((self quant-chord-seq))
  "Gets the chords for each segment, separated in lists"
  (let* ((k-best-analysis (get-k-best-analysis self))
         (analysis-segments (analysis-segments k-best-analysis)))
    (mapcar #'get-segment-chords (make-list (length analysis-segments) :initial-element self) analysis-segments)))
