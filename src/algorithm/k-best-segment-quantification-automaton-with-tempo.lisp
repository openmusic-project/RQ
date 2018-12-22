(in-package :rq)

;;;; Functions to run the algorithm (ie build a k-best structure) on one segment
;;   In this version, only 1 input and 1 table are created, and they take into account 1 tempo only (such that the segment's length is an integer multiple of the length of a beat)

(defun k-best-segment-quantify-automaton-with-tempo (onsets durations k schema segment-dur tempo &optional (precision 0.5) (gracepen 2) (initial-slur nil))
  "Version with a given tempo"
    (let* ((input (k-best-segment-create-input-with-tempo onsets (normalize-durations durations) schema tempo segment-dur precision gracepen initial-slur))
           (ktable (init (transitions input))))
      (rq-best-top ktable input k )
      (make-instance 'k-best :ktables (list ktable) :inputs (list input) :slur initial-slur :gracenotes (list (loop for i from 1 to k collect (k-best-segment-get-gracenotes ktable input i))) :n-solution k :tempomerge nil)))

(defun k-best-segment-create-input-with-tempo (onsets durations schema tempo segment-dur precision gracepen initial-slur)
  "Creates an input structure when the tempo is given. The tempo will be approximated so that the segment's length is an integer multiple of the length of a beat."
  (labels ((create-schema (onsets segment-dur schema tempo) 
             (let* ((beat-dur (/ 60000 tempo))
                    )
               (multiple-value-bind (nbeats rest) (round segment-dur beat-dur)
                 (append (list (max nbeats 1)) schema))))
           (append-initial-slur (onsets durations initial-slur)
             (if (and (equalp onsets '(0)) (null durations)) ;if the segment is empty
                 (if initial-slur
                     (values (list 0 initial-slur) (list initial-slur))
                   (valuea onsets durations))
               (if (zerop (first onsets))
                   (values onsets durations)
                 (if (< (first onsets) initial-slur)
                     (values (append (list 0) onsets) (append (list (first onsets)) durations))
                   (values (append (list 0) onsets) (append (list initial-slur) durations)))))))
    (if initial-slur
        (multiple-value-bind (onsets durations) (append-initial-slur onsets durations initial-slur)
          (k-best-segment-to-input onsets durations (create-schema onsets segment-dur schema tempo) segment-dur precision gracepen))
      (k-best-segment-to-input onsets durations (create-schema onsets segment-dur schema tempo) segment-dur precision gracepen))))

