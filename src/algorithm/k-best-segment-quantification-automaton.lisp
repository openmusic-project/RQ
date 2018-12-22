

(use-package :om :rq)
(in-package :rq)

;;;; Functions to run the algorithm (ie build a k-best structure) on one segment
;;   In this version, only 1 input and 1 table are created, and they take into account all admissible tempi.
;;   In this way, all the solutions are ranked across all possible tempi. The best solution thus also gives the best tempo.
;;   However, it is not possible to compute more solutions for one particular tempo.
;;   Called when "Merge tempi" is ticked

(defun k-best-segment-quantify-automaton (onsets durations k schema segment-dur &optional (precision 0.5) (gracepen 2) (initial-slur nil) (tempo-min 40) (tempo-max 200))
  "Computes a k-best structure with only one ktable (all tempi merged)"
    (let* ((input (k-best-segment-create-input onsets (normalize-durations durations) schema segment-dur initial-slur tempo-min tempo-max precision gracepen))
           (ktable (init (transitions input))))
      (rq-best-top ktable input k)
      ;put ktable and input inside a list for compatibility reasons
      (make-instance 'k-best :ktables (list ktable) :inputs (list input) :slur initial-slur :gracenotes (list (loop for i from 1 to k collect (k-best-segment-get-gracenotes ktable input i))) :n-solution k :tempomerge t)))

(defun k-best-segment-create-input (onsets durations schema segment-dur initial-slur tempo-min tempo-max precision gracepen)
  "Create one single input structure to quantify with all tempi merged"
  (labels ((create-schema (segment-dur schema) 
             (let* ((beat-min (/ 60000 tempo-max))
                    (beat-max (/ 60000 tempo-min))
                    (ndiv-min (max (round segment-dur beat-max) 1))
                    (ndiv-max (max (round segment-dur beat-min) 1)))
               (append (list (loop for i from ndiv-min to ndiv-max collect i)) schema)))
           (append-initial-slur (onsets durations initial-slur)
             (if (and (equalp onsets '(0)) (null durations)) ;if the segment is empty
                 (if initial-slur
                     (values (list 0 initial-slur) (list initial-slur))
                   (values onsets durations))
               (if (zerop (first onsets))
                   (values onsets durations)
                 (if (< (first onsets) initial-slur)
                     (values (append (list 0) onsets) (append (list (first onsets)) durations))
                   (values (append (list 0) onsets) (append (list initial-slur) durations)))))))
      (if initial-slur
          (multiple-value-bind (onsets durations) (append-initial-slur onsets durations initial-slur)
            (k-best-segment-to-input onsets durations (create-schema segment-dur schema) segment-dur precision gracepen))
        (k-best-segment-to-input onsets durations (create-schema segment-dur schema) segment-dur precision gracepen))))


(defun k-best-segment-to-input (onsets durations schema segment-dur precision gracepen)
  "Takes a list of onsets and a list of durations (of same length) and returns an input object.
Used in the case of one single input."
  ;creates the output list and the mask from the onsets and durations
  (labels ((out-mask (onsets durations output mask) 
             (if (or (null onsets) (null durations))
                 (values (append output (list segment-dur)) (append mask (list -1)))
               (let ((on1 (first onsets))
                     (dur1 (first durations))
                     (on2 (second onsets))
                     (dur2 (second durations)))
                 (if (or (null on2) (null dur2)) ;on1 is the last note
                       (if (> (+ on1 dur1) segment-dur)
                           (values (append output (list on1 segment-dur)) (append mask (list 1 -1)))
                         (out-mask (cdr onsets) (cdr durations) (append output (list on1 (+ on1 dur1))) (append mask (list 1 -1))))
                     (if (< (+ on1 dur1) on2) ;if there is a silence between the current note and the next one
                         (out-mask (cdr onsets) (cdr durations) (append output (list on1 (+ on1 dur1))) (append mask (list 1 -1)))
                       (out-mask (cdr onsets) (cdr durations) (append output (list on1)) (append mask (list 1)))))))))
    (multiple-value-bind (output mask) (out-mask onsets durations nil nil)
      (input-make schema (coerce output 'vector) (coerce mask 'vector) precision gracepen))))
