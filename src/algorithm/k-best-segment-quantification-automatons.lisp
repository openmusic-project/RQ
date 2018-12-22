
(use-package :om :rq)
(in-package :rq)

;;;; Functions to run the algorithm (ie build a k-best structure) on one segment
;;   In this version, one input and one table is created for each tempo, and each correspond to one tempo only.
;;   In this way, the solutions are ranked independently for each tempo.
;;   It gives the possibility to compute more solutions for one particular tempo.
;;   Called when "Merge tempi" is not ticked

(defun k-best-segment-quantify-automatons (onsets durations k schema segment-dur &optional (precision 0.5) (gracepen 2) (initial-slur nil) (tempo-min 40) (tempo-max 200))
  "Computes a k-best structure containing several ktables and several inputs, one for each tempo"
    (let* ((inputs (k-best-segment-create-inputs onsets (normalize-durations durations) schema segment-dur initial-slur tempo-min tempo-max precision gracepen))
           (ktables (loop for input in inputs collect (init (transitions input))))
           (N (length ktables)))
      (loop for ktable in ktables 
            for input in inputs do 
            (rq-best-top ktable input k ))
      (let ((gracenotes  (loop for i from 0 to (1- N) collect 
                               (loop for j from 1 to k collect (k-best-segment-get-gracenotes (nth i ktables) (nth i inputs) j)))))
        (make-instance 'k-best :ktables ktables :inputs inputs :slur initial-slur :gracenotes gracenotes :n-solution k :tempomerge nil))))

(defun k-best-segment-create-inputs (onsets durations schema segment-dur initial-slur tempo-min tempo-max precision gracepen)
  "this version returns a list of inputs, each corresponding to one tempo"
  (labels ((create-schemas (segment-dur schema) 
             (let* ((beat-min (/ 60000 tempo-max))
                    (beat-max (/ 60000 tempo-min))
                    (ndiv-min (max (round segment-dur beat-max) 1)) 
                    (ndiv-max (max (round segment-dur beat-min) 1)))
                (loop for i from ndiv-min to ndiv-max collect (append (list i) schema))))
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
    (let  ((schema-list (or (create-schemas segment-dur schema)
                            (list (append (list 1) schema)))))
      (if initial-slur
          (multiple-value-bind (onsets durations) (append-initial-slur onsets durations initial-slur)
            (k-best-segment-to-inputs onsets durations schema-list segment-dur precision gracepen))
        (k-best-segment-to-inputs onsets durations schema-list segment-dur precision gracepen)))))

(defun k-best-segment-to-inputs (onsets durations schema-list segment-dur precision gracepen)
  "Takes a list of onsets and a list of durations (of same length) and returns a list of input objects
Used in the case of multiple inputs"

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
      (loop for schema in schema-list collect
            (input-make schema (coerce output 'vector) (coerce mask 'vector) precision gracepen)))))
