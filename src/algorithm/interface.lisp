;;; Interfaces for k-best quantification
;; Most of the functions here are now obsolete (everything replaced by the GUI)

(use-package :om :rq)

(in-package :rq)

(export '(k-best-quantify) :rq)

(defun to-input (onsets durations schema sig tempo)
  "Takes a list of onsets and a list of durations (of same length) and returns an input object"
  (let ((mesdur (* (* (first sig) (/ 4 (second sig))) (/ 60 tempo) 1000)))
  ;creates the output list and the mask from the onsets and durations
  (labels ((out-mask (onsets durations output mask) 
             (if (or (null onsets) (null durations))
                 (values (append output (list mesdur)) (append mask (list -1)))
               (let ((on1 (first onsets))
                     (dur1 (first durations))
                     (on2 (second onsets))
                     (dur2 (second durations)))
                 (if (or (null on2) (null dur2)) ;on1 is the last note
                     (out-mask (cdr onsets) (cdr durations) (append output (list on1 (+ on1 dur1))) (append mask (list 1 -1)))
                   (if (< (+ on1 dur1) on2) ;if there is a silence between the current note and the next one
                         (out-mask (cdr onsets) (cdr durations) (append output (list on1 (+ on1 dur1))) (append mask (list 1 -1)))
                     (out-mask (cdr onsets) (cdr durations) (append output (list on1)) (append mask (list 1)))))))))
    (multiple-value-bind (output mask) (out-mask onsets durations nil nil)
      (input-make schema (coerce output 'vector) (coerce mask 'vector))))))

(defun cut-measures (onsets durations signatures tempo)
  " cuts the onsets and durations lists in measures
 the note series has to be monophonic, otherwise, the results are not guaranteed.
 output onsets are given as the duration between the beginning of the measure and the onset of the note

OUTPUTS :
  on-mes holds the onsets grouped in measures
  dur-mes holds the durations grouped in measures
  slur is a list : the nth element is 1 if the last note of the nth measure is slurred with the next one, nil otherwise"
    (labels ((make-measures (onsets durations sig temp &optional (on-mes nil) (dur-mes nil) (slur nil) (sigs nil) (temps nil))
                 (let* ((current-sig (first sig))
                        (current-temp (first temp))
                        (mes-duration (* (* (first current-sig) (/ 4 (second current-sig))) (/ 60 current-temp) 1000)) ;duration of a measure in ms
                        (end mes-duration) ;the beginning of the measure is always 0
                        (index (position-if #'(lambda (x) (>= x end)) onsets)) ;index of the first element of the next measures
                        (last-on (if index (nth (1- index) onsets) (car (last onsets))))   ;if there are no elements in the next measures (ie current measure is last), last on is the last onset, otherwise it is the index-th element of onsets
                        (last-dur (if index (nth (1- index) durations) (car (last durations))))  ;idem
                        (exceed (> (+ last-on last-dur) end)) ;true iff the last note of the measure lasts after the end of the measure
                        (new-sig (if (null (rest sig))  ;if there are not enough signatures or tempo given, we continue with the last one
                                     sig
                                   (rest sig)))
                        (new-temp (if (null (rest temp))
                                      temp
                                    (rest temp))))
                   (if (null exceed)
                       (if (null index)
                           ; the current measure is the last : there are no notes after the end of the measure
                           (values (append on-mes (list onsets)) (append dur-mes (list durations)) (append slur (list nil)) (append sigs (list current-sig)) (append temps (list current-temp)))
                         ;the current measure is not the last, we iterate
                         (make-measures (update-onsets (subseq onsets index) mes-duration) (subseq durations index) new-sig new-temp (append on-mes (list (subseq onsets 0 index))) (append dur-mes (list (subseq durations 0 index))) (append slur (list nil)) (append sigs (list current-sig)) (append temps (list current-temp))   ))
                     ;if the last note is cut by the measure bar, we cut it into two notes, and put 1 in slur (the measures are slurred)
                     (let* ((new-dur (- end last-on))
                           (excess (- last-dur new-dur)))
                       (if (null index)
                           ;if there are no notes after, we just add a measure for the end of the note
                           (values (append on-mes (list onsets) (list (list 0))) (append dur-mes (list (append (butlast durations) (list new-dur))) (list (list excess))) (append slur (list 1 nil)) (append sigs (list current-sig (car new-sig))) (append temps (list current-temp (car new-temp)))  )
                         ;if there are notes after, we iterate
                         (make-measures (append (list 0) (update-onsets (subseq onsets index) mes-duration)) (append (list excess) (subseq durations index)) new-sig new-temp (append on-mes (list (subseq onsets 0 index))) (append dur-mes (list (append (subseq durations 0 (1- index)) (list new-dur)))) (append slur (list 1))  (append sigs (list current-sig)) (append temps (list current-temp))))))))
             (update-onsets (onsets measure)
               (mapcar #'(lambda (x) (- x measure)) onsets))) 
      (if (listp (car signatures))   ;if a list of signatures is given 
          (if (listp tempo)          ;if a list of tempo is given
              () ;we do nothing
          ;if a list of signatures is provided, but only 1 tempo is given
            (setq tempo (make-list (length signatures) :initial-element tempo)))
      ;if only one signature is given
        (if (listp tempo)
            (setq signatures (make-list (length tempo) :initial-element signatures))
        ;if only one signature and one tempo is given
          (progn 
            (setq signatures (list signatures))
            (setq tempo (list tempo)))))
       (make-measures onsets (remove-overlap onsets (normalize-durations durations)) signatures tempo)))

(defun normalize-durations (durations &optional (buffer nil))
  "if durations is the output of a chord-seq, it is a list of lists of length 1, otherwise, the input is a normal list
This function transforms the durations output of a chord-seq into a normal list
The chord-seq has to be monophonic"
  (if (null durations)
      buffer
    (let ((current (car durations)))
      (if (listp current)
          (normalize-durations (rest durations) (append buffer (list (reduce #'max current))))
        (normalize-durations (rest durations) (append buffer (list current)))))))

(defun remove-overlap (onsets durations &optional (dur-buf nil))
  (let ((on1 (first onsets))
        (dur1 (first durations))
        (on2 (second onsets))
        (dur2 (second durations)))
    (if (or (null on2) (null dur2))
        (append dur-buf (list dur1))
      (if (< (+ on1 dur1) on2) ;if there is a silence between the current note and the next one)
          (remove-overlap (rest onsets) (rest durations) (append dur-buf (list dur1)))
        (remove-overlap (rest onsets) (rest durations) (append dur-buf (list (- on2 on1))))))))

(defun create-inputs (onsets durations schema sigs tempo schema-measure?)
  "returns a list of input objects, each corresponding to a measure"
  (labels ((create-schemas (siglist schema)
             (mapcar #'(lambda (sig) (append  (list (car sig)) schema)) siglist)))  
  (multiple-value-bind (onlist durlist slurs siglist templist) (cut-measures onsets durations sigs tempo)
    (if schema-measure?
        (values (mapcar #'to-input onlist durlist (make-list (length onlist) :initial-element schema) siglist templist) slurs siglist templist)
      (values (mapcar #'to-input onlist durlist (create-schemas siglist schema) siglist templist) slurs siglist templist)))))

(om::defmethod! k-best-quantify (onsets durations k schema sigs tempo &optional (precision 0.5) (schema-measure? nil))
                :initvals '((0) (1000) 10 '(4 2 2) '(4 4) 60 0.5 nil)
                :indoc '("onsets" "durations" "number of solutions" "schema" "signatures" "tempo" "precision (0.0-1.0)" "schema-measure?")
                :icon '(252)
                :doc "Quantizes a list of onsets and a list of durations. The output is given as a list of possible rhythm trees.
<onsets> : a list of onsets in 1/1000th of a second
<dusations> : a list of durations in 1/1000th of a second
<k> : number of output solutions
<schema> : a subdivision schema applied to each time unit (by default : each quarter note).  A subdivision schema is given in the form of a list. For example : (2 2 3) means that each time unit is divided in two, then each part is again divided in two, and finally, each of these parts is divided in 3.

If an element of the list is a list itself, it means that the choice is given between the values of the list. For example : (2 (2 3 5) 3) means (2 2 3) or (2 3 3) or (2 5 3).

If an element of the list is a list of lists, it means that the choice is given between various successions of subdivisions. For example : ( ((2 3) (3 2)) ) means (2 3) or (3 2).

Example : ((2 3) ((2 3) ((3 5) 2))) means (2 2 3) or (2 3 2) or (2 5 2) or (3 2 3) or (3 3 2) or (3 5 2).

 Non prime numbers can be used as, for exemple, dividing in 2 then in 2 gives a different notation than dividing in 4.

<sigs> : the signature to be used for each measure. If there are various signatures, a list of signatures can be given (ex : ((3 4) (4 4)) means the signature of the first measure is (3 4), and the signature of the following measures is (4 4))
<tempo> : the tempo to be used. If the tempo changes from measure to measure, a list of tempo can be given (ex : (60 50) means the tempo of the first measure is 60, and the tempo of the following measures is 50)

<precision> : a float in (0.0 .. 1.0). Smaller values mean 'simplicity' while bigger values mean 'precision'.

<schema-measure?> : a boolean. If true, the schema is applied to each measure. If false, the schema is applied to each pulse. False by default.
"
  (labels ((add-signatures (trees signatures &optional (buffer nil))
             (if (null trees)
                 buffer
               (add-signatures (cdr trees) (cdr signatures) (append buffer (list (loop for tree in (car trees) collect (list (car signatures) tree))))))))
  (multiple-value-bind (inputs slurs siglist templist) (create-inputs (butlast onsets) durations schema sigs tempo schema-measure?) 
    (let ((trees nil)
          (N (length inputs))
          (slurs (append '(0) slurs))) ;we add a 0 to math the indexes : a 1 in slurs now means the current measure is slurred with the previous, and not that the next is slurred with the current.
      (if (equalp *weight-precision* precision)
          ()
        (setf *weight-precision* precision))
      ;for each measure, computation of the k best solutions
      (setq trees (loop for n from 0 to (1- N) 
                        collect
                        (let ((ktable (init (transitions (nth n inputs))))) 
                          (loop for i from 1 to k
                                collect  (rq-to-omtree ktable (nth n inputs) i (nth n slurs))))))
      ;we add the signatures
      (setq trees (add-signatures trees siglist))
      ;we have a list of size n_measures, containing lists of length k. Instead, we want a list o length k, containing lists of size n_measure
      ;In other words, we want to have a list indexed by the rank of the solution, not by the number of the measure.
      (setq trees (loop for i from 0 to (1- k) collect 
                        (loop for tree in trees collect
                              (nth i tree))))
      (mapcar #'(lambda (x) (append (list '?) (list x)))  trees)))))

(defun rq-to-omtree (ktable inst k flag &optional (slur nil) (gracenotes 0) (path nil) (previous -1))
"Function to reconstruct a rhythm tree from the hashtable
 The slur parameter is nil when the current measure is not slurred with the previous one, an integer when it is"
  (let ((rootp (or path (path-empty inst)))
        (slurred nil) ;nil when the first note has not been slurred yet, t when if has.
        (grace (if gracenotes gracenotes 0)) ;number of grace notes in the previous step
        (prev (if previous previous -1))) ;prev : sign of the last input in the previous subdivision (1: note, -1: silence). By default : we begin with a silence
    (labels ((to-omtree (inst p k flag)
               (multiple-value-bind (run w) (rq-best ktable inst p k flag)
                 (let ((r (length run))
                       (y nil)) ;buffer to hold the sub-tree
                   (if (> r 0)
                    ;   (list 1 (mapcar #'(lambda (k i) (to-omtree inst (path-addi p r i) k)) run (from-1-to r)))
                       (list 1 (loop for i from 1 to r
                                     for krun in run
                                     collect (to-omtree inst (path-addi p r i) (car krun) (cdr krun))))
                     ;if the node considered is a leaf
                     (multiple-value-bind (ll rr) (align ktable p)
                       ;processing ll
                       (if (null ll) ;if there are no inputs aligned to the left
                           (if (> grace 0) ;if there were notes aligned to the right in the previous subdivision
                               (if (equalp grace 1) ;if there is only one note
                                   (setq y 1)
                                 (setq y (list 1 (append (make-list (1- grace) :initial-element 0) (list 1)))))
                             ;if there is no input in the subdivision and there was no note aligned to the right before
                             (if (equalp prev 1) 
                                   (setq y 1.0) ;if there was a note before, tie it to the current
                                 (setq y -1))) ;if there was a silence, return a silence
                         ;if there are inputs aligned to the left
                         (if (and (equalp grace 0) (equalp (length ll) 1)) 
                             (setq y (* (input-sign inst (car ll)) 1))  ;if there is only 1 input aligned to the left
                           (let ((ll0 (remove-silences ll inst)))
                             (if (null ll0) ;if there are only rests aligned to the left (at least 1)
                                 (if (> grace 0) ;if there were notes aligned to the right in the previous subdivision
                                     (if (equalp grace 1) ;if there is only one note
                                         (setq y 1)
                                       (setq y (list 1 (append (make-list (1- grace) :initial-element 0) (list 1)))))
                                   ;if there are only rests in the subdivision and there was no note aligned to the right before
                                   (setq y -1))
                               ;if there are notes aligned to the left
                               (if (and (equalp (length ll0) 1) (equalp grace 0)) ;if there is exactly one note aligned to the left
                                   (setq y 1)
                                 ;if there is at least 1 note aligned to the left or 1 note aligned to the right before
                                 (setq y (list 1 (append (make-list (1- (+ grace (length ll0))) :initial-element 0) (list 1)))))))))
                       ;if the current note is the first and if it has to be slurred, we slur it and remember that the first note has already been slurred
                       (if (and (null slurred) slur)
                           (progn
                             (if (listp y) ;if the note to slur has grace-notes : the exceeding part of the previous measure was aligned to the left so we delete it
                                 (if (equalp (second y) '(0 1))
                                     (setq y 1)
                                   (setq y (list 1 (rest (second y)))))
                               (setq y (float y)))
                             (setq slurred t)))
                         ;otherwise, we do nothing
                       ;processing rr
                       (if (null rr)
                           (if (not (null ll))
                               (setq prev (input-sign inst (car (last ll)))) ;if there are no notes aligned to the right, but there are some to the left
                             ;if there are no notes in the subdivision, we do nothing
                             )
                         (setq prev (input-sign inst (car (last rr)))))
                       (setq grace (length (remove-silences rr inst)))
                       y))))))
        (if (weight-infinityp (nth-value 1 (rq-best ktable inst rootp k flag)))
            (setq out -1)
          (setq out (to-omtree inst rootp k flag)))
        (list out))))

(defun rq-remove-top-level (tree)
  (if (or (equalp tree '(1)) (equalp tree '(-1)) (equalp tree '(1.0)) (equalp tree '(-1.0))) ;we remove the first top-level (1 (...)), except when we only have a 1
      tree
    (second (car tree))))

(defun output (tree &optional (sig '(1 4)))
  (list '? (list (list sig tree))))

(defun get-nth-measure (tree n)
  (nth n (second tree)))

(defun concatenate-measures (&rest trees)
  (list '? trees))
