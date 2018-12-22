;;; K-best parsing for rhythm quantification
;; Author : Adrien Ycart

; The implemented algorithm is based on the one described in : Better k-best Parsing - L. Huang & D. Chiang, 2005 


(use-package :om :rq)

(in-package :rq)

; A run is a list of couples (index flag).
; If we consider a node that has n sons, the run will have n elements :
;   the first element is the index in the k-best list of the first son,
;   the second is the index in the k-best list of the second son,
;   etc.

(defun run-make (flaglist)
  (loop for flag in flaglist collect (cons 1 flag)))

; An entry of the ktable.
; ktable is a hashtable in which keys are paths

(defstruct (entry)
  (args)  ; the list of possible next subdivisions (0 always belongs to the list, 0 means we don't subdivise further)
  (dist)  ; distance object, holding the distance computed considering the node given by path is a leaf (see distance.lisp)
  (flagp) ;if flagp is nil, it means the gracenote flag cannot be true for this segment (no notes are aligned to the right)
  (cand)  ; heap holding candidate k-lists (ie couples (run . weight) for the next-best (initialized at (1 1 ... 1))) (see heap.lisp, run.lisp). runs can have different length (if different subdivisions are possible)
  (best)) ; ordered list of bests (ie lists (run  weight ll rr)). (see k-list.lisp, run.lisp). runs can have different length (if different subdivisions are possible)

(defstruct (cand)
  (cand1)  ;list of candidates when the gracenote flag is 1
  (cand0)) ;list of candidates when the gracenote flag is 0

(defun entry-candidates (e flag)
  (if flag
      (cand-cand1 (entry-cand e))
    (cand-cand0 (entry-cand e))))

(defstruct (best)
  (best1) ;list of bests when the gracenote flag is 1
  (best0)) ;list of bests when the gracenote flag is 0

(defun entry-bestlist (e flag)
  (if flag
      (best-best1 (entry-best e))
    (best-best0 (entry-best e))))

(defun cand-make (flagp)
  (if (null flagp)
      (make-cand  :cand0 (empty-queue) :cand1 nil)
    (make-cand  :cand0 (empty-queue) :cand1 (empty-queue))))


(defun print-table (table)
  (let ((sts 0) ; number of states
        (trs 0)) ; number of transitions
  (flet ((print-entry (p e)
           (let ((al (entry-args e))
                 (d  (entry-dist e)))
             (multiple-value-bind (ll lr) (distance-align d)
               (progn
                 (print-path p)
                 (format t " : args = ~a left = ~a right = ~a~%" al ll lr)
                 (setq sts (1+ sts))
                 (setq trs (+ trs (length al))))))))
    (progn 
      (format t "----------------------------------------------------------------------~%Transition table~%----------------------------------------------------------------------~%")
      (maphash #'print-entry table)
      (format t "----------------------------------------------------------------------~%")
      (format t "states :      ~d~%" sts)
      (format t "transitions : ~d~%" trs)
      (format t "----------------------------------------------------------------------~%")))))

(defun puthash (key value ktable)
  "add an entry to the hashtable"
  (setf (gethash key ktable) value))

(defun rq-linsert (e il)
  "insert an element in the args list
il : ordered int list without repetition"
  (if (null il)
      (list e)
    (let ((a (car il)))
      (cond ((< e a) (append (list e) il))
            ((> e a) (append (list a) (rq-linsert e (rest il))))
            (t il))))) ; a==e, we do not change anything (no repetition)
 
(defun register-state (ktable inst p)
  "create an entry, add it in the ktable (associated to path p) and return it
also initialises the distance"
  (let* ((dist (distance-make1 inst p))
         (rr (nth-value 1 (distance-align dist)))
         (flagp (not (null (remove-silences rr inst)))))
  (puthash p (make-entry :args (list 0) :dist dist :flagp flagp :cand (cand-make flagp) :best (make-best)) ktable)))

(defun inhabited (e)
  "true iff there are input points in the interval defined by the path associated to e"
  (distance-inhabitedp (entry-dist e)))

(defun transitions (inst)
  "construct transition table : create all the possible paths, and put it in the table associated to empty entries"
  ;creation of a new table
  (let  ((ktable (make-hash-table :test 'equalp)))
  (labels ((transitions-inner (inst schema p r i)
             (progn
               (transitions-state inst schema (path-addi p r i))
               (if (equalp r i) 
                   nil
                 (transitions-inner inst schema p r (1+ i)))))
           (transitions-state (inst schema p)
             (assert (null (nth-value 1 (gethash p ktable))))
             (let ((e (register-state ktable inst p)))
               (if (inhabited e)
                   (let* ((tops (schema-top schema))
                         (al (reduce #'rq-linsert tops :initial-value (entry-args e) :from-end t)))
                     (progn
                       (setf (entry-args e) al)
                       (map nil #'(lambda (x) (transitions-inner inst (schema-pop x schema) p x 1)) tops)))))))
    (let ((rootp (path-empty inst))) 
      (transitions-state inst (input-sch inst) rootp))) ;add all transitions with target state rootp (path empty, the root of the derivation tree);
  ktable))
  
(defun init (ktable)
  "initialises the candidates for the entries linked to each path created by the function transition
each entry's candidate list is initialised with a list (1 1... 1) of length r for each subdivision r possible"
  (labels ((init-cand (p e)
             (map nil #'(lambda (x) (add-heap p x e)) (entry-args e)))
           (add-heap (p n e)
             (let ((lflagps (get-flagps-sons p n e))
                   (cand (entry-cand e))
                   (flagp (entry-flagp e)))
               (labels ((add-run-flags (lflagps lastflag &optional (lflags nil))
                          (if (null lflagps) ;when all the possibilities are done
                              (if lastflag
                                  (setf (cand-cand1 cand) (queue-insert (cand-cand1 cand) (make-undef) (run-make lflags)))
                                (setf (cand-cand0 cand) (queue-insert (cand-cand0 cand) (make-undef) (run-make lflags))))
                            (if (null (cdr lflagps)) ;when only the last flag is left
                                (add-run-flags (cdr lflagps) lastflag (append lflags (list lastflag)))
                              (if (car lflagps) ;if flagp is t, we add both 1 and 0 flags, only 0 otherwise.
                                  (progn
                                    (add-run-flags (cdr lflagps) lastflag (append lflags (list t)))
                                    (add-run-flags (cdr lflagps) lastflag (append lflags (list nil))))
                                (add-run-flags (cdr lflagps) lastflag (append lflags (list nil))))))
                          ))
               ;if (last lflagps) is nil, we add nothing to cand1, and we add all possible combinations in cand0
               ;if (last lflagps) is t, we add all possible conbinations in cand1 and cand0
               (if (or (and flagp (car (last lflagps))) (and flagp (null lflagps)))  
                   (progn
                     (add-run-flags lflagps nil)
                     (add-run-flags lflagps t))
                 (add-run-flags lflagps nil)))))
           (get-flagps-sons (p n e)
             (loop for i from 1 to n collect (entry-flagp (gethash (path-addi p n i) ktable)))))
    (maphash #'init-cand ktable)
    ktable))

(defun rq-eval (ktable inst p run flag)
  "evaluate the weight of a tree given a run (ie the list of indices in the k-best list of each son)"
  (labels ((best-weights (pkl) ;for each element of the list pkl of elements (path . k-index), get the k-th best for the given path. if impossible, return weight infinity, othewise, return the list of weights.
             (if (null pkl) 
                 nil
               (let* ((current (first pkl))
                      (x (car current))
                      (k (cadr current))
                      (flag (cddr current))
                      (weight (nth-value 1 (rq-best ktable inst x k flag))))
                 (if (weight-infinityp weight)
                     (list (make-infinity)) 
                   (append (list weight) (best-weights (rest pkl))))))))
  (multiple-value-bind (e found) (gethash p ktable)
    (assert found)
    (let ((r (length run)))
      (if (equalp 0 r) ;if we do not subdivise futher, ie we have a leaf, the distance is already computed and stored in e (function register-state)
          (let ((d (entry-dist e))
                (c (complexity-make1 ktable inst p e flag))) 
            (weight-make d c (input-precision inst) (input-gracepen inst)))
        (let* ((pl (path-children p r))
              (pkl (mapcar #'cons pl run))
              (wl (best-weights pkl)))
          (weight-addall wl (input-precision inst) (input-gracepen inst))))))))



(defmethod rq-best (ktable inst p k flag)
  "returns the k-th best run and its weight given an input and a path (root if the tree for a whole measure is wanted)"
  (labels ((process-next (e run w)
             (multiple-value-bind (ll rr) (align-run ktable inst p run)
               (if (or (and (not (null run)) (is-redundant ktable inst p ll rr)) (not (check-flags run (length run)))) 
                   () ;if the current run gives a redundant alignment (ie equivalent to not subdividing further), we only add the nexts to the cand list (we do not put the run in the best list)
                 (if flag ;if not, we add it to the list and add the nexts to the cand list
                     (setf (best-best1 (entry-best e)) (k-list-add run w ll rr (best-best1 (entry-best e))))
                   (setf (best-best0 (entry-best e)) (k-list-add run w ll rr (best-best0 (entry-best e))))))
               (if flag
                   (map nil #'(lambda (x) (setf (cand-cand1 (entry-cand e)) (queue-insert (cand-cand1 (entry-cand e)) (make-undef) x))) (nexts run))
                 (map nil #'(lambda (x) (setf (cand-cand0 (entry-cand e)) (queue-insert (cand-cand0 (entry-cand e)) (make-undef) x))) (nexts run)))))
           (check-flags (run arity &optional (direction 1)) ;checks if all the flags are compatible (true iff note aligned to the right and note to the left in the next segment)
             (if (<= (length run) 1)
                 t
               (let* ((flag1 (cdar run))
                      (flag2 (cdadr run))
                      (k1 (caar run))
                      (k2 (caadr run))
                      (rr01 (remove-silences (nth-value 3 (rq-best ktable inst (path-addi p arity direction) k1 flag1)) inst))
                      (ll02 (remove-silences (nth-value 2 (rq-best ktable inst (path-addi p arity (1+ direction)) k2 flag2)) inst)))
                 (if (or (and ll02 flag1) (and (null flag1) (or (null ll02) (null rr01))))
                     (check-flags (cdr run) arity (1+ direction))
                   nil)))))
    (let* ((e (gethash p ktable))
           (bests (entry-best e))
           (best (if flag (best-best1 bests) (best-best0 bests)))
           (candidates (entry-cand e))
           (cand (if flag (cand-cand1 candidates) (cand-cand0 candidates))))
      (if (<= k (length best)) ;if the k-th best has already been computed
          (let ((result (k-list-ith (1- k) best)))
            (values (first result) (second result) (third result) (fourth result)))
        (if (queue-empty cand) ;if there is no candidate left (ie all the possible k-bests have already been computed)
            (values nil (make-infinity))
          (multiple-value-bind (w run cands) (queue-extract cand) ;otherwise, compute the next best
            (if flag
                (setf (cand-cand1 (entry-cand e)) cands)
              (setf (cand-cand0 (entry-cand e)) cands))
            (if (weight-defp w)
                (progn
                  (process-next e run w)
                  (rq-best ktable inst p k flag)) ;repeat until k-best is constructed
              (let ((w0 (rq-eval ktable inst p run flag))) 
                (if (weight-defp w0) ;if the weight of the candidate was undefined and has been successfully computed
                    (progn
                      (if flag
                          (setf (cand-cand1 (entry-cand e)) (queue-reinsert cands w0 run))
                        (setf (cand-cand0 (entry-cand e)) (queue-reinsert cands w0 run)))
                      (rq-best ktable inst p k flag))
                  (rq-best ktable inst p k flag)))))))))) ;otherwise, the candidate cannot be weighted, discard it and retry with next

(defun nexts (list)
  "given a run (i1, i2, ... , in), returns a list of n runs (i1+1, i2, ..., in), (i1, i2+1, ... , in), ..., (i1, i2, ... , in+1)"
  (if (null list) 
      nil
    (let* ((a (car list))
          (l (rest list))
          (n (append (list (cons (1+ (car a)) (cdr a))) l)))
      (append (list n) (reverse (mapcar #'(lambda (x) (append (list a) x)) (nexts l)))))))

(defmethod align (ktable p)
  "Returns the list of input points aligned to the left and to the right of the segment given by p"
  (distance-align (entry-dist (gethash p ktable))))

(defmethod notes-align (ktable inst p)
  "Returns the list of input NOTES aligned to the left and to the right of the segment given by p (rests are discarded)"
  (distance-notes-align (entry-dist (gethash p ktable)) inst))

(defun rq-printk (ktable inst k)
  (let ((rootp (path-empty inst)))
    (multiple-value-bind (run w) (rq-best ktable inst rootp k)
      (progn
        (format t "~%~d-best = ~%" k)
        (format t "weight=")
        (print-weight w)
        (format t " run=~a ~%" run)))))

(defun remove-silences (list inst)
  "Removes from the list all the input points that correspond to the beginning of a rest (not in-place)"
  (remove-if #'(lambda (x) (input-silencep inst x)) list))

(defun rq-to-ratio-from-top (ktable inst k &optional (slur nil))
  (let ((out nil)
        (prev (if slur 1 -1))
        (grace 0))
  (labels ((to-ratio (p k) 
             (multiple-value-bind (run w) (rq-best ktable inst p k)
               (let ((r (length run)))
                 (if (zerop r)
                     ;if we have a leaf
                     (let ((len (path-len p))) 
                       (multiple-value-bind (ll rr) (align ktable p)
                         ;processing ll
                         (if (null ll) ;if there are no inputs aligned to the left
                             (if (> grace 0) ;if there were notes aligned to the right in the previous subdivision
                                 (if (equalp grace 1) ;if there is only one note
                                     (setq out (append out (list len)))
                                   (setq out (append out  (make-list (1- grace) :initial-element 0) (list len))))
                               ;if there is no input in the subdivision and there was no note aligned to the right before
                               (if (equalp prev 1) 
                                   (if (> (car (last out)) 0)
                                       (setf (car (last out)) (+ (car (last out)) len))
                                     (setq out (append out (list len))))
                                 (if (> (car (last out)) 0)
                                     (setq out (append out (list (- len))))
                                   (setf (car (last out))  (- (car (last out)) len))))) 
                           ;if there are inputs aligned to the left
                           (if (and (equalp grace 0) (equalp (length ll) 1)) 
                               (setq out (append out (list (* (input-sign inst (car ll)) len))))  ;if there is only 1 input aligned to the left
                             (let ((ll0 (remove-silences ll inst)))
                               (if (null ll0) ;if there are only rests aligned to the left (at least 1)
                                   (if (> grace 0) ;if there were notes aligned to the right in the previous subdivision
                                       (if (equalp grace 1) ;if there is only one note
                                           (setq out (append out (list len)))
                                         (setq out (append out (make-list (1- grace) :initial-element 0) (list len))))
                                     ;if there are only rests in the subdivision and there was no note aligned to the right before
                                     (setq out (append out (list (- len)))))
                                 ;if there are notes aligned to the left
                                 (if (and (equalp (length ll0) 1) (equalp grace 0)) ;if there is exactly one note aligned to the left
                                     (setq out (append out (list len)))
                                   ;if there is more than 1 note aligned to the left or at least 1 note aligned to the right before and 1 to the left in this subdivision
                                   (setq out (append out (make-list (1- (+ grace (length ll0))) :initial-element 0) (list len))))))))
                       ;processing rr
                       (if (null rr)
                           (if (not (null ll))
                               (setq prev (input-sign inst (car (last ll)))) ;if there are no notes aligned to the right, but there are some to the left
                             ;if there are no notes in the subdivision, we do nothing
                             )
                         (setq prev (input-sign inst (car (last rr)))))
                       (setq grace (length (remove-silences rr inst)))))
                 ;if run is a list, ie, if we subdivise further
                   (loop for i from 1 to r
                         for krun in run
                         do (to-ratio (path-addi p r i) krun)))))))
    (let* ((p (path-empty inst))
          (len-tot (path-len p)))
      (to-ratio (path-empty inst) k)
      (mapcar #'(lambda (x) (/ x len-tot)) out)))))

(in-package :om)

;;OBSOLETE : TODO : adapt to the new best and cand structures
;;Not used in the newer save functions : instead of being saved, the table is recomputed each time the patch is opened
(defmethod om::omng-save ((self rq::entry) &optional values?)
  `(let ((best ,(append (list `list) (mapcar #'(lambda (elt) `(list ',(first elt) ,(om::omng-save (second elt)) ',(third elt) ',(fourth elt))) (rq::entry-best self)))))
    (rq::make-entry :args ',(rq::entry-args self) :dist ,(om::omng-save (rq::entry-dist self)) :cand ,(om::omng-save (rq::entry-cand self)) :best best )))
