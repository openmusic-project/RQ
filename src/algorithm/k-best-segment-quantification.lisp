

(use-package :om :rq)
(in-package :rq)

;;; General functions, used whatever is the number of inputs and tables for the current segment.


(defmethod k-best-segment-get-gracenotes (ktable inst k &optional (flag nil) (path (path-empty inst)))
  "Returns the number of grace notes for the segment given by path (ie, number of note onsets aligned to the right of the segment)."
  (length (remove-silences (nth-value 3 (rq-best ktable inst path k flag)) inst)))

;;; New rq-eval and rq-best for the first level of subdivision
;; (subdivision in beats, the complexity measure is not the same)

(defun rq-best-top (ktable inst k)
  "Special rq-best function for top-level subdivision (subdivision in beats, the weight functions are different)"
  (let ((p (path-empty inst)))
  (labels ((process-next (e run w)
             (multiple-value-bind (ll rr) (align-run ktable inst p run)
               (if (or (and (not (null run)) (is-redundant ktable inst p ll rr)) (not (check-flags run (length run))))                 
                   () ;if the current run gives a redundant alignment (ie equivalent to not subdividing further), we only add the nexts to the cand list (we do not put the run in the best list)
                 (setf (best-best0 (entry-best e)) (k-list-add run w ll rr (best-best0 (entry-best e))))) ;if not, we add it to the list and add the nexts to the cand list
               (map nil #'(lambda (x) (setf (cand-cand0 (entry-cand e)) (queue-insert (cand-cand0 (entry-cand e)) (make-undef) x))) (nexts run))))
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
           (bests (best-best0 (entry-best e)))
           (candidates (cand-cand0 (entry-cand e))))
      (if (<= k (length bests)) ;if the k-th best has already been computed
          (let ((result (k-list-ith (1- k) bests)))
            (values (first result) (second result) (third result) (fourth result)))
        (if (queue-empty candidates) ;if there is no candidate left (ie all the possible k-bests have already been computed)
            (values nil (make-infinity))
          (multiple-value-bind (w run cands) (queue-extract candidates) ;otherwise, compute the next best
            (if (weight-infinityp w) ;if the only candidates left are impossible
                (values nil (make-infinity))
              (progn
                (setf (cand-cand0 (entry-cand e)) cands)
                (if (weight-defp w)
                    (progn
                      (process-next e run w)
                      (rq-best-top ktable inst k)) ;repeat until k-best is constructed
                  (let ((w0 (rq-eval-top ktable inst run))) 
                    (if (weight-defp w0) ;if the weight of the candidate was undefined and has been successfully computed
                        (progn
                          (setf (cand-cand0 (entry-cand e)) (queue-reinsert cands w0 run))
                          (rq-best-top ktable inst k))
                      (rq-best-top ktable inst k))))))))))))) ;otherwise, the candidate cannot be weighted, discard it and retry with next

(defun rq-eval-top (ktable inst run)
  "Special rq-eval function for top-level subdivision (subdivision in beats, the weight functions are different)"
  (let ((p (path-empty inst)))
    (multiple-value-bind (e found) (gethash p ktable)
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
                       (append (list weight) (best-weights (rest pkl)))))))
               (weight-addall-top (wl)
                 (cond ((find-if #'weight-undefp wl) (make-undef))
                       ((find-if #'weight-infinityp wl) (make-infinity))
                       (t (multiple-value-bind (dl cl) (weight-split wl)       
                            (weight-make (distance-addall dl) (complexity-addall-top cl) (input-precision inst) (input-gracepen inst)))))))
        (assert found)
        (let ((r (length run)))
          (if (equalp 0 r) ;if we do not subdivise futher, ie we have a leaf, the distance is already computed and stored in e (function register-state)
              (let ((d (entry-dist e))
                    (c (complexity-make1 ktable inst p e nil))
                    (penalty (when (atom (car (input-sch inst))) (car (input-sch inst))))) ;when tempi are not merged, we add a penalty to compensate the one added in complexity-addall-top
                (weight-make d c (input-precision inst) (input-gracepen inst) :penalty penalty))
            (let* ((pl (path-children p r))
                   (pkl (mapcar #'cons pl run))
                   (wl (best-weights pkl)))
              (weight-addall-top wl))))))))


