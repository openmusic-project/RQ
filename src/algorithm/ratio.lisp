
(use-package :om :rq)

(in-package :rq)

;;; Functions to remove redundant solutions.
;;  Redundant runs are deleted in each entry of the table
;;  A run is considered redundant when it gives the same alignments to the left and right boundaries as the cas when the segment is not divided.
;;  Note that even when a run is deleted, the next candidates are still added to the cand list.


(defun align-run (ktable inst p run)
  "Returns the indexes of the notes aligned to the left and right bounds of the segment given by path, for the run given."
  (if (null run)
      (align ktable p)
    (let* ((r (length run))
           (path1 (path-addi p r 1))
           (pathr (path-addi p r r)))
      (multiple-value-bind (run1 weight1 ll1 rr1) (rq-best ktable inst path1 (caar run) (cdar run))
        (multiple-value-bind (run1 weight1 ll2 rr2) (rq-best ktable inst pathr (caar (last run)) (cdar (last run)))
          (values ll1 rr2))))))


(defun is-redundant (ktable inst p ll rr)
  "True iff the alignments ll and rr are the same as when the segment given by p is not divided any further." 
  (multiple-value-bind (ll1 rr1) (align ktable p)     
      (and (equalp ll1 ll) (equalp rr1 rr))))
