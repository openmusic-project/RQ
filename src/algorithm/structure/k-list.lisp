;;;; Storage of k elements sorted by weight
;;; k list is a list whose elements are lists of the form :
;;;   (run  weight  ll  rr), with weights sorted in reverse order
;;;   (the first element of the list is the greatest)

(use-package :om :rq)

(in-package :rq)

(defun k-list-empty () nil)

(defun k-list-length (list)
  (length list))

(defun k-list-add (x w ll rr list)
  (if (null list)
      (list (list x w ll rr))
    (progn
      (assert (<= (weight-compare (second (first list)) w) 0))
      (append (list (list x w ll rr)) list))))

(defun k-list-ith (i list)
  "returns the ith greatest element. (k-list-ith 0 list) returns the smallest element in list"
  (nth  (1- (- (length list) i)) list))
