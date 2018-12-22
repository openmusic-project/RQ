;;;; A structure for holding weights

(use-package :om :rq)

(in-package :rq)

(defstruct (weight
            (:print-function print-weight))
  (dist) ; a distance object (see distance.lisp)
  (cpty) ; a complexity value (float) (see complexity.lisp)
  (val)) ; val can have the following values :  UNDEF = most-negative-fixnum , INFINITY = most-positive-fixnum , or a float (combination of distance and complexity) 


(defun print-weight (weight &optional (stream t) (depth 0))
  (cond 
   ((weight-undefp weight) (format stream "UNDEF"))
   ((weight-infinityp weight) (format stream "INFINITY"))
   (t (format stream "{W=~5$; D=~5$; C=~$}" (weight-val weight) (weight-distance weight) (weight-complexity weight) ))))


(defun weight-defp (weight)
  (null (or (equalp (weight-val weight) most-negative-fixnum)
            (equalp (weight-val weight) most-positive-fixnum))))

(defun weight-undefp (weight)
  (equalp (weight-val weight) most-negative-fixnum))

(defun weight-infinityp (weight)
  (equalp (weight-val weight) most-positive-fixnum))

(defun weight-distance (weight)
  (cond ((weight-undefp weight) 0.0)
        ((weight-infinityp weight) -1.0)
        (t (distance-to-float (weight-dist weight)))))


;distance normalized by the length of the subdivision, not the total length
(defun weight-distance-normalized-by-length (weight path)
  (distance-normalized-by-length (weight-dist weight) path) 
)

(defun weight-complexity (weight)
  (weight-cpty weight)
)


(defun weight-compare (w1 w2)
  "negative if w1 < w2, positive if w1 > w2, 0 if w1 = w2"

  (let ((x1 (weight-val w1))
        (x2 (weight-val w2)))
  (cond ((equalp x1 most-negative-fixnum)  (if (equalp x2 most-negative-fixnum)
                                               0
                                             -1))
        ((equalp x1 most-positive-fixnum) (if (equalp x2 most-positive-fixnum)
                                              0
                                            1))
        (t (cond ((equalp x2 most-negative-fixnum)  1)
                 ((equalp x2 most-positive-fixnum) -1)
                 ((< x1 x2) -1)
                 ((> x1 x2)  1)
                 (t 0)))


)))
 
(defun make-undef ()
  (make-weight :dist (empty-distance) :cpty nil :val most-negative-fixnum))

(defun make-infinity ()
  (make-weight :dist (empty-distance) :cpty nil :val most-positive-fixnum))

; Function not linear when cd is taken into account : not monotonous in this case !
(defun weight-combine (d c precision gracepen &optional (penalty nil) )
  "returns the weight value corresponding to the distance and complexity"
  (let ((dv (distance-norm d)))
    (if penalty
        (+  (* precision 10 dv) (* (- 1 precision) (complexity-value c gracepen)) penalty)
      (+  (* precision 10 dv) (* (- 1 precision) (complexity-value c gracepen))))))



(defun weight-make (d c precision gracepen &key (penalty nil))
  (make-weight :dist d :cpty c :val (weight-combine d c precision gracepen penalty)))


(defun weight-split (wlist)
  "given a list of weights, return the list of distances and the list of complexities"
  (if (null (first wlist)) 
      (values nil nil)
    (let* ((current (first wlist))
           (d (weight-dist current))
           (c (weight-cpty current)))
      (multiple-value-bind (dl cl) (weight-split (rest wlist))
        (values (append (list d) dl) (append (list c) cl))))))


(defun weight-addall (wl precision gracepen)
  "add all the weights in the entry list"
  (cond ((find-if #'weight-undefp wl) (make-undef))
        ((find-if #'weight-infinityp wl) (make-infinity))
        (t (multiple-value-bind (dl cl) (weight-split wl)
             (let ((d (distance-addall dl))
                   (c (complexity-addall1 cl)))
               (weight-make d c precision gracepen))))))

(defun weight-to-float (w)
  (cond ((weight-undefp w) 0.0)
        ((weight-infinityp w) -1.0)
        (t (weight-val w))))


(in-package :om)

(defmethod om::omng-save ((self rq::weight) &optional values?)
  (cond ((rq::weight-undefp self) '(rq::make-undef))
        ((rq::weight-infinityp self) '(rq::make-infinity))
        (t `(rq::make-weight  :dist ,(om::omng-save (rq::weight-dist self)) :cpty ,(om::omng-save (rq::weight-cpty self)) :val ,(rq::weight-val self)))))

