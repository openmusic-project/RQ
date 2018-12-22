;;; Distance run-input (pointwise distance) and computation of alignment to paths
;; Adrien Ycart

(use-package :om :rq)

(in-package :rq)

; Distance is represented by a vector of size N (with N the number of inputs). Given a certain path, the nth element of the vector is the distance between the nth input and the nearest bound of the segment represented by path. 
; Mask is a vector of size N. mask[i] = 0 if ith input is outside of the interval
;                                      -1 if ith input is aligned to the left of the interval
;                                       1 if ith input is aligned to the right of the interval
(defstruct (distance 
            (:constructor make-distance ( vec mask res val &optional (normalized nil)))
            (:constructor empty-distance (&optional  (vec #()) (mask #()) (res 0) (val 0.0) (normalized nil))))
  (vec nil :type array)            
  (mask nil :type array)
  (res 0 :type integer)           ; resolution as in input
  (val 0 :type float)             ; value of the distance (norm of vector vec)
  (normalized nil :type boolean)) ; true iff val is the computed norm of the current distance vector vec (in that cas we don't have to compute it again)

(defun aset (array index value)
  "to set a value in an array"
  (setf (aref array index) value))

(defun distance-dim (dist)
  (length (distance-vec dist)))

(defun distance-inhabitedp (dist)
  "true iff there are input points in the segment corresponding to the path considered"
  (notevery #'(lambda (x) (equalp 0 x)) (distance-mask dist)))

(defun distance-align (dist)
  " return a pair of lists (ll, lr). ll (respectively lr) is the list of indexes of points in the input instance aligned to the left (right) bound of the interval defined by p "
  (labels ((align1 (ll lr i list)
                (let ((val (car list))
                      (l (cdr list)))
                  (cond ((null list) (values (reverse ll) (reverse lr)))
                        ((equalp val -1) (align1 (cons i ll) lr (1+ i) l))
                        ((equalp val  1) (align1 ll (cons i lr) (1+ i) l))
                        ((equalp val  0) (align1 ll lr (1+ i) l))
                        (t (error "error in distance-align"))))))
  (align1 nil nil 1 (coerce (distance-mask dist) 'list))))

(defun distance-notes-align (dist inst)
  (multiple-value-bind (ll rr) (distance-align dist)
    (values (remove-silences ll inst) (remove-silences rr inst))))
    
(defun distance-make (dim res)
  (make-distance (make-array dim :initial-element 0.0) (make-array dim :initial-element 0) res 0.0 nil))

(defun distance-make0 (inst)
  (distance-make (input-dim inst) (input-res inst)))

(defun distance-make1 (inst p)
  "returns a distance stucture holding the pointwise distance for the alignment of the subarray of inputs between the bounds of the interval given by p to the nearest bound of the interval."
  (let ((pleft (path-left p))
        (pright (path-right p))
        (linput (coerce (input-input inst) 'list))
        (dist (distance-make0 inst)))
    (labels ((make11 (pl pr d i li)
                    (if (null li)
                      d
                      (let ((a (car li))
                            (l (cdr li)))
                        (if (< a pl)
                            (make11 pl pr d (1+ i) l) ;point i out of interval of p (i not treated, we treat the next one)
                          (if (< a pr) ;point i inside interval
                              (let ((dtol (- a pl)) ;distance to left
                                    (dtor (- pr a)));distance to right
                                (if (< dtol dtor)
                                    (progn ;align to left
                                      (aset (distance-vec d) i dtol)
                                      (aset (distance-mask d) i -1)
                                      (make11 pl pr d (1+ i) l))
                                  (progn ;align to right
                                    (aset (distance-vec d) i dtor)
                                    (aset (distance-mask d) i 1)
                                    (make11 pl pr d (1+ i) l))
                                )) 
                            d ;point i and following outside of p
                            ))))))
    (make11 pleft pright dist 0 linput))))

(defun distance-add (dist1 dist2)
  "adds two distances. The modification is done in-place, the result is stored in dist1. The norm is not computed"
  (let ((dim (distance-dim dist1))
        (d1 (distance-vec dist1))
        (d2 (distance-vec dist2))
        (m1 (distance-mask dist1))
        (m2 (distance-mask dist2)))
    (progn
      (loop for i from 0 to (- dim 1)
            do 
            (aset d1 i (+ (aref d1 i) (aref d2 i)))
            (aset m1 i (+ (aref m1 i) (aref m2 i))))
      (setf (distance-normalized dist1) nil) ;distance vector is modified, we have to recompute the norm
      dist1)))

(defun distance-addall (dlist)
  "adds all the vectors in dlist. The norm is not computed"
  (let* ((d1 (car dlist))
         (d0 (distance-make (distance-dim d1) (distance-res d1))))
    (reduce #'distance-add dlist :initial-value d0)))

(defun minkowski (vec n)
  (expt (reduce #'(lambda (y x) (+ y (expt x n))) vec :initial-value 0.0) (/ 1 n)))

(defun sum (vec)
  (minkowski vec 1))

(defun euclidian (vec)
  (minkowski vec 2))

(defun cubic (vec)
  (minkowski vec 3))

(defun distance-norm (dist)
  "if not already computed, computes the norm of the distance vector, and in both cases, returns it"
  (if (distance-normalized dist)
      (distance-val dist)
    (let ((res (float (distance-res dist))))
      (progn
        (setf (distance-val dist) (/ (sum (distance-vec dist)) res))
        (setf (distance-normalized dist) t)
        (distance-val dist)))))

(defun distance-to-float (dist)
  (distance-norm dist))

(defun distance-normalized-by-length (dist path)
  (let ((length (float (path-len path))))
    (/ (sum (distance-vec dist)) length)))

(in-package :om)

(defmethod om::omng-save ((self rq::distance) &optional values?)
  `(rq::make-distance ,(rq::distance-vec self) ,(rq::distance-mask self) ,(rq::distance-res self) ,(rq::distance-val self) ,(rq::distance-normalized self)))
