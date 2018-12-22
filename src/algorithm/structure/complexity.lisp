;;; Complexity
;; Author : Adrien Ycart

(use-package :om :rq)

(in-package :rq)

(defstruct (complexity
            (:print-function print-complexity))
  (arity)       ; Holds the number of each arities in the subtree (the ith element is the number of subtrees of arity i+1 (i from 0 to 18)). The arities are ordered by complexity as follows : 1 2 4 3 6 5 8 7 10 12 16 9 14 11 13 15 17...
  (depth)       ; Complexity value for the depth : propotional to the depth of the sub-tree
  (gracenotes)  ; Complexity value for the grace notes. 
  (val)) 

; order of arities (taken from omquantify) :
;1 2 4 3 6 5 8 7 10 12 16 9 14 11 13 15 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50

(defun print-complexity (c &optional (stream t) (depth 0))  
  (format stream "[a=~d; d=~d; g=~d; val=~d]"
          (complexity-arity-reduce (complexity-arity c))
          (complexity-depth c)
          (complexity-gracenotes c)
          (complexity-val c)))

(defun complexity-make (c) c)

(defun complexity-make1 (ktable inst p e flag)
  (make-complexity :val nil :arity (make-array 19 :initial-element 0) :depth 1 :gracenotes (complexity-get-gracenotes ktable inst p e flag)))

(defun complexity-get-gracenotes (ktable inst p e flag)
  ""
  (let ((d (entry-dist e))
        (grace (if flag 1 0)))
    (multiple-value-bind (ll0 rr0) (notes-align ktable inst p)
			 (if (> (length ll0) 1)
			     (setq grace (+ grace (1- (length ll0)))))
			 (if (> (length rr0) 1)
			     (setq grace (+ grace (1- (length rr0)))))
			 grace)))

(defun complexity-value (c &optional (gracepen 2))
  ""
  (if (complexity-val c)
      (complexity-val c)
    (let ((ca (complexity-arity c))
	  (cd (complexity-depth c))
	  (cg (complexity-gracenotes c)))
      (setf (complexity-val c) (+ (* 1 (complexity-arity-reduce ca)) (* 0 cd) (* gracepen cg))))))

(defun complexity-min (l)
  (reduce #'min l))

(defun complexity-addall (cl)
  (+ (1- (length cl)) (* 2.0 (reduce #'+ cl)))); 2*(sum of elements) + (arity -1)

(defun complexity-addall1 (cl)
  (let ((al (mapcar #'complexity-arity cl))
        (dl (mapcar #'complexity-depth cl))
        (gl (mapcar #'complexity-gracenotes cl)))
    (make-complexity :arity (complexity-arity-addall al) :depth (complexity-depth-addall dl) :gracenotes (reduce #'+ gl) :val nil)))

(defun complexity-addall-top (cl)
  (let ((cvals (mapcar #'complexity-value cl)))
					; (print "test")
    (make-complexity :val (+ (reduce #'+ cvals) (length cl)) ;we add  (length cl) in order to penalize the cases where the tempo is fast and results in an all-quarternotes transcription
                     :arity (make-array 19 :initial-element 0)
                     :depth 0
                     :gracenotes 0)))

;when arity is just an integer and not a vector
(defun complexity-arity-addall1 (al)
  (let ((order '(1 2 4 3 6 5 8 7 10 12 16 9 14 11 13 15 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50)))
    
    (flet ((get-weight (a)
		       (position a order)))
	  (* (get-weight (length al)) (reduce #'+ al)))))

(defun complexity-arity-addall (al)
  "adds the arities in the entry list and returns the arity correponding to the sum"
  (let ((out (make-array 19 :initial-element 0)))
    (labels ((arity-add (alist buffer)
			(let ((current (car alist)))
			  (if (null current)
			      buffer
			    (progn 
			      (loop for i from 0 to 18 do (setf (aref buffer i) (+ (aref buffer i) (aref current i))))
			      (arity-add (cdr alist) buffer))))))
	    (arity-add al out))
    (setf (aref out (1- (length al))) (1+ (aref out (1- (length al)))))
    out))

(defun complexity-arity-reduce (a)
  "returns a score for the arity given as entry"
  (reduce #'+ (mapcar #'(lambda (x y) (* x y)) (coerce a 'list) (list 0 1 3 2 5 4 7 6 11 8 13 9 14 12 15 10 16 17 18))))


(defun complexity-depth-addall (dl)
  (1+ (reduce #'max dl)))

(in-package :om)

(defmethod om::omng-save ((self rq::complexity) &optional values?) 
  (labels ((serialize-arities (arities)
			      (loop for i from 0 to 18 unless (zerop (aref arities i))
				    collect (list i (aref arities i)))))
	  `(load-complexity ',(serialize-arities (rq::complexity-arity self)) ,(rq::complexity-depth self) ,(rq::complexity-gracenotes self))))

(defmethod load-complexity (aritylist depth gracenotes)
  (let ((arities (make-array 19 :initial-element 0)))
    (labels ((make-arities (list)
			   (when list
			     (setf (aref arities (caar list)) (cadar list))
			     (make-arities (cdr list)))))
	    (make-arities aritylist)
	    (rq::make-complexity :arity arities :depth depth :gracenotes gracenotes))))

