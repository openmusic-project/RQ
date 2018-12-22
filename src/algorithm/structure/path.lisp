;;; Paths
;;Author : Adrien Ycart

(use-package :om :rq)

(in-package :rq)

;a path p defines an interval of positions in [0..res] where res is the resolution associated to the given schema 

(defstruct (path
             (:constructor make-path (val begin len dur) )
             (:constructor empty-path (&optional (val nil) (begin 0) (len 0) (dur 0))))
           (val :list) ; p-val is a list of couples : each couple holds the arity of the node and the branch chosen : ((arity1 . choice1) (arity2 . choice2) ... ) 
           (begin :int) ; left bound of interval in [0..res]
           (len :int) ;length of interval
           (dur :int)) ;inverse of duration defined by p

(defun path-empty (inst)
  (make-path nil 0 (input-res inst) 1))

(defun path-emptyp (path)
  (equalp (path-val path) nil))

(defun path-left (path)
  "left bound of interval"
  (path-begin path))

(defun path-end (path)
  "right bound of interval"
  (+ (path-begin path) (path-len path)))

(defun path-right (path)
  "right bound of interval"
  (path-end path))

(defun path-depth (path)
  "depth of the path"
  (length (path-val path)))

(defun path-projection (val)
  ""
  (if (null val) 
      nil
    (let ((label (first val)))
      (list (cdr label) (car label) (path-projection (rest val))))))

(defun path-add (path element)
  "add an element to path (ie we subdivide further) and updates the other fields "
  (let* ((arity (car element))
         (direction (cdr element))
         (pval (path-val path))
         (pbegin (path-begin path))
         (plen (path-len path))
         (pdur (path-dur path))
         (len (floor plen arity))  ;duration of the new path : old duration/arity
         (val (append pval (list element))))
    (make-path val (+ pbegin (* (1- direction) len)) len (* pdur arity))))
    
(defun path-addi (path arity direction)
  "add an element to path (ie we subdivide further in arity and we choose the direction-th branch)"
  (path-add path (cons arity direction)))

(defun path-previous (path)
  ""
  (let* ((pval (path-val path))
         (arity (caar (last pval)))
         (direction (cdar (last pval)))
         (pbegin (path-begin path))
         (plen (path-len path))
         (pdur (path-dur path)))
    (assert (< 1 (print direction)))
    (make-path (append (reverse (cdr (reverse pval))) (list (cons arity (1- direction)))) (- pbegin plen) plen pdur)))

(defun path-father (path)
  ""
  (let* ((pval (path-val path))
         (arity (caar (last pval)))
         (direction (cdar (last pval)))
         (pbegin (path-begin path))
         (plen (path-len path))
         (pdur (path-dur path)))
    (assert pval)
    (make-path  (reverse (cdr (reverse pval))) (- pbegin (* (1- direction) plen)) (* plen arity) (floor pdur arity))))

(defun path-make (inst list &optional (path (path-empty inst)))
  "makes a path from a list of couples (arity . direction)"
  (if (null list)
      path
    (path-make inst (cdr list) (path-add path (car list)))))

(defun path-memberp (pos path)
  "true iff the position pos belongs to the segment given by path"
  (and (<= (path-begin path) pos) (< pos (path-end path))))

(defun path-compare (p1 p2)
  ""
  (let ((len1 (path-len p1))
        (len2 (path-len p2))
        (begin1 (path-begin p1))
        (begin2 (path-begin p2)))
    (if (> len1 len2)
        -1
      (if (< len1 len2)
          1
        (if (< begin1 begin2)
            -1
          (if (> begin1 begin2)
              1
            0))))))

(defun path-children (path arity &optional (i 1))
  "gives the list of the paths corresponding to the current path"
  (if (<= i arity)
     (append (list (path-addi path arity i)) (path-children path arity (1+ i)))
    nil))

(defun path-to-intlist (path)
  ""
  (reverse (path-projection (path-val path))))

(defun path-to-position (path)
  ""
  (reverse (mapcar #'cdr (path-val path))))

(defun print-pathlabel (label &optional (s t))
  ""
  (format s "[~D]~D" (car label) (cdr label)))

(defun print-path (path &optional (stream t) (depth nil))
  ""
  (labels ((print-val (val s)
                   (if (null val)
                     (format s "E")
                    (progn
                      (print-pathlabel (first val))
                      (when (> (length (rest val)) 0)
                          (progn
                            (format s ".")
                            (print-val (rest val) s)))))))
    (print-val (path-val path) stream)))

(in-package :om)

(defmethod om::omng-save ((self rq::path) &optional values?)
  `(rq::make-path ',(rq::path-val self) ,(rq::path-begin self) ,(rq::path-len self) ,(rq::path-dur self)))
