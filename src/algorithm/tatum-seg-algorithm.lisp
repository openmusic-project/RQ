(in-package :om)


;;;============================
;;; ALGORITHM FOR SEGMENTATION IN CONSTANT-TATUM REGIONS
;;;
;;; Algorithm based on A. C. Yang, E. Chew, A. Volk : "A Dynamic Programming Approach to Adaptative Tatum Assignment for Rhythm Transcription"
;;; Note that the pseudo-code given in the above paper is false (indexes do not match), but the principle is correct.
;;; Moreover, a customizable "new segment penalty" has been added in order to prevent the algorithm from placing a new segmentation mark at each note.


(defpackage "CTSeg"
    (:nicknames :cts)
    (:use "COMMON-LISP" "CL-USER" "OM-API"  "LISPWORKS" "OM-LISP")
    (:import-from "CL-USER")   
    (:export #:segmentation)
    )

(in-package :cts)

(defun e (ioil p)
  "Computes the remainder squared error for the given IOI list and the given tatum p"

    (reduce #'(lambda (x y) (+ (* y y) x)) (mapcar #'(lambda (x) (- (/ x (float p)) (round (/ x (float p))))) ioil) :initial-value 0)
  
)


(defun err (ioilist i &optional (range '(40 100)))
  "Returns the value of the minimum RSE for the sublist of indexes between i and N and the corresponding value of tatum p" 
  (let ((ioil (subseq ioilist i))
        (errors)
        (minimum))

    (setq errors (loop for p from (car range) to (cadr range) collect
                       (e ioil p)))
    (setq minimum (reduce #'min errors))
    (values minimum (+ (position  minimum errors :from-end t) 40))
))  


(defun opt (ioilist optlist k)
  "Returns the weight and the index of the previous segmentation mark that gives the best weight"  
  (let* ((errorlist (make-list k))
         (Tlist (make-list  k))
         (minimum)
         (index))
    
    (loop for i from 0 to (1- k) do
          (multiple-value-bind (error tatum) (err ioilist  i)
            (setf (nth i errorlist) (+ (nth i optlist) error)
                  (nth i Tlist)     tatum)
                  ))
    (setq minimum (reduce #'min errorlist))
    (setq index (position minimum errorlist))
    (values minimum index (nth index Tlist)) 

))

(defun backtrace (Slist Tlist &optional Sbuff Tbuff)
  (if Slist
      (let ((S (car (last Slist))))
        (backtrace (subseq Slist 0 S) (subseq Tlist 0 S) (append (list S) Sbuff) (append (last Tlist) Tbuff)) 
        )

    (values Sbuff Tbuff)))
  

(defun segmentation (onsets &optional (penalty 5))
  "Given a list of onsets (in ms), returns a list of segmentation marks (in ms) segmenting the input in constant-tatum regions."
  (let* ((ioilist  (mapcar #'- (subseq onsets 1) onsets))
         (N (length onsets))
         (optlist (make-list  N :initial-element 0 ))
         (Slist (make-list (1- N) :initial-element 0))
         (Tlist (make-list (1- N) :initial-element 0))
         (pen (* 0.01 penalty))
         )
    
    (loop for k from 1 to (1- N) do
          (multiple-value-bind (OPT S Tatum) (opt (subseq ioilist 0 k) optlist  k)
            (setf (nth  k optlist)   (+ OPT pen))
            (setf (nth (1- k) Slist)  S)
            (setf (nth (1- k) Tlist)   Tatum)

            ))

    (multiple-value-bind (Ss Ts) (backtrace  Slist Tlist) ;we discard the tatum list, as we do not use it further
      (loop for S in Ss collect
            (nth S onsets))) 

))


