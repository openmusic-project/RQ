

;;;============================
;;; ALGORITHM FOR TEMPO SMOOTHING
;;;
;;; Dynamic-programming segmentation algorithm
;;; Inspired by the segmentation algorithm described in : A. C. Yang, E. Chew, A. Volk : "A Dynamic Programming Approach to Adaptative Tatum Assignment for Rhythm Transcription" (see also tatum-seg-algorithm.lisp)
;;; Also includes a new-tempo penalty (tempo jump penalty)
;;; The "ERR" function is replaced by a Viterbi-like best-path algorithm
;;; The "ERR" function finds the series of tempo that minimizes the variations between successive tempi, and the weights of the corresponding transcriptions.
;;; The balance between tempo variations and weights is made by the alpha parameter



(defpackage "TempSmooth"
    (:nicknames :ts)
    (:use "COMMON-LISP" "CL-USER" "OM-API"  "LISPWORKS" "OM-LISP")
    (:import-from "CL-USER")   
    (:export #:tempo-smooth)
    )

(in-package :ts)


;------- Functions to calculate the best sequence of tempi in a sublist ------;

(defun dist (tempo1 tempo2 &optional (type :rse))
  "Computes the distance between two tempi"
  (let ((diff (- tempo2 tempo1)))
    (case type 
      (:rse (* diff diff))
      (:abs (abs diff))
      (:alg diff)
      )))

(defun dists (tempo ltempo &optional (type :rse))
  "Returns a list holding the distances between TEMPO and each tempo in LTEMPO"
  (loop for tempo2 in ltempo
        collect (dist tempo tempo2 type)))

(defun min-argmin (list &optional (min most-positive-fixnum) (min-index 0) (cur-index 0))
  "Returns the min (in absolute value) and the argmin of a list"
  (if list
      (if (< (abs (car list)) (abs min)) 
          (min-argmin (rest list) (car list) cur-index (1+ cur-index))
        (min-argmin (rest list) min min-index (1+ cur-index)))
    (values min min-index))
)


(defun viterbi-backtrace (T1 &optional indexes tempi)
  (let* ((new (nth (car indexes) (car T1)))
         (new-index (second new))
         (new-tempo (third new)))

    (if new-index
        (viterbi-backtrace (rest T1) (append (list new-index) indexes) (append (list new-tempo) tempi))
      (values indexes (append (list new-tempo) tempi))))
)

(defun tempo-viterbi (ltempi lweights alpha &optional (type :rse))
  "Computes the sequence of tempi that minimises the weight (compromise between the weight of the solution and the tempi difference among segments)"
  (let ((T1 (make-list (length ltempi))))
        ;T1 holds the cumulated values. It has the same length as ltempi. 
        ;Its i-th element is a list of same length as (nth i ltempi)
        ;Each element of this list is a triplet (min argmin tempo)

    ;initialization
    (setf (first T1) (loop for weight in (first lweights)
                           for tempo in (first ltempi)
                           collect (list (* (- 1 alpha) weight) nil tempo)))

    ;iteration
    (loop for tempi in (rest ltempi)
          for weights in (rest lweights)
          for i = 1 then (1+ i)
          do
          (setf (nth i T1)
                (let* ((prev (nth (1- i) T1))
                       (prev-vals (mapcar #'first prev))
                       (prev-tempi (mapcar #'third prev)))
                  (loop for tempo in tempi
                        for weight in weights
                        collect
                        (multiple-value-bind (val index) (min-argmin (om::om+ prev-vals 
                                                                              ;(om::om*  (dists tempo prev-tempi type)  weight)))
                                                                              (om::om+ (om::om* alpha (dists tempo prev-tempi type)) (* 2 (- 1 alpha) weight))))
                          (list val index tempo))))))

    ;backtrace
    (let* ((last-vals (mapcar #'first (car (last T1))))
           (min-val (reduce #'min (mapcar #'abs last-vals)))
           (last-index (position min-val last-vals)))
      (multiple-value-bind (indexes tempi)
          (viterbi-backtrace (reverse T1) (list last-index)) 
        (values min-val indexes tempi)
        ))
))


;-------- General algorithm --------;

(defun opt (ltempi lweights optlist k alpha type)
  "Returns the weight and the index of the previous segmentation mark that gives the best weight"
  (let* ((errorlist (make-list k))
         (Tlist (make-list  k))
         (minimum)
         (index))
    
    (loop for i from 0 to (1- k) do
          (multiple-value-bind (error index-list) (tempo-viterbi (subseq ltempi i) (subseq lweights i) alpha type)
            (setf (nth i errorlist) (+ (nth i optlist) error)
                  (nth i Tlist)     index-list)
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

(defun tempo-smooth (ltempi lweights &key (alpha 0.5) (type :abs) (penalty 5))
  "Returns a list of indexes that optimises the variations of tempo between each measure"
  (let* ((N (length ltempi))
         (optlist (make-list  (1+ N) :initial-element 0 ))
         (Slist (make-list N :initial-element 0))
         (Tlist (make-list N :initial-element 0)))
    (loop for k from 1 to N do
          (multiple-value-bind (OPT S index-list) (opt (subseq ltempi 0 k) (subseq lweights 0 k) optlist k alpha type)
            (setf (nth k optlist)   (+ OPT penalty))
            (setf (nth (1- k) Slist)     S)
            (setf (nth (1- k) Tlist)     index-list)
            ))
    (multiple-value-bind (Ss Ts) (backtrace  Slist Tlist) ;we discard the segmentation points list, as we do not use it further (we return the indexes of the solution to keep for each segment)
      (let ((ks (reduce #'append Ts)))
        (values ks
                (loop for k in ks
                      for tempi in ltempi
                      collect (nth k tempi))))

)))

