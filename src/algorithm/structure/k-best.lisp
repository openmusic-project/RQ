(use-package :om :rq)
(in-package :rq)


;;; The k-best structure is the structure holding all the elements relative to the k-best algorithm 

(defclass! k-best ()
           ((ktables :accessor ktables :initform nil :initarg :ktables) ;list of hashtables (might have only 1 element)
            (inputs :accessor inputs :initform nil :initarg :inputs) ;list of input structures (might only have 1 element). The ktables and inputs lists always have the same number of elements.
            (slur :accessor slur :initform nil :initarg :slur) ; nil or a number, indicates if each measure has to be slurred with the previous
            (gracenotes :accessor gracenotes :initform nil :initarg :gracenotes) ;list of size n-solution, indicates the number of grace notes to be added in the next measure for each solution
            (n-solution :accessor n-solution :initform nil :initarg :n-solution) ;number of solutions per automaton
            (tempomerge :accessor tempomerge :initform nil :initarg :tempomerge) ;true iff there is 1 input for all tempi
            )) 

(defun k-best-to-poly (k-best &optional (chords (list 6000)))
  "Generates poly objects"
  (let* ((ktables (ktables k-best))
         (inputs (inputs k-best))
         (N (length ktables))
         (slur (slur k-best))
         (n-solution (n-solution k-best))
         (gracenotes (append (loop for n from 1 to N collect (make-list list-n-solution :initial-element 0)) (gracenotes k-best)))) 
    (k-best-create-poly ktables inputs n-solution slur gracenotes chords)))

(defun k-best-create-poly (ktables inputs n-solution slur gracenotes tempomerge &optional (chords '(6000)))
  "Creates a poly containing all the computed solutions"

  ;Always output a couple of lists (one for the sigs, one for the tempi) of size (length inputs), composed of lists of size n-solution
  (labels ((get-signatures-and-tempi (ktables insts n-solution)
				     (let ((segment-dur (aref (input-rinput (car inputs)) (1- (length (input-rinput (car inputs)))))))
					;when there is only 1 input (tempi merged)
				       (if tempomerge
					   (let* ((ktable (car ktables))
						  (inst (car insts))
						  (sigs  (loop for k from 1 to n-solution collect (get-signature ktable inst tempomerge k))))
					     (values 
					      (list sigs)
					      (list (mapcar #'(lambda (sig) (round (* (/ 60000 segment-dur) (car sig)))) sigs))))
					;when there are multiple inputs (1 automaton per tempo)
					 (let ((sigs (loop for inst in insts
                                                           for ktable in ktables
							   collect
							   (loop for k from 1 to n-solution collect (get-signature ktable inst tempomerge)))))
					   (values
					    sigs
					    (loop for list in sigs
						  collect (mapcar #'(lambda (sig) (round (* (/ 60000 segment-dur) (car sig)))) list))))))))
	  (let* ((trees nil)
		 (N (length ktables)))
	    (multiple-value-bind (signatures tempi) (get-signatures-and-tempi ktables inputs n-solution)
				 (setq trees (loop for n from 0 to (1- N) collect 
						   (loop for k from 1 to n-solution collect (rq-remove-top-level (rq-to-omtree (nth n ktables) (nth n inputs) k nil slur (nth (1- k) (nth n gracenotes)))))))
				 (setq trees (loop for n from 0 to (1- N) collect 
						   (mapcar #'(lambda (sig tree) (list (list sig tree))) (nth n signatures)  (nth n trees))))
				 (setq trees  (loop for n from 0 to (1- N) collect
						    (mapcar #'(lambda (x) (append (list '?) (list x)))  (nth n trees))))
				 (make-instance 'poly 
						:voices (loop for n from 0 to (1- N) 
							      append (mapcar #'(lambda (rtree tempo) (make-instance 'voice
														    :tree (reduce-rt rtree)
														    :chords  chords
														    :tempo tempo))
									     (nth n trees) (nth n tempi))))))))


(defmethod get-signature (table input tempomerge &optional (k 1))
  "Gets the signature of a particular solution : (number-of-pulses 4)"
  (if tempomerge 
      (list (max (length (rq-best table input (path-empty input) k nil)) 1) 4)
    (list (or (car (input-sch input)) 1) 4))
)

(defmethod get-tempi-weigths-ks (k-best)
  "Returns the list of tempi with no doubles, with corresponding weights and indexes"
  (let  ((tempi nil)
         (weights nil)
         (ks nil))
    (if (tempomerge k-best)
        (let* ((ktable (car (ktables k-best)))
               (input (car (inputs k-best)))
               (input-dur (input-dur input)))
          (loop for i from 1 to (n-solution k-best)
                do 
                (let* ((n-beats (max (length (rq-best ktable input (path-empty input) i nil)) 1))
                       (tempo (round (* (/ 60000 input-dur) n-beats))))
                  (unless (find tempo tempi)
                    (setq tempi   (append tempi (list tempo))
                          weights (append weights (list (rq::weight-val (nth-value 1 (rq::rq-best-top ktable input i)))))
                          ks      (append ks (list (1- i))))))))
      (let ((ktables (ktables k-best))
            (inputs (inputs k-best))
            (n-solution (n-solution k-best)))
                (loop for ktable in ktables
                      for input in inputs
                      for i = 0 then (+ i n-solution)
                      do
                      (setq tempi (append tempi (list (round (* (/ 60000 (rq::input-dur input)) (car (rq::input-sch input)))))))
                      (setq weights (append weights (list (rq::weight-val (nth-value 1 (rq::rq-best-top ktable input 1))))))
                      (setq ks (append ks (list i))))))
    (values tempi weights ks)
))



(defmethod get-distances (k-bests)
  "Returns a list containing all the distances for all the computed solutions"
  (loop for k-best in k-bests collect
        (let* ((inputs (inputs k-best))
               (ktables (ktables k-best))
               (n-solution (n-solution k-best)))
          (loop for ktable in ktables
                for input in inputs append
                (loop for k from 1 to n-solution collect 
                      (/ (round (* 1000  (weight-distance (nth-value 1 (rq-best-top ktable input k))))) 10.0)))))) ;distance in percents, rounded to 1 decimal


(in-package :om)

;; Not used anymore : the k-bests are recomputed each time a patch is opened
(defmethod om::omng-save ((self rq::k-best) &optional values?)
  `(let ((ktables ,(append (list `list) (mapcar #'(lambda (elt) (om::omng-save elt)) (rq::ktables self))))
         (inputs  ,(append (list `list) (mapcar #'(lambda (elt) (om::omng-save elt)) (rq::inputs self))))
         (slur   ,(rq::slur self))
         (gracenotes  ',(rq::gracenotes self))
         (n-solution ,(rq::n-solution self)))
     (make-instance 'rq::k-best :ktables ktables :inputs inputs :slur slur :gracenotes gracenotes :n-solution n-solution)))
