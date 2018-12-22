;;;; Problem input
;;; v.1 input = sequence of dates (float) in [0..t] starting with 0, ending with t
;;; 
;;; input represents a sequence of onsets. 
;;; Each onset corresponds to a note or a silence, depending on the value of mask (1: note, -1: silence)
;;; The duration of this note is given by the difference between two successive onsets.
;;; The first element of input is always 0.0, the last is the end of the last note (= the beginning of the first of the next segment)
;;; The last value of the mask doesn't matter : the last element of input is not a note but the marker of the end of the segment 
;;;
;;; input is given by a sequence of dates
;;; = float array of length > 1
;;; 
;;; 
;;; v.1 normalized input = sequence of dates (float) in [0..RES] starting with 0, ending with RES
;;; RES is resolution value
;;; 0 is the first sample of the current segment
;;; RES-1 is the last sample of the current segment
;;; RES is the first sample of the next segment
;;; 

(use-package :om :rq)

(in-package :rq)


(defstruct (input)
  (sch)                      ;subdivision schema (see subdivision.lisp)
  (rinput nil :type array)   ;input as described above, not normalized
  (mask nil :type array)     ;vector of same size as input : 1 means note, -1 means silence
  (res )                     ;resolution of the schema
  (input nil :type array)    ;input normalized
  
  (precision)                ;precision parameter to determine the relative importance of the complexity and distance measures
  (gracepen))                ;penalty weight for gracenotes

(defun input-emptyp (in)
  (equalp (length (input-input in)) 0))


(defun input-dim (in)
  "dimension of the input"
  (length (input-input in)))

(defun input-dur (in)
  "duration of the input"
  (aref (input-rinput in) (1- (length (input-rinput in)))))


(defun input-sample (ri mask res)
  "normalises and samples the array to float values in [0..res]"
  (let* ((nb (length ri))
        (fres (float res))
        (max (if (equalp nb 0)
                 0.0
               (aref ri (1- nb)))))
     ;if ri doesn't begin with 0.0, we add it, and assume it begins with a silence
    (assert (equalp (length ri) (length mask)))
    (cond ((equalp (length ri) 0) (values (map 'vector #'(lambda (x) (* (/ x max) fres)) ri) mask))
          ((equalp (aref ri 0) 0.0) (values (map 'vector #'(lambda (x) (* (/ x max) fres)) ri) mask))
          (t (let ((ri0 (coerce (append (list 0.0) (coerce ri 'list)) 'vector))
                   (mask0 (coerce (append (list -1) (coerce mask 'list)) 'vector)))
               (values (map 'vector #'(lambda (x) (* (/ x max) fres)) ri0) mask0))))))

(defun input-make (s ri mask precision gracepen)
  (let ((res (schema-resolution s)))
    (multiple-value-bind (rs mask) (input-sample ri mask res)
      (if (equalp (length ri) 0)
          (make-input :sch s :rinput ri :mask mask :res res :input rs :precision precision :gracepen gracepen)
      ;we delete the last element (boundary limit, is the first element of the next segment)
        (make-input :sch s :rinput ri :mask (subseq mask 0 (1- (length mask))) :res res :input (subseq rs 0 (1- (length rs))) :precision precision :gracepen gracepen)))))
 

(defun input-sign (inst index)
  "returns 1 if the input point at position index is a note, -1 if it is a rest"
  (aref (input-mask inst) (1- index)))

(defun input-silencep (inst index)
  (equalp (input-sign inst index) -1))

(defun input-print (inst)
  (let ((lr (coerce (input-rinput inst) 'list))
        (li (coerce (input-input inst) 'list)))
    (progn
      (format t "__________________________________~%Problem Instance~%__________________________________~%")
      (format t "resolution : ~D ~%dimension : ~D ~%rinput : ~S ~%input : ~S ~%" (input-res inst) (input-dim inst) (input-rinput inst) (input-input inst)))))

(in-package :om)

(defmethod om::omng-save ((self rq::input) &optional values?)
  `(rq::make-input :sch ',(rq::input-sch self) :rinput ,(rq::input-rinput self) :mask ,(rq::input-mask self) :res ,(rq::input-res self) :input ,(rq::input-input self)))
