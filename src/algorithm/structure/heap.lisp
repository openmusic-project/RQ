;;; Priority Queue ;;;
;; Author : Adrien Ycart

(use-package :om :rq)

(in-package :rq)

;; A heap used to hold all the candidates

(defstruct (heap (:print-function print-queue))
           (elts) ; a list of all the elements that have already been created. Used to forbid duplicates.
           (queue))  ; the queue containing all the elements and their weights. It is a node structure.

(defstruct (node 
             (:constructor make-node ( w elt left right &optional (empty nil)))
             (:constructor empty-node (&optional  (w nil) (elt nil) (left nil) (right nil) (empty t)))
             (:print-function print-node ))
           (w)      ; weight of the candidate (see weight.lisp)
           (elt )   ; the run associated to the candidate (see run.lisp)
           (left )  ; the left tree of the current node (a node structure)
           (right ) ; the right tree of the current node (a node structure)
           (empty t :type boolean)) ; true iff the node is empty

(defun make-queue (w elt left right &optional (empty nil))
  (make-heap :elts nil :queue (make-node w elt left right empty)))

(defun empty-queue (&optional  (w nil) (elt nil) (left nil) (right nil) (empty t))
  (make-heap :elts nil :queue (empty-node w elt left right empty)))

(defun queue-empty (heap)
  (or (node-empty (heap-queue heap))
      (weight-infinityp (node-w (heap-queue heap)))))

(defun print-node (queue stream depth)
  (if (node-empty queue)
         (format stream "~A EMPTY" (make-sequence 'string depth :initial-element #\Space))
       (progn
         
         (format stream "~A([~D, ~A]~%" 
                 (make-sequence 'string depth :initial-element #\Space) ;pour indanter l'affichage
                 (node-w queue)
                 (node-elt queue)
                 )
         (print-node (node-left queue) stream (1+ depth))
         (format stream "~%")
         (print-node (node-right queue) stream (1+ depth))
         (format stream "~%~A)" (make-sequence 'string depth :initial-element #\Space)))))

(defun print-queue (heap stream depth)
  (print-node (heap-queue heap) stream depth))




(defun queue-insert (heap weight elt)
  "Insert an element in the heap if it has not already been created (NOT in-place)"
    (if (find elt (heap-elts heap) :test #'equalp)
        ;if the element has been created before, we do not add it, we return the heap not modified
        heap
      ;if the element is new, we put it in the heap and we add it to elts
      (make-heap :elts (append (heap-elts heap) (list elt)) :queue (queue-insert1 (heap-queue heap) weight elt))))

;; Insert an element in the heap (takes a node as entry)
(defun queue-insert1 (queue weight elt)

 (if (node-empty queue)
      (make-node weight elt (empty-node) (empty-node))
    (let 
        ((w (node-w queue))
         (e (node-elt queue))
         (left (node-left queue) )
         (right (node-right queue)))
      
      (if (<= (weight-compare weight w) 0 )                 
          (make-node weight elt (queue-insert1 right w e ) left) ;we add the node at this place and we switch the left and right trees
        (make-node w e (queue-insert1 right weight elt) left ))))) ;we add the node in the left tree and we switch the left and right trees (in order to equilibrate the tree)






 
(defun queue-reinsert (heap weight elt)
  "Insert back an element after it has been evaluated"
  (make-heap :elts (heap-elts heap) :queue (queue-insert1 (heap-queue heap) weight elt))
)
 


;; Error when a queue is empty
(define-condition queue-is-empty (error)
                  ()
                  (:report (lambda (condition stream)
                             (format stream "Queue is empty !"))))


 
(defun queue-top (heap)
  "returns the smallest element of the heap"
  (let ((queue (heap-queue heap)))
    (if (node-empty queue)
        (error (make-condition 'queue-is-empty))
      (values (node-elt queue) (node-w queue)))))



(defun queue-remove-top (queue)
  "removes the smallest element of the heap"

     (if (node-empty queue)
        (error (make-condition 'queue-is-empty))
      (let
          ((w (node-w queue))
           (e (node-elt queue))
           (left (node-left queue) )
           (right (node-right queue) ))
        (cond 
         ((node-empty left)
          right)
         ((node-empty right)
          left)
         (t
          (let 
              ((lw (node-w left))
               (le (node-elt left))
               (rw (node-w right))
               (re (node-elt right)))
            (if (<= (weight-compare lw rw) 0)
                (make-node lw le (queue-remove-top left) right)
              (make-node rw re left (queue-remove-top right)))
            ))))))





 
(defun queue-extract (heap)
  "given a heap, returns the smallest weight, the corresponding run, and the heap with the smallest element removed"
  (let ((queue (heap-queue heap)))
  (if (node-empty queue)
      (error (make-condition 'queue-is-empty))
    (values (node-w queue) (node-elt queue) (make-heap :elts (heap-elts heap) :queue (queue-remove-top queue))))))





;;;===================
;;;       Save
;;;===================



(in-package :om)

(defmethod om::omng-save ((self rq::heap) &optional values?)
  `(rq::make-heap  :elts ',(rq::heap-elts self) :queue ,(om::omng-save (rq::heap-queue self))))

(defmethod om::omng-save ((self rq::node) &optional values?)
  (if (rq::node-empty self)
       `(rq::empty-node)
     `(let ((left ,(om::omng-save (rq::node-left self)))
            (right ,(om::omng-save (rq::node-right self)))
            (w ,(om::omng-save (rq::node-w self))))
        (rq::make-node w ',(rq::node-elt self) left right ,(rq::node-empty self)))))
