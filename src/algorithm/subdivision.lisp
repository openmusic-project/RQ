;;; Subdivision schema

;; A subdivision schema is given in the form of a list. For example : (2 2 3) means that each measure is divided in two, then each part is again divided in two, and finally, each of these parts is divided in 3.
;; If an element of the list is a list itself, it means that the choice is given between the values of the list. For example : (2 (2 3 5) 3) means (2 2 3) or (2 3 3) or (2 5 3).
;; If an element of the list is a list of lists, it means that the choice is given between various successions of subdivisions. For example : ( ((2 3) (3 2)) ) means (2 3) or (3 2).
;; Example : ((2 3) ((2 3) ((3 5) 2))) means (2 2 3) or (2 3 2) or (2 5 2) or (3 2 3) or (3 3 2) or (3 5 2).
;; Non prime numbers can be used as, for exemple, dividing in 2 then in 2 gives a different notation than dividing in 4.

(use-package :om :rq)

(in-package :rq)

;; Error when a schema is empty
(define-condition schema-is-empty (error)
                  ()
                  (:report (lambda (condition stream)
                             (format stream "Schema is empty !"))))

(defun schema-resolution (schema)
  "resolution of the schema, ie least common multiple of the numbers of subdivisions for each possible succession of subdivisions"
  (labels ((vreso (vschema)    ;when there are successives subdivisions (v for vertical, successive subdivision are represented vertically)
             (cond ((null vschema)  1)
                   ((not (listp vschema)) vschema)
                   (t (reduce #'* (mapcar #'reso vschema)))))
           (reso (hschema) ;when there are different choices of subdivisions (choice is represented horizontally)
             (if (listp hschema)
                 (reduce #'lcm (mapcar #'vreso hschema))
               hschema)))
  (vreso schema)))

(defun schema-top (schema)
  "gives the next possible subdivisions (there can be repetitions)"
  (labels ((vtop (vschema)
             (cond  ((null vschema) nil)
                    ((not (listp vschema)) vschema)
                    (t (htop (first vschema)))))
           (htop (hschema)
             (if (listp hschema)
                 (mapcar #'vtop hschema)
               (vtop hschema))))
    (flatten (vtop schema))))

;; to flatten nested lists
(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))

(defun schema-pop (val schema)
  "gives the next schema, given we have subdivided the current node in val. val has to be one of the values given by schema-top"
  (labels ((vpop (vschema)
             (cond ((null vschema)  nil)
                   ((not (listp vschema)) (equalp val vschema))              
                   ((listp  vschema) (let ((result (hpop (first vschema))))
                                      (if result 
                                                (append (list result) (rest vschema))    
                                              nil)))
                   (t nil)))
           (hpop (hschema)
             (cond ((null hschema) nil)
                   ((not (listp hschema)) (equalp val hschema)) 
                   (t (remove nil (mapcar #'vpop hschema)))))
           (remove-t-nil-recursively (x)
             (if (listp x)
                 (remove nil (mapcar #'remove-t-nil-recursively
                         (remove nil (remove t x))))
               x)))
   (remove-t-nil-recursively (vpop schema))))
