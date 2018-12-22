(in-package :rq)


;------------ CHOICE-TABLE -------------;
;The choice-table stores the modified version of the chosen transcription (the one displayed in the bottom panel). 
;
;It is a compact version of the global hashtable found in the k-best structures (same keys), 
;but with less entries, and instead of an "entry" structure, each entry is a list :
;(selected-run weight-of-selected-run ll-align rr-align)
;It holds only one solution, that is the selected solution, instead of the whole best-list.
;
;It also stores the list of modifications done by the user manually. 
;Each modification is a list of 2 elements : the first is the tree to be replaced, the second is the cons-container-path where it has to be replaced
;Each time a modification is done manually by the user, the modification is added to the list. 
;All the previous modifications that are erased by the new one are removed from the list.


(defclass! choice-table ()
  ((table :accessor table :initarg :table :initform nil)
   (modified :accessor modified :initarg :modified :initform nil)      ;true iff the table has been modified (ie the solution has been edited)
   (user-edit :accessor user-edit :initard :user-edit :initform nil))) ;nil if the tree was not edited by the user, only by exploring the choice-table. If the user edited it, it stores the sub-tree along with the cons-container-path at which it has to be replaced.

  


               
(defmethod add-choice ((choice choice-table) ktable input path k &optional (flag nil))
  "Adds recursively the Kth best run in KTABLE at PATH to the choice-table CHOICE"
  (multiple-value-bind (run weight ll rr) (rq-best ktable input path k flag)
    (setf (gethash path (table choice)) (list run weight ll rr)) 
    (when run
      (let ((r (length run)))
        (loop for i from 1 to r do
              (add-choice choice ktable input (path-addi path r i) (car (nth (1- i) run)) (cdr (nth (1- i) run)) ))))))


(defmethod set-weight ((choice choice-table) path weight)
  "Sets the weight of entry PATH in CHOICE"
  (setf (second (gethash path (table choice))) weight)
)

(defmethod set-aligns ((choice choice-table) path ll rr)
  "Sets the aligns lists (list of indexes of inputs aligned to the left and to the right) of entry PATH in CHOICE"
  (setf (third (gethash path (table choice))) ll
        (fourth (gethash path (table choice))) rr))


(defmethod make-tree-from-choice-table ((choice choice-table) input slur gracenote render)
  "Builds an OM rhythm tree from one choice table, taking into account the user-edit modifications."
  (let (out)
    (setq out (rq-to-omtree choice input 1 nil slur gracenote))

    ;Apply the modifications done by the user
    (when (user-edit choice)
      (loop for edit in (user-edit choice) do
            ;when there is only 1 beat in the measure, the first level of the tree is removed
            ;so we have to adapt the cons-container-path consequently.
            (if (= 1 (length (rq-best choice input (path-empty input) 1 nil))) 
                (set-subtree-in-place (car out) (cadr edit) (car edit))
              (set-subtree-in-place (car out) (cadr edit) (cdar edit)))))

    ;Render tree
    (if render
        (setq out (rq-remove-top-level out))
      )
    out)
)

(defmethod make-grid (tables ks inputs)
  "Makes the grids for all the choice-tables and return them in a list"
             ;This function creates one grid for one table
    (labels ((grid (table inst path k &optional (length 1) (conslist '(0)))
               (let* ((run (rq-best table inst path k nil))
                      (r (length run)))
                 (if (and run (not (is-in-edited-group conslist table))) ;We do not create the grid for the subtrees that were modified by the user
                     (loop for i from 1 to r collect
                           (grid table inst (path-addi path r i) (car (nth (1- i) run)) (/ length r) (append conslist (list (1- i)))))
                   length))))
      (loop for table in tables
            for input in inputs
            for k in ks
            collect
            (let ((grid (grid table input (path-empty input) k))) 
              (if (listp grid)
                  grid
                (list grid))))))


;------------ Functions to replace a subtree by another inside a rhythm tree ------------;

(defun get-subtree (tree conslist)
  (if (null conslist)
      tree
    (if (atom tree)
        tree
      (get-subtree (nth (car conslist) (cadr tree)) (cdr conslist))))


)

;; Warning : this function works in-place !
(defun set-subtree-in-place (tree subtree conslist)
  (if (= (length conslist) 1)
      (setf (nth (car conslist) (cadr tree)) subtree)  
    (if (atom tree)
        tree
      (set-subtree-in-place (nth (car conslist) (cadr tree)) subtree (cdr conslist))))


)

(defun set-subtree (tree subtree conslist)
  (let ((out (copy-tree  tree)))
    (set-subtree-in-place out subtree conslist)
    out))


;---------- Functions relative to the user-edit list ----------;

(defmethod add-change-to-user-edit ((choice choice-table) conslist tree)
  "Adds a modification to the user-edit list of CHOICE"
  (let ((user-edit (user-edit choice)))
    ;remove obsolete changes
    (setq user-edit
          (loop for elt in user-edit
                unless (is-an-ancestor conslist (car elt))
                collect elt))
    ;add current change to list
    (setf (user-edit choice) (append user-edit (list (list conslist tree)))))

)

;true iff cons1 is an ancestor of cons2
;when conslist1 and conslist2 are equal, returns equal?
(defmethod is-an-ancestor (conslist1 conslist2 &optional (equal? t))
  "True iff CONSLIST1 is an ancestor of CONSLIST2. When CONSLIST1 and CONSLIST2 are equal, returns equal?"
  (if (null conslist1)
      (if (null conslist2)
          equal?
        t)
    (if (and (car conslist2) (= (car conslist1) (car conslist2)))
        (is-an-ancestor (cdr conslist1) (cdr conslist2) equal?)
      nil))
)

(defmethod remove-descendents-from-user-edit (conslist (choice choice-table) &key ((:equal? eq) t))
  "Remove obsolete modifications (modifications of descendants of a newly modified group"
  (remove-if #'(lambda (x) (is-an-ancestor (car x) conslist eq)) (user-edit choice))
)


(defmethod is-in-edited-group (conslist (choice choice-table) &key ((:equal? eq) t))
  "true iff CONSLIST is a descendent of one of the groups modified by the user. When CONSLIST belongs to the user-edit list, returns equal?"
  (find-if #'(lambda (x) (is-an-ancestor (car x) conslist eq)) (user-edit choice))
)



;------------- Redefinition of the rq functions for the choice-table -----------;

;;; To build the tree using the same rq-to-omtree function, we have to redefine rq-best and align


(defmethod rq-best ((ktable choice-table) inst p k flag)
  (let ((val (gethash p (table ktable))))
    (values (first val) (second val) (third val) (fourth val))))

(defun choice-inhabited (ktable p)
  (distance-inhabitedp (weight-dist (second (gethash p (table ktable))))))

(defmethod align ((ktable choice-table) p)
  (let ((out (gethash p (table ktable))))
    (values (third out)   (fourth out))))

(defmethod notes-align ((ktable choice-table) inst p)
  (let ((out (gethash p (table ktable))))
    (values (remove-silences (third out) inst)  (remove-silences (fourth out) inst))))

(defmethod get-gracenotes ((choice choice-table) inst k  &optional (flag nil) (path (path-empty inst)))
    (length (nth-value 1 (notes-align choice inst path))))

(defmethod previous-subdiv ((choice choice-table) path)
  (let* ((val (path-val path))
         (last-val (car (last val))))
    (if (= (path-begin path) 0) ;if the current subdivision is the first
        nil
      (if (= 1 (cdr last-val))
          (let* ((father (path-father path)))
            (previous-subdiv choice father))
      (path-previous path)))))

(defmethod get-last-input (input path)
  (let* ((onsets (input-input input))
         (mask (input-mask input))
         (begin (path-begin path))
         (index (position-if #'(lambda (x) (< x begin)) onsets :from-end t)))
    (if (and index (plusp (aref mask index )))
        1
      0)))

(defmethod previous-gracenotes (choice input k flag path)
  (let* ((previous (previous-subdiv choice path)))
    (if previous 
            (get-gracenotes choice input k flag previous)
      0)))


;----- Save ------;
(in-package :om)
(defmethod om::omng-save ((self rq::choice-table) &optional values?)
  (let ((choicetable (rq::table self)))
  `(let* ((choice (make-instance 'rq::choice-table)))

     (setf (rq::table choice) (make-hash-table :test 'equalp))

  
     ,(let ((entries nil))
        
        (maphash #'(lambda (p e) (setq entries (append entries 
                                                               (list `(gethash ,(om::omng-save p) (rq::table choice))) 
                                                               (list `(list ',(first e) ,(om::omng-save (second e)) ',(third e) ',(fourth e)))))) choicetable)
        (append (list `setf) entries))

     (setf (rq::modified choice) ,(rq::modified self))
     (setf (rq::user-edit choice) ',(rq::user-edit self))
     choice)))
