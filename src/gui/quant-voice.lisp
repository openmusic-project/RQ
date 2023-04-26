(in-package :rq)

;;; Quant-voice
;;; Contains the functions relative to the editor panel in a rq.

(defclass! quant-voice (voice) 
   ((rq :accessor rq :initform nil)     ; Reference to the RQ system it is in
    (k-bests :accessor k-bests :initform nil ) ;reference to the k-best list of the RQ system (REDUNDANT, TO BE DELETED, SEE UML DIAGRAM)
   (indexes :accessor indexes :initarg :indexes :initform nil ) ;list of index of automaton (one per segment) in the list of automata
   (ks :accessor ks :initarg :ks :initform nil )  ;list of ranks (one per segment) of the solution corresponding to the voice chosen
   (slurs :accessor slurs :initarg :slurs :initform nil ) ;list of slurs for each segment
   (choices :accessor choices :initarg :choices :initform nil ) ;choices is a list of choice-table (one per segment) holding a hashtable where keys are paths and entries are tuples (runs ll rr)
   (editflag :accessor editflag :initarg :editflag :initform t ) 
   (colorflag :accessor colorflag :initarg :colorflag :initform nil ))) 



;; SET-TREE is called each time the voice's tree is modified
;; the tempo is updated each time
(defmethod set-tree ((self quant-voice) tree)
  "This function is called each time the voice's tree is modified. The tempo is updated each time."
  (let ((voicepanel (om::panel (voice-editor (editor (rq self))))))
    (setf (om::selection? voicepanel) nil)
    (initialize-instance self
                         :tree tree :chords (get-chords (chord-seq (rq self)))
                         :tempo (get-tempo self))
    (update-grids self)
    (om::update-panel voicepanel)
    (om-invalidate-view (om::panel (chord-seq-editor (editor (rq self)))))))

(defmethod init-all-choices ((self quant-voice) &optional (nmes nil))
  "Inits the choices structures for each measure. Basically, it clones in a more compact way what is useful in the ktable corresponding."
  (let* ((k-bests (k-bests self))
         (nsegment (length k-bests))
         (indexes (indexes self))
         (ks (ks self))
         (ktables  (mapcar #'(lambda (index k-best) (nth index (ktables k-best))) indexes k-bests))
         (inputs (mapcar #'(lambda (index k-best) (nth index (inputs k-best))) indexes k-bests))
         (out   (loop for i from 1 to nsegment collect (make-instance 'choice-table))))         
          (loop for ktable in ktables
                for input in inputs
                for k in ks
                for choice in out do
                (setf (table choice) (make-hash-table :test 'equalp))    
                (add-choice choice ktable input (path-empty input) k))
          (setf (choices self) out)))

(defmethod init-one-choice ((self quant-voice) nmes)
  "Inits the choice structure for only one measure. Basically, it clones in a more compact way what is useful in the ktable corresponding."
  (let* ((k-bests (k-bests self))
         (nsegment (length k-bests))
         (index (nth nmes (indexes self)))
         (k (nth nmes (ks self)))
         (ktable (nth index (ktables (nth nmes k-bests))))
         (input (nth index (inputs (nth nmes k-bests))))
         (out (make-instance 'choice-table)))
      (setf (table out) (make-hash-table :test 'equalp))
      (add-choice out ktable input (path-empty input) k)
      (setf (nth nmes (choices self)) out)
      (update-grids self)))


(defmethod update-tree ((self quant-voice) &optional (render nil))
  "Builds the complete OM rhythm tree from all the choice-tables" 
  (let* ((choices (choices self))    
         (k-bests (k-bests self))
         (inputs (mapcar #'(lambda (index k-best) (nth index (inputs k-best))) (indexes self) k-bests))
         (sigs (get-signatures-of-solution self))
         (gracenotes (append '(0) (loop for choice in choices
                                        for input in inputs collect (get-gracenotes choice input 1))))
         (slurs (slurs self))
         trees)
    (if (null k-bests)
        '(1 (((4 4) (-1))))
      (progn
        (setq trees
              (loop for choice in choices
                    for input in inputs
                    for slur in slurs
                    for gracenote in gracenotes collect (make-tree-from-choice-table choice input slur gracenote render)))
        
        (setq out (mapcar #'(lambda (sig tree) (append (list sig) (list tree))) sigs trees))
        (setq out (append (list '?) (list out)))
        (if render
            (reduce-rt out)
          out)))))

(defmethod get-tempo ((self quant-voice))
  "Returns a list of tempi compatible with a voice corresponding to the tempi of the selected transcriptions."
  (let* ((choices (choices self))
         (k-bests (k-bests self))
         (inputs (mapcar #'(lambda (index k-best) (nth index (inputs k-best))) (indexes self) k-bests))
         (sigs (loop for choice in choices
                     for input in inputs
                     for k-best in k-bests
                     collect (get-signature choice input (tempomerge k-best))))
         (tempi (mapcar #'(lambda (input sig) (round (* (/ 60000 (input-dur input)) (car sig)))) inputs sigs))
         (tempo1 (list (list 1/4 (car tempi)) nil))
         (prev-temp (car (car tempo1)))
         (tempo2 nil)
         )
    (if (null k-bests)
        60
      (progn
        (when (rest tempi)
          (setq tempo2 (loop for temp in (rest tempi)
                             for i = 1 then (1+ i)
                             unless (equalp prev-temp temp)
                             collect (list (list i 0) (list 1/4 temp))
                             do (setq prev-temp temp))))
        (append (list (car tempo1)) (list tempo2)))
      )))

(defmethod update-grids ((self quant-voice))
  "Updates the grid corresponding to the current tree in the chord-seq panel for each segment"
  (let* ((choices (choices self))
         (k-bests (k-bests self))
         (inputs (mapcar #'(lambda (index k-best) (nth index (inputs k-best))) (indexes self) k-bests))
         (grids  (make-grid choices k-bests inputs))
         (k-best-analysis (get-k-best-analysis (chord-seq (rq self)))))
    (loop for segment in (analysis-segments k-best-analysis)
          for grid in grids
          do
          (setf (grid (segment-data segment)) grid))))


(defmethod get-cons-list (self)
  "Returns the cons-list (path in the OM tree) for an object. Handles the case of gracenotes. 
WARNING : due to the handling of the gracenotes, chords (as opposed to single notes) are considered as grace notes."
  
  (if (and (equal (type-of self) 'chord) (= 1 (length (cadr (om::tree (om::parent self))))))  (reverse (cdr (om::cons-container-path self))) (reverse (om::cons-container-path self))))


(defmethod get-trees (self (inside quant-voice))
  "Gets the list of the best sub-trees for an object (group or chord).
Second value is true iff more values can be computed"
  (let* ((k-bests (k-bests inside))
         (path1  (print (get-cons-list self)))
         (nmes (or (first path1) 0))
         (k-best (nth nmes k-bests))
         (pathflag   (print (cons-list->path path1 inside)))
         (path (car pathflag))
         (flag (cdr pathflag))
         (index (nth nmes (indexes inside)))
         (k (nth nmes (ks inside)))
         (ktable (nth index (ktables k-best)))
         (input (nth index (inputs k-best)))
         (choice (nth nmes (choices inside)))
         (entry (gethash path ktable))
         (nbests (length (entry-bestlist entry nil)))
         (prev-gracenotes (previous-gracenotes choice input nil nil path ))
         (prev-input (get-last-input input path)))
                     
    (values  (loop for k from 1 to nbests collect
                   (let ((tree (rq-to-omtree ktable input k nil nil prev-gracenotes path prev-input)))
                     (if (equal tree '(-1))
                         (list 1 tree)
                       (reduce-rt (list 1 tree)))))
             (null (queue-empty  (entry-candidates entry nil))))))

(defmethod get-signature-of-group (self (inside quant-voice))
  "Gets the signature (ie the length) of the segment corresponding to the group/chord"
  (let* ((path1 (get-cons-list self))
         (nmes (or (first path1) 0))
         (path (car (cons-list->path path1 inside)))
         (measure-signature (nth nmes (get-signatures inside))))
    (labels ((subdivide (num denom path)
               (if (null path)
                   (list num denom)
                 (if (equalp num 1)
                     (subdivide num (* denom (car (car path))) (rest path))
                   (subdivide (/ num (car (car path))) denom (rest path))))))
      (subdivide (car measure-signature) (second measure-signature) (path-val path)))))

(defmethod get-gracenotes-from-obj (self (inside quant-voice))
  "Gets a list holding the number of gracenotes (notes aligned to the right) for each proposition for the current segment"
  (let* ((k-bests (k-bests inside))
         (path1 (get-cons-list self))
         (nmes (or (first path1) 0))
         (k-best (nth nmes k-bests))
         (pathflag   (cons-list->path path1 inside))
         (path (car pathflag))
         (flag (cdr pathflag))
         (index (nth nmes (indexes inside))) 
         (k (nth nmes (ks inside)))
         (ktable (nth index (ktables k-best)))
         (input (nth index (inputs k-best)))
         (nbests (length (entry-bestlist (gethash path ktable) nil))))
    (loop for k from 1 to nbests collect
          (k-best-segment-get-gracenotes ktable input k nil path))))

(defmethod get-colors (self (inside quant-voice))
  "Gets a list of couples (distance color) for each proposition for the current segment"
    (let* ((k-bests (k-bests inside))
           (path1 (get-cons-list self))
           (nmes (or (first path1) 0))
           (k-best (nth nmes k-bests))
           (pathflag   (cons-list->path path1 inside))
           (path (car pathflag))
           (flag (cdr pathflag))
           (index (nth nmes (indexes inside))) 
           (k (nth nmes (ks inside)))
           (ktable (nth index (ktables k-best)))
           (input (nth index (inputs k-best)))
           (nbests (length (entry-bestlist (gethash path ktable) nil))))
    (loop for k from 1 to nbests collect
          (let ((dist (/ (round (* 1000 (weight-distance-normalized-by-length (nth-value 1 (rq-best  ktable input path k nil)) path))) 10.0))) ;Distance in percets instead of decimal value
            (list dist (when (colorflag inside) (distance-to-color dist)))))))

(defmethod get-signatures-of-solution ((self quant-voice))
  "Returns the signatures for each measure in the form of a list"
  (let* ((choices (choices self))
        (k-bests (k-bests self))
        (inputs (mapcar #'(lambda (index k-best) (nth index (inputs k-best))) (indexes self) k-bests)))
    
    (loop for choice in choices
          for k-best in k-bests
          for input in inputs collect
          (get-signature choice input (tempomerge k-best)))))



(defmethod compute-more-solutions ((voice quant-voice) self n)
  "To compute N additional solutions for a subtree"
  (let* ((k-bests (k-bests voice))
         (path1 (get-cons-list self))
         (nmes (or (first path1) 0))
         (k-best (nth (first path1) k-bests))
         (pathflag   (cons-list->path path1 voice))
         (path (car pathflag))
         (flag (cdr pathflag))
         (index (nth (first path1) (indexes voice))) 
         (k (nth (first path1) (ks voice)))
         (ktable (nth index (ktables k-best)))
         (input (nth index (inputs k-best)))
         (nbests (length (entry-bestlist  (gethash path ktable) flag))))
    (if (null (path-val path))
        (rq-best-top ktable input (+ nbests n))
      (rq-best  ktable input path (+ nbests n) flag))))  




(defmethod cons-list->path (path1 (voice quant-voice))
  "Converts from the cons-list representation to a path structure"
  (let* ((k-bests (k-bests voice))
         (nmes (or (first path1) 0))
         (k-best (nth nmes k-bests))
         (index (nth nmes (indexes voice)))
         (choice (nth nmes (choices voice)))
         (k (nth nmes (ks voice)))
         (ktable (nth index (ktables k-best)))
         (input (nth index (inputs k-best)))
         (path2 (path-empty input))
         (run (rq-best choice  input path2 k nil)))
    (labels ((build-path (path1 run path2 flag)
                (if (or (null path1)
                        (null run))
                     (cons path2 flag)
                  (let* ((new-path (path-addi path2 (length run) (1+  (car path1) )))
                         (kflag (nth (car path1) run))
                         (new-k (car kflag))
                         (new-flag (cdr kflag))
                         (new-run (or (rq-best choice input new-path new-k new-flag) 
                                      (rq-best ktable input new-path new-k new-flag))))
                    (build-path (rest path1) new-run new-path new-flag)))))
      (if (= (length run) 1)
          (build-path (cdr path1) run path2 nil)
        (build-path (cddr path1) run path2 nil)))))


(defmethod om::get-object-selection-color ((self t) (in quant-voice))
  "Gets the color of the object SELF that is currently selected (to display the color of the selected object in the voice panel)"
  (when (colorflag in)
    (unless (or (typep self 'quant-voice) (is-in-edited-group self in) (contains-edited-group self in))
      (let* ((path1 (get-cons-list self))
             (nmes (or (first path1) 0))
             (pathflag   (cons-list->path path1 in))
             (path (car pathflag))
             (choice (nth nmes (choices in))))
        (distance-to-color (/ (round (* 1000 (weight-distance-normalized-by-length (nth-value 1 (rq-best choice nil path nil nil)) path))) 10.0))))))


; Functions relative to user-modified choices (cf quant-voice-panel.lisp)
(defmethod is-in-edited-group (obj (voice quant-voice) &key ((:equal? eq) t))
  "true iff obj is inside a group that was edited by the user. When the group is the one that was edited (when it belongs to the user-edit list), returns equal?"
  (let* ((conslist (get-cons-list obj))
         (choice (nth (car conslist) (choices voice))))
    (is-in-edited-group (cdr conslist) choice :equal? eq)
    )
)

(defmethod contains-edited-group (obj (voice quant-voice) &key ((:equal? eq) t))
  "true iff obj contains a group that was edited by the user. When obj is the one that was edited (when it belongs to the user-edit list), returns equal?"
    (let* ((conslist (get-cons-list obj))
           (choice (nth (car conslist) (choices voice))))
      (find-if #'(lambda (x) (is-an-ancestor (cdr conslist) (car x) eq)) (user-edit choice))))



;;Function called by open-trees-panel (cf quant-voice-panel.lisp)
(defmethod set-tree-choice (self  (voice quant-voice) k)
  "Replaces the sub-tree of index k in the list into the choice table"
  (let* ((k-bests (k-bests voice))
         (path1 (get-cons-list self))
         (nmes (or (first path1) 0))
         (k-best (nth nmes k-bests))
         (pathflag   (cons-list->path path1 voice))
         (path (car pathflag))
         (flag (cdr pathflag))
         (index (nth nmes (indexes voice))) 
         (ktable (nth index (ktables k-best)))
         (input (nth index (inputs k-best)))
         (choice (nth nmes (choices voice))))
    
    ;This function recomputes and updates recursively the distance values and the aligns for the choices in the choice-table, from the modified subtree up to the root of the tree
    (labels ((update-distance (path rest-path-val)
               (multiple-value-bind (run weight rr ll) (rq-best choice input path 1 nil)
                 
                 (if (null rest-path-val)
                     (when run
                       (let ((wl (loop for i from 1 to (length run) collect (nth-value 1 (rq-best choice input (path-addi path (length run) i ) 1 nil))))
                             (new-rr (nth-value 3 (rq-best choice input (path-addi path (length run) (length run)) 1 nil)))
                             (new-ll (nth-value 2 (rq-best choice input (path-addi path (length run) 1) 1 nil)))
                             )
                         (set-weight choice path (weight-addall wl (input-precision input) (input-gracepen input)))
                         (set-aligns choice path  new-ll new-rr))
                       )
                   (progn 
                     (update-distance (path-add path (car rest-path-val)) (rest rest-path-val) )
                     (let ((wl (loop for i from 1 to (length run) collect (nth-value 1 (rq-best choice input (path-addi path (length run) i) 1 nil))))
                           (new-rr (nth-value 3 (rq-best choice input (path-addi path (length run) (length run)) 1 nil)))
                           (new-ll (nth-value 2 (rq-best choice input (path-addi path (length run) 1) 1 nil))))
                         (set-weight choice path (weight-addall wl (input-precision input) (input-gracepen input)))
                         (set-aligns choice path new-ll  new-rr)))))))
      ;From the selected node down to the leaves, update the runs and distances
      (add-choice choice ktable input path (1+ k) nil)
      ;From the selected node up to the root, recompute the distance values and aligns
      (update-distance (path-empty input) (path-val path) )
      (setf (modified choice) t)
      ;Overwrite the user modifications that are invalidated by the current modification
      (setf (user-edit choice) (remove-descendents-from-user-edit (cdr path1) choice))
      )))

(in-package :om)
(defmethod om::omng-save ((self rq::quant-voice) &optional values?)
  `(let ((qv ,(call-next-method)))
    (setf (rq::indexes qv) ',(rq::indexes self)
          (rq::ks qv) ',(rq::ks self)
          (rq::slurs qv) ',(rq::slurs self)
          (rq::choices qv) ,(om::omng-save (rq::choices self))
          (rq::editflag qv) ,(rq::editflag self)
          (rq::colorflag qv) ,(rq::colorflag self))
    qv))






