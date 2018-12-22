(in-package :rq)

;;; Quant-voice Panel
;;; All the functions related to the bottom panel
;;; In particular, there are the functions to open the editor dialog (with the list of propositions for a subtree) and the user-edit dialog (to type a subtree)

(defclass! quant-voice-panel (om::voicePanel)
  ())

;Update the chord-seq view each time the voice-panel is clicked
;in order to highlight in the top panel the segment that corresponds to the selected subtree.
(defmethod om-view-click-handler ((self quant-voice-panel) where)
  "When the voice panel is clicked, updates the chord-seq panel to highlight the current subdivision."
  (call-next-method)
  (when (parent (om::editor self))
    (om::update-panel (om::panel (chord-seq-editor (parent (om::editor self)))))))

;Double click on a measure in the bottom panel to go to that segment in the right panel
(defmethod om-view-doubleclick-handler ((self quant-voice-panel) where)
  "When the voice panel is double-clicked, go to this segment in all views."
  (when (plusp (om::pixels-to-time self (om-point-x where)))
    (let* ((x-click  (om-point-x where))
           (measures (om::inside (om::graphic-obj self)))
           (x-list (mapcar #'(lambda (mes) (car (om::rectangle mes))) measures))
           (selected (position-if #'(lambda (x) (<= x x-click)) x-list :from-end t))
           (wp (wp (rq (om::object (om::editor self))))))
      (when selected
        (change-poly (poly-editor (parent (om::editor self))) (- selected (current-poly wp))))
      (om-invalidate-view self)))
  (call-next-method))



(defmethod om::draw-object :after ((self om::grap-voice) (view quant-voice-panel) x y zoom minx maxx miny maxy slot size linear? staff grille-p chnote)
  "To draw the additional info in the quant-voice"
  (let* ((qs (om::value (om::ref (om::editor view))))
         (current (current-poly (wp qs)))
         (measures (om::inside self))
         (current-measure (nth current measures)))
    ;Draw a rectangle around the selected measure
    (om-with-fg-color view om::*om-red2-color* 
      (om-draw-rect-outline (nth 0 (om::rectangle current-measure))
                            (- (nth 1 (om::rectangle current-measure)) 10)
                            (- (nth 2 (om::rectangle current-measure)) (nth 0 (om::rectangle current-measure)))
                            (+ (- (nth 3 (om::rectangle current-measure)) (nth 1 (om::rectangle current-measure))) 20)
                            1))))

;The purpose of the following functions is to open the dialog with the possible transcriptions when a group, a note or a rest is double-clicked
(defmethod om::obj-for-internal-editor ((self om::group)) (if (equal (type-of (om::get-root-parent self)) 'quant-voice) '(group-selection) nil))
(defmethod om::obj-for-internal-editor ((self om::chord)) (if (equal (type-of (om::get-root-parent self)) 'quant-voice) '(group-selection) (list 'om::chordEditor "internal chord")))
(defmethod om::obj-for-internal-editor ((self om::rest))  (if (equal (type-of (om::get-root-parent self)) 'quant-voice) '(group-selection) nil))
(defmethod om::obj-for-internal-editor ((self om::continuation-chord)) (if (equal (type-of (om::get-root-parent self)) 'quant-voice) '(group-selection) nil))

(defmethod om::handle-internal-open ((self quant-voice-panel) (type (eql 'group-selection)) (obj om::chord) add-info)
  (choose-and-update-solution self type obj add-info))         
(defmethod om::handle-internal-open ((self quant-voice-panel) (type (eql 'group-selection)) (obj om::group) add-info)
  (choose-and-update-solution self type obj add-info))
(defmethod om::handle-internal-open ((self quant-voice-panel) (type (eql 'group-selection)) (obj om::rest) add-info)
  (choose-and-update-solution self type obj add-info))

;---------- Functions to open the panel with the various propositions of a particular subtree ----------;

(defmethod choose-and-update-solution ((self quant-voice-panel) (type (eql 'group-selection)) obj add-info)
  "Opens the dialog containing all the possible transcriptions, then updates the choice-table and the voice's tree. Also focuses the view of the chord-seq on the current subdivision"
  (focus-view (chord-seq-editor (parent (om::editor self))) (or (car (get-cons-list obj)) 0))
  (let ((dialog-return t))
    ;as long as the "more" button is pressed, re-open the dialog
    (loop while (equal dialog-return t) do 
          (setq dialog-return (open-trees-panel self type obj add-info)))
    ;when a solution has been selected
    (when (numberp dialog-return)
      (let ((voice (om::object (om::editor self))))
        (set-tree-choice obj voice dialog-return)
        (set-tree voice (update-tree voice (not (editflag voice)))))
)))

(defmethod open-trees-panel ((self quant-voice-panel) (type (eql 'group-selection)) obj add-info)
  "Opens the tree panel, that displays the othe solutions for the object OBJ selected"

  (when (editflag (om::object (om::editor self))) ;when edition is allowed
    (unless  (is-in-edited-group obj (om::object (om::editor self)) :equal? nil)   ;disable edition when inside a tree that was edited by the user 
                                                                                   ;(but allow it for the group that was modified)   
    (let* ((voice (om::object (om::editor self)))
           (gracenotes (get-gracenotes-from-obj obj voice))
           (colors  (get-colors obj voice) )
           (signature (get-signature-of-group obj voice))
           (h (max 70 (+ 70 (* 65 (length gracenotes)))))
           (selection nil)
           (dialog (om-make-window 'om-dialog
                                   :size (om-make-point 260 (min h 550))
                                   :window-title "" :owner nil
                                   :maximize nil :minimize nil :resizable t
                                   :position (om-add-points (om-view-position (om::window self)) (om-mouse-position self))))

           (choice-panel (om-make-view 'om-scroller 
                                       :scrollbars :v 
                                       :background :gray 
                                       :field-size (om-make-point 235 (- h 60))  
                                       :position (om-make-point 10 5) 
                                       :size (om-make-point 235 (min (- h 60) 490))))
           

           (morebutton (om-make-dialog-item 'om-button (om-make-point (- (om::w dialog) 90) (- (om::h dialog) 45)) (om-make-point 70 15) "More"
                                          :di-action (om-dialog-item-act item 
                                                       (declare (ignore item))
                                                       (compute-more-solutions voice obj 3)
                                                       (om-return-from-modal-dialog dialog t)
                                                        )  
                                          :focus nil :default-button nil)))
      (om-set-bg-color choice-panel (om-make-color 0.95 0.95 0.95))

      (multiple-value-bind (trees more?) (get-trees obj voice)
        (om-add-subviews dialog choice-panel)
        (apply 'om-add-subviews 
               choice-panel
               (loop for a-tree in trees
                     for grace in gracenotes
                     for color in colors
                     for i = 0 then (+ i 1) collect
                     (let* ((b-tree (if (numberp a-tree) (list a-tree '(1)) a-tree))
                            (view (om-make-view 'tree-view :position (om-make-point 10 (+ 10 (* i 65)))
                                                :size (om-make-point 200 60)
                                                :tree (make-instance 'tree-voice 
                                                                     :tree (list '? (list (list 
                                                                                           signature
                                                                                           (list (list 1 (cadr b-tree))))))
                                                                     :chords '(6200)
                                                                     :dist (first color)
                                                                     :color (second color))        
                                                :gracenotes grace
                                                :dialog dialog
                                                :k i)))
                       view)))
        

        (apply 'om-add-subviews 
               (cons dialog
                     (list 
                      morebutton
                      (om-make-dialog-item 'om-button (om-make-point 20 (- (om::h dialog) 45)) (om-make-point 70 15) (om::om-str :cancel)
                                           :di-action (om-dialog-item-act item 
                                                        (declare (ignore item))
                                                        (om-return-from-modal-dialog dialog nil))
                                           :focus nil :default-button t)
                      )))
      (om-enable-dialog-item morebutton more?)
      (om-modal-dialog dialog))))))


;------ Function to open the panel allowing the user to hand-write a subtree ------;

(defmethod open-user-edit-panel ((self quant-voice-panel) obj)
  "Opens the panel to type in an OM rhythm tree"
  (when (editflag (om::object (om::editor self)))
    
    (let* ((voice (om::object (om::editor self)))
           (win (om-make-window 'om-dialog :position :centered 
                               :size (om-make-point 400 150)))
          (pane (om-make-view 'om-view
                              :size (om-make-point 370 130)
                              :position (om-make-point 10 10)
                              :bg-color *om-white-color*))
          (cons-list (get-cons-list obj))
          (current-tree  (get-subtree (om::tree voice) cons-list))
          treetxt)
      (om-add-subviews pane
                       (om-make-dialog-item 'om-static-text (om-make-point 20 20)
                                            (om-make-point 380 40)
                                            "Replace selection by the following subtree:"
                                            :font *om-default-font2b*)
                       
                       (setf treetxt (om-make-dialog-item 'om-editable-text (om-make-point 20 60)  (om-make-point 330 13)
                                                           (format nil "~D" current-tree) 
                                                           :font *om-default-font1*))
                       (om-make-dialog-item 'om-button (om-make-point 100 90)
                                            (om-make-point 80 20)
                                            "Cancel"
                                            :di-action (om-dialog-item-act item 
                                                         (om-return-from-modal-dialog win nil)))
                       
                       (om-make-dialog-item  'om-button (om-make-point 200 90)
                                             (om-make-point 80 20)
                                             "OK"
                                             :di-action (om-dialog-item-act item 
                                                          (let ((tree (ignore-errors (read-from-string (om-dialog-item-text treetxt))))
                                                                (choice (nth (car cons-list) (choices voice)))
                                                                )
                                                            (if (tree-equal (om::resolve-? tree) tree) ;if the input tree is a correct rhythm-tree
                                                                (progn
                                                                  (add-change-to-user-edit choice (cdr cons-list) tree)
                                                                  (setf (modified choice) t)
                                                                  (om-return-from-modal-dialog win t))
                                                              (error (format nil "Invalid Rhythm Tree : ~A" tree)) 
                                                              )))))

      (om-add-subviews win pane)
      (om-modal-dialog win)
      
)))



