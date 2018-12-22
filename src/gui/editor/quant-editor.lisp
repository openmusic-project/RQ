

(in-package :rq)

;;;; Quant-editor
;;;  The general editor for a quant-system
;;;  Holds the 3 sub-editors : chord-seq, voice and poly

(defmethod om::class-has-editor-p ((self rq)) t)
(defmethod om::get-editor-class ((self rq)) 'quant-editor)
(defmethod om::default-edition-params ((self rq)) 
  (list (cons 'om::winsize (or (om::get-win-ed-size self) (om-make-point 370 280)))
        (cons 'om::winpos (or (om::get-win-ed-pos self) (om-make-point 400 20)))
        ;initialize the 3 zoom levels for all 3 sub-editors
        (cons 'chord-seq-zoom 1) 
        (cons 'voice-zoom 1)
        (cons 'poly-zoom 1)))

(defclass quant-editor (om::editorview) 
  ((chord-seq-editor :accessor chord-seq-editor :initform nil)
   (voice-editor :accessor voice-editor :initform nil)
   (poly-editor :accessor poly-editor :initform nil)
   (buttons :accessor buttons :initform nil)
   (voice-mode :accessor voice-mode :initarg :voice-mode :initform :edit)
   (display-mode :accessor display-mode :initarg :display-mode :initform :close)
   ))


(defmethod om::initialize-instance ((self quant-editor) &rest args)
  (call-next-method)
  (om-set-bg-color self (om-make-color 0.4 0.4 0.42))
  (setf (editor (om::object self)) self)
  
  ;Chord-seq
  (setf (chord-seq-editor self)
        (om-make-view 'quant-chord-seq-editor :owner self :object (chord-seq (om::object self)) :ref (om::ref self)
                      :position (om-make-point 10 10) :size (om-make-point 500 300)))
  (setf (parent (om::object (chord-seq-editor self))) (om::object self))
  (setf (parent (chord-seq-editor self)) self)
  (setf (om::score-mode (om::panel (chord-seq-editor self))) 3) ;to be in "segmentation mode" when opening the editor
  (om-invalidate-view (om::title-bar (chord-seq-editor self)))

  ;Voice
  (setf (voice-editor self)
        (om::om-make-view 'quant-voice-editor :owner self :object (solution (om::object self)) :ref (om::ref self)
                      :position (om::om-make-point 10 370) :size (om::om-make-point 500 220)))
  (setf (om::score-mode (om::panel (voice-editor self))) 0)
  (setf (parent (voice-editor self)) self)

  ;Poly
  (setf (poly-editor self)
        (om::om-make-view 'multi-poly-editor :object (nth (current-poly (wp (om::object self))) (polys (wp (om::object self))))  
                      :owner self  :ref (om::ref self)
                      :position (om::om-make-point 510 10) :size (om::om-make-point 500 530)))
  (setf (parent (poly-editor self)) self)
  (setf (om::score-mode (om::panel (poly-editor self))) 0)
  (update-poly self)
  (update-title (poly-editor self))
  
  (setf (display-mode self)  (display-mode (om::object self)))

  (let ((fgc #-linux *om-white-color* #+linux *om-black-color*))
    (setf (buttons self) (list
			  (om-make-dialog-item 'om-button (om-make-point (- (om::w (om::window self)) 130) 325) (om-make-point 100 20) "Parameters"
					       :di-action #'(lambda (b)
							      (progn
								(set-global-k-best-parameters (om::object self))
								(om-invalidate-view (om::panel (chord-seq-editor self)))
								)))

                          (om-make-dialog-item 'om-radio-button (om-make-point (- (om::w (om::window self)) 340) 317) (om-make-point 120 20) "Edit" :font *om-default-font1b* :fg-color fgc
					       :checked-p (editflag (om::object (voice-editor self)))
					       :di-action #'(lambda (b) 
							      (edit-mode-quant-voice self)))
                          (om-make-dialog-item 'om-radio-button (om-make-point (- (om::w (om::window self)) 340) 337) (om-make-point 120 20) "Render" :font *om-default-font1b* :fg-color fgc
                                               :checked-p (not (editflag (om::object (voice-editor self))))
                                               :di-action #'(lambda (b) 
                                                              (render-mode-quant-voice self)))
                          (om-make-dialog-item 'om-check-box 
                                               (om-make-point (- (om::w (om::window self)) 260) 310) (om-make-point 120 20)
                                               "Color mode" :font *om-default-font1b* :fg-color fgc
                                               :checked-p (color-flag (om::object self))
                                               :di-action #'(lambda (b) 
                                                              (setf (color-flag (om::object self)) (not (color-flag (om::object self)))
                                                                    (colorflag (solution (om::object self))) (not (colorflag (solution (om::object self)))))
                                                              (setf (om::selected-p b) (color-flag (om::object self)))
                                                              (om::update-subviews self)))
                          (om-make-dialog-item 'om-check-box 
                                               (om-make-point (- (om::w (om::window self)) 260) 330) (om-make-point 120 20)
                                               "Show pulses" :font *om-default-font1b* :fg-color fgc
                                               :checked-p (pulse-flag (om::object self))
                                               :di-action #'(lambda (b) 
                                                              (setf (pulse-flag (om::object self)) (not (pulse-flag (om::object self)))
                                                                    (pulse-flag (chord-seq (om::object self))) (not (pulse-flag (chord-seq (om::object self)))))
                                                              (setf (om::selected-p b) (pulse-flag (om::object self)))
                                                              (om::update-subviews self)))
                          (om-make-dialog-item 'om-check-box 
                                               (om-make-point (- (om::w (om::window self)) 260) 350) (om-make-point 120 20)
                                               "Show solutions" :font *om-default-font1b* :fg-color fgc
                                               :checked-p (equal :open (display-mode self))
                                               :di-action #'(lambda (b) 
                                                              (if (equal :close (display-mode self))
                                                                  (open-poly-edit self)
								  (close-poly-edit self))
                                                              (setf (om::selected-p b) (equal :open (display-mode self))))))))


  (om-add-subviews self
                   (om-make-dialog-item 'om-button (om-make-point 10 325) (om-make-point 115 20) "Segmentation"
                                        :di-action #'(lambda (b)
                                                       (when (om-y-or-n-dialog "This will delete the current segmentation. Continue ?" :default-button :yes)
                                                         (om::compute-segments (get-k-best-analysis (chord-seq (om::object self))) (chord-seq (om::object self)))
                                                         (om-invalidate-view (om::panel (chord-seq-editor self))))))

                   (om-make-dialog-item 'om-button (om-make-point 120 325) (om-make-point 100 20) "Quantify"
                                        :di-action #'(lambda (b)
                                                       (when (om-y-or-n-dialog "This will delete the current quantification. Continue ?" :default-button :yes)
                                                         (if (null (get-k-best-segments (om::object self)))
                                                             (om-message-dialog "Nothing to analyse. Please segment input.")
                                                           (progn              
                                                             (compute-k-best (om::object self))
                                                             (when (not (member nil  (k-bests (om::object self))))
                                                               (setf (wp (om::object self)) (make-working-poly (om::object self)))
                                                               (update-poly self)
                                                               (init-solution (om::object self))
                                                               (update-grids (solution (om::object self)))
                                                               (om-invalidate-view (om::panel (chord-seq-editor self)))
                                                               (om::update-panel (om::panel (poly-editor self)))
                                                               (om::update-panel (om::panel (voice-editor self)))
                                                               (update-poly self)
                                                               (update-scroll (poly-editor self))))))))

                   (om-make-dialog-item 'om-button (om-make-point 215 325) (om-make-point 120 20) "Tempo-smooth"
                                        :di-action #'(lambda (b)
                                                       (setf (selections (wp (om::object self))) (get-smoothed-selections (om::object self)))
                                                       (update-poly self)
                                                       (init-solution (om::object self))
                                                       (update-grids (solution (om::object self)))
                                                       (update-scroll (poly-editor self))))

                   (first (buttons self))
                   (second (buttons self))
                   (third (buttons self))
                   (fourth (buttons self))
                   (fifth (buttons self))
                   (sixth (buttons self))
                   )
  self)

(defmethod update-poly ((self quant-editor))
  "To update the poly-panel (redraw the poly, update the title bar)"
  (update-poly (poly-editor self))
  )


(defmethod om::update-subviews ((self quant-editor))
  "Handles subviews resizing (when the right panel is diplayed or hidden)"
  (cond ((equal (display-mode self) :open)
         (let ((middle (round (- (om::w self) 20) 2)))
           (om-set-view-size (chord-seq-editor self) (om-make-point middle (om::h (chord-seq-editor self))))
           (om-set-view-size (voice-editor self) (om-make-point middle (- (om::h self) (om::h (chord-seq-editor self)) 80)))
           (om-set-view-position (poly-editor self) (om-make-point (+ middle 20) 10))
           (om-set-view-size (poly-editor self)  (om-make-point (- middle 10) (- (om::h self) 20)))
           (update-buttons self :open)))
        ((equal (display-mode self) :close)
         (progn
           (om-set-view-size (chord-seq-editor self) (om-make-point (- (om::w self) 20) (om::h (chord-seq-editor self))))
           (om-set-view-size (voice-editor self) (om-make-point (- (om::w self) 20) (- (om::h self) (om::h (chord-seq-editor self)) 80)))
           (om-set-view-position (poly-editor self) (om-make-point (om::w self) 10 ))
           (update-buttons self :close)))
        ((equal (display-mode self) :closing)
         (progn
           (om-set-view-position (poly-editor self) (om-make-point (om::x (poly-editor self)) (om::y (poly-editor self))))
           (om-set-view-size (poly-editor self) (om-make-point (round (- (om::w self) 40) 2) (om::h (poly-editor self))))))
        ((equal (display-mode self) :opening)
         (progn
           (om-set-view-position (poly-editor self) (om-make-point (+ (om::w (chord-seq-editor self)) 20) (om::y (poly-editor self))))
           (om-set-view-size (poly-editor self) (om-make-point (- (om::w self) 20 (om::x (poly-editor self) ) ) (- (om::h self) 20)))))))

(defmethod update-buttons ((self quant-editor) display-mode)
  "Updates the position of all the buttons"
  (let ((middle (round (- (om::w self) 20) 2))
               (button1 (first (buttons self)))
               (button2 (second (buttons self)))
               (button3 (third (buttons self)))
               (button4 (fourth (buttons self)))
               (button5 (fifth (buttons self)))
               (button6 (sixth (buttons self))))
    (cond ((equal display-mode :open)
           (let ((middle (round (- (om::w self) 20) 2)))
             (om-set-view-position button1 (om-make-point (- middle 110) (om::y button1)))
             (om-set-view-position button2 (om-make-point (- middle 320) (om::y button2)))
             (om-set-view-position button3 (om-make-point (- middle 320) (om::y button3)))
             (om-set-view-position button4 (om-make-point (- middle 240) (om::y button4)))
             (om-set-view-position button5 (om-make-point (- middle 240) (om::y button5)))
             (om-set-view-position button6 (om-make-point (- middle 240) (om::y button6)))))
          ((equal display-mode :close)
           (om-set-view-position button1 (om-make-point (- (om::w self) 130) (om::y button1)))
           (om-set-view-position button2 (om-make-point (- (om::w self) 340) (om::y button2)))
           (om-set-view-position button3 (om-make-point (- (om::w self) 340) (om::y button3)))
           (om-set-view-position button4 (om-make-point (- (om::w self) 260) (om::y button4)))
           (om-set-view-position button5 (om-make-point (- (om::w self) 260) (om::y button5)))
           (om-set-view-position button6 (om-make-point (- (om::w self) 260) (om::y button6)))))))
               

(defmethod close-poly-edit ((self quant-editor))
  "A sequence to close the poly panel"
  (om::set-edit-param (om::ref self) 'display-mode :close)
  (setf (display-mode self) :closing)
  (om::update-subviews self)
  (om-set-view-size (om::window self) (om-make-point (om-point-h (om::get-win-ed-size (om::object self)) ) (om-point-v (om-view-size (om::window self)))))
  (setf (display-mode self) :close)
  (setf (display-mode (om::object self)) :close)
  (om::update-subviews self))

(defmethod open-poly-edit ((self quant-editor))
  "A sequence to open the poly panel"
  (om::set-edit-param (om::ref self) 'display-mode :open)
  (setf (display-mode self) :opening)
  (om::update-subviews self)
  (om-set-view-size (om::window self) (om-make-point (om-point-h (om::get-win-ed-size2 (om::object self)) ) (om-point-v (om-view-size (om::window self)))))
  (setf (display-mode self) :open)
  (setf (display-mode (om::object self)) :open)
  (om::update-subviews self))

;; diplay-mode = :open / :close
;; => 2 default sizes
(defmethod om::get-win-ed-size ((self rq) ) (om-make-point 750 590))
(defmethod om::get-win-ed-size2 ((self rq)) (om-make-point 1500 590))


(defmethod render-mode-quant-voice ((self quant-editor))
  "To set the voice to Render mode"
  (om::set-edit-param (om::ref self) 'voice-mode :render)
  (setf (voice-mode self) :render)
  (set-tree (om::object (voice-editor self)) (update-tree (om::object (voice-editor self)) t))
  (setf (editflag (om::object (voice-editor self))) nil))

(defmethod edit-mode-quant-voice ((self quant-editor))
  "To set the voice to Edit mode"
  (om::set-edit-param (om::ref self) 'voice-mode :edit)
  (setf (voice-mode self) :edit)
  (set-tree (om::object (voice-editor self)) (update-tree (om::object (voice-editor self)) nil))
  (setf (editflag (om::object (voice-editor self))) t))

;;;; Key event handlers:
(defmethod om::handle-key-event ((self quant-editor) char)
  "Handles key events for a `quant-editor'. All the key events are redirected to each sub-editor, depending of the last clicked panel."
  (cond ((equal *clicked-panel* (om::panel (chord-seq-editor self)))
         (om::handle-key-event  (chord-seq-editor self) char))
        ((equal *clicked-panel* (om::panel (voice-editor self)))
         (om::handle-key-event (voice-editor self) char))
        ((equal *clicked-panel* (om::panel (poly-editor self)))
         (om::handle-key-event (poly-editor self) char))
        (t (call-next-method))))



;;; Mouse event handlers:
(defvar *clicked-panel* nil) ; A hack to catch key events on each editor.

(defmethod om-view-click-handler :before ((self om::scorePanel) where)
  (setf *clicked-panel* self))
       
    





