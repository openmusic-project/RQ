
(in-package :rq)


;------------ TREE VOICE and TREE VIEW -------------;
; The voices and views used in the internal editor
; They are created by the function open-tree-panel (cf. quant-voice-panel.lisp)

(defclass tree-voice (voice)
  ((dist :accessor dist :initarg :dist :initform nil)
   (color :accessor color :initarg :color :initform nil)))

(defmethod om::get-object-selection-color ((self t) (in tree-voice))
  (color in)
)

(defclass tree-view (om-view)
  ((tree :accessor tree :initarg :tree :initform nil)
   (gracenotes :accessor gracenotes :initarg :gracenotes :initform nil)
   (dialog :accessor dialog :initarg :dialog :initform nil) ;tree-views are always created inside a dialog window
   (k :accessor k :initarg :k :initform nil))) ;index of the proposition in the list

;Double-click on a solution to select it (returns the index of the solution)
(defmethod om-view-doubleclick-handler ((self tree-view) pos)
  "When a solution is double-clicked, return from dialog its index"
  (om-return-from-modal-dialog (dialog self) (k self))
)

(defmethod om-draw-contents ((self tree-view))
  "Draws each solution in editor mode"
  (when (tree self)
    (let* ((obj (tree self))
           (sizefont 20)
           (om::*internal-score-fonts* (om::init-fonts-to-draw sizefont))
           (om::*draw-mini-pict* t)
           (staffsys (om::get-staff-system 'om::empty))
           (scale om::*current-1/2-scale*)
           (grapobj (om::make-graph-form-obj obj 0 (om::get-miniview-top obj staffsys) (/ sizefont 4) 0 scale nil staffsys t)))
    (when grapobj 
      (om::space-objects grapobj sizefont)
      (om::set-graph-rectangles grapobj)
      (om-with-focused-view self
        (om-with-line-size 1 
          (om-with-font 
           (om::get-font-to-draw 0)
           (om::draw-object-ryth  (car (om::inside (car (om::inside grapobj)))) self 
                             0 -35 0.8 
                             0 (om::w self) 0 (om::h self) 
                             'om::midic sizefont t staffsys nil)

           ;Add colors
           (when (color obj)
             (om::draw-score-selection   
              (car (om::inside (car (om::inside grapobj)))) (list (om::reference (car (om::inside (car (om::inside grapobj)))))) staffsys sizefont))

           ;Draw gracenotes
           (unless (zerop (gracenotes self))
             (om-with-fg-color self *om-gray-color*
                 (om-with-dashline '(2 2) 
                   (om-draw-line (- (om::w self) 60) (- (om::h self) 10)
                                 (- (om::w self) 60) 10))
                          
               (om-draw-string  (- (om::w self) 50) 45  (make-string (gracenotes self) :initial-element #\N))))

           ;Display error
           (om-with-fg-color self (color obj)
             (om-with-font *om-default-font1b*
                           (om-draw-string 10 10 (format nil "Error = ~A%" (dist obj)))))


          )))
      ))))


