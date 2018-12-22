;;;;

(in-package :om)



;;; Segments = time marker, until next one
(defclass! k-best-analysis (abstract-analysis) 
           ((segpen :accessor segpen :initarg :segpen :initform 5) ;penalty given to the creation of a new segment in the segmentation algorithm
            (warning-p :accessor warning-p :initarg :warning-p :initform t))); to display the long segment warning, or not. 

;A k-best analysis is only allowed with a quand-chod-seq (ie in the context of a RQ system)
(defmethod compatible-analysis-p ((analyse k-best-analysis) (object rq::quant-chord-seq)) t)
(defmethod compatible-analysis-p ((analyse k-best-analysis) (object t)) nil)

(defmethod default-segment-class ((self k-best-analysis)) 'marker-segment)

(defmethod compute-segments-p ((self k-best-analysis)) t)
(defmethod analyse-segments-p ((self k-best-analysis)) t)
(defmethod compute+analyse-segments-p ((self k-best-analysis)) nil)

(defmethod analysis-init ((self k-best-analysis) object)
  (unless (analysis-segments self)
    (setf (analysis-segments self)
          (list (make-instance 'marker-segment :mrk-time 0))))
  (call-next-method))

(defmethod analysis-init-segment ((self k-best-analysis) segment)
    (unless (segment-data segment)
        (setf (segment-data segment) (make-instance 'rq::k-best-data)))
  (when (previous-segment segment)
    (progn
      (setf (rq::updateflag (segment-data (previous-segment segment))) nil)
      (compute-and-set-slur self segment))))
 
(defmethod delete-from-analysis ((self k-best-analysis) segment)
  (when (previous-segment segment)
    (setf (rq::updateflag (segment-data (previous-segment segment))) nil))
  (call-next-method))

(defmethod compute-analysis-segments ((self k-best-analysis) (object chord-seq))
  "Computes the segmentation marks using a constant tatum segmentation algorithm"
  (let ((lonsets (lonset object))
        marks)  
    (setq marks (cts::segmentation lonsets (segpen self)))
    (loop for mark in marks collect
          (make-instance 'marker-segment :mrk-time mark))
))

(defmethod analyse-one-segment ((self k-best-analysis) (segment segment) (object t))
  "Runs the analysis on one segment. Computes a k-best structure."
  (let* ((temp-chord-seq (select object (segment-begin segment) (min (segment-end segment) (get-obj-dur object))))
         (onsets (print (lonset temp-chord-seq)))
         (durs (print (ldur temp-chord-seq)))
         (k-best-data (or (segment-data segment)
                       (setf (segment-data segment) (make-instance 'rq::k-best-data))))
         (schema (or (rq::schema k-best-data) '(( ((2 3) (2 3) 2) (5 (2 3) 2) ((7 11 13))))))
         (tempo (rq::tempo k-best-data))
         (tempomerge (rq::tempomerge k-best-data))
         (warning (warning-p self))
         out)
    ;Warning when the segment is long (computations may take a long time)
    (when (or (< (- (segment-end segment) (segment-begin segment)) 5000) (if warning (om-y-or-n-dialog (format nil "The segment starting at ~d, ending at ~d seems long, computations might take a long time. Continue anyway ?" (segment-begin segment) (segment-end segment))) t))

          ;If each segment corresponds to a beat, we compute the tempo value from the segment length
      (if (rq::segments=beats k-best-data)
          (let ((tempo (round (/ (* 60000 (rq::n-beats-per-seg k-best-data)) (- (segment-end segment) (segment-begin segment))))))
            (setf out (rq::k-best-segment-quantify-automaton-with-tempo onsets durs (rq::n-solution k-best-data) schema (- (segment-end segment) (segment-begin segment)) tempo (rq::precision k-best-data) (rq::gracepen k-best-data) (rq::slur k-best-data))))

        ;Otherwise, use the tempo parameter
             ;No tempo specified, try tempi between 40 and 200 bpm
        (cond ((null tempo) 
               (if tempomerge
                 ;Solutions ranked across all tempi (one ktable for all)
                   (setf out (rq::k-best-segment-quantify-automaton onsets durs (rq::n-solution k-best-data) schema (- (segment-end segment) (segment-begin segment)) (rq::precision k-best-data) (rq::gracepen k-best-data)  (rq::slur k-best-data)))
               ;Solutions ranked independently for each tempo (one ktable per tempo)
                 (setf out (rq::k-best-segment-quantify-automatons onsets durs (rq::n-solution k-best-data) schema (- (segment-end segment) (segment-begin segment)) (rq::precision k-best-data) (rq::gracepen k-best-data)  (rq::slur k-best-data)))))
            ;One single tempo specified
              ((numberp tempo)  
               (setf out (rq::k-best-segment-quantify-automaton-with-tempo onsets durs (rq::n-solution k-best-data) schema (- (segment-end segment) (segment-begin segment)) (rq::tempo k-best-data) (rq::precision k-best-data) (rq::gracepen k-best-data) (rq::slur k-best-data))))
            ;A range of tempi (in the form of a 2 elements list) is specified
              (t 
               (if tempomerge
                 ;Solutions ranked across all tempi (one ktable for all)
                   (setf out (rq::k-best-segment-quantify-automaton onsets durs (rq::n-solution k-best-data) schema (- (segment-end segment) (segment-begin segment)) (rq::precision k-best-data) (rq::gracepen k-best-data)  (rq::slur k-best-data) (first tempo) (second tempo)))
               ;Solutions ranked independently for each tempo (one ktable per tempo)
                 (setf out (rq::k-best-segment-quantify-automatons onsets durs (rq::n-solution k-best-data) schema (- (segment-end segment) (segment-begin segment)) (rq::precision k-best-data) (rq::gracepen k-best-data) (rq::slur k-best-data) (first tempo) (second tempo)))))))
        (setf (rq::updateflag k-best-data) t)
      out)))

(defmethod handle-segment-doubleclick ((self k-best-analysis) segment panel pos) 
  (rq::k-best-data-window  (or (segment-data segment) 
                               (setf (segment-data segment) (make-instance 'rq::k-best-data))))
  (update-panel panel))

(defmethod draw-segment-data ((self k-best-analysis) segment view) 
  (when (segment-data segment)
    (let ((x1 (time-to-pixels view (segment-begin segment)))
          (x2 (time-to-pixels view (segment-end segment)))
          (qs (rq::parent (analysis-object self)))
          (segment-index  (position  segment (analysis-segments self) :test 'equalp))
          (color (if (rq::updateflag (segment-data segment))
                     (color segment) 
                   *om-gray-color*)))

      ;Display k-best parameters
      (om-with-fg-color view
          color
        (om-with-font *om-default-font1*
                      (om-draw-string x1 (- (h view) 115) (format nil "t=~D" (segment-begin segment)))
                      (om-draw-string x1 (- (h view) 105) (format nil "Slur=~D" (rq::slur (segment-data segment))))

                      (om-draw-string x1 (- (h view) 85) (format nil "Tempo: ~A" (rq::tempo (segment-data segment))))
                      (om-draw-string x1 (- (h view) 75) (format nil "Nb. solutions : ~A" (rq::n-solution (segment-data segment))))
                      (om-draw-string x1 (- (h view) 65) (format nil "Precision: ~A" (rq::precision (segment-data segment))))
                      (om-draw-string x1 (- (h view) 55)  (format nil "Grace pen. : ~A" (rq::gracepen (segment-data segment))))

                      ;DISPLAY THE GRID
                      (if (and (numberp (rq::tempo (segment-data segment)))
                               (equal (type-of rq::*clicked-panel*) 'rq::quant-chord-seq-panel))
                          
                          ;When a tempo is specified, and the chord-seq panel is the last clicked, display the beats corresponding to the selected tempo.
                          ;Otherwise, display the grid
                          (let (tempo)
                            (if (numberp (rq::tempo (segment-data segment)))
                                (setq tempo (rq::tempo (segment-data segment)))
                              (let* ((mp (rq::wp qs))
                                     (current-poly  (nth segment-index  (rq::polys mp))))
                                (when current-poly
                                  (let ((select-voice (nth (nth segment-index (rq::selections mp)) (voices current-poly))))
                                    (setq tempo (second (first (tempo select-voice))))))))
                            ;Draw the beats
                            (let* ((segment-dur (- (segment-end segment) (segment-begin segment)))
                                   (temps (* (/ 60 tempo) 1000))
                                   (nb-temps (ceiling segment-dur temps)))
                              
                              (om-draw-line x1 (- (h view) 130) x2 (- (h view) 130))
                              (loop for i from 1 to nb-temps
                                    for time = (segment-begin segment) then (+ time temps) do
                                    (om-draw-string (time-to-pixels view time) (- (h view) 130) (format nil "~A" i))
                                    (om-with-dashline '(2 2) 
                                      (om-draw-line (time-to-pixels view time) (- (h view) 130) 
                                                    (time-to-pixels view time) 0)))))

                        ;To draw the recursive grid corresponding to the current solution
                        ;Also highlights the selected segment.
                        (when (rq::pulse-flag (rq::chord-seq qs))
                          (let ((segment-dur (- (segment-end segment) (segment-begin segment)))
                                (grid (rq::grid (segment-data segment)))
                                (init-depth 1)
                                (select (selection? (panel (rq::voice-editor (rq::editor qs))))))
                            (when (= (length grid) 1)
                              (setq grid (car grid) ;we remove top level of grid when there is only one beat in the subdivision so that the grid matches the displayed rectangle
                                    init-depth 2)) ;we compensate the top level in the grid
                            (labels ((draw-grid (grid path &optional (depth 1) (begin (segment-begin segment)) (dur segment-dur) (current-path '(0)))
                                       (if (listp grid)
                                           (let ((n (length grid)))
                                             (loop for i from 0 to (1- n)
                                                   for time = begin then (+ time (round dur n))
                                                   do
                                                   (progn
                                                     (if (= depth 1)
                                                         (progn
                                                           (om-with-dashline '(2 2) 
                                                             (om-draw-line (time-to-pixels view time) (- (h view) 130) 
                                                                           (time-to-pixels view time) 0))
                                                           (om-draw-string (time-to-pixels view time) (- (h view) 130) (format nil "~A" (1+ i))))
                                                       (unless (= time begin)
                                                         (om-with-dashline '(2 2) 
                                                           (om-draw-line (time-to-pixels view time) (- (h view) 130) 
                                                                         (time-to-pixels view time)  (+ (- (h view) 190) (* 10 (min depth 5)) )))))
                                                     (draw-grid (nth i grid) path (1+ depth) time (round dur n) (append current-path (list i)) )))))
                                       (when (and path (equalp path current-path) (equalp (rq::voice-mode (rq::editor qs)) :edit)) 
                                          (draw-h-rectangle (list (time-to-pixels view begin) 20 (+ (time-to-pixels view (+ begin dur)) 1) 95) :fill t :color *om-black-color*))
                                       ))
                              (if (or (null select) (not (= segment-index (or (car (rq::get-cons-list (car select))) 0))))
                                  (draw-grid grid nil init-depth)
                                (draw-grid grid (cdr (rq::get-cons-list (car select))) init-depth))
                              ))))))
      ;Display "Merge tempi" when the tempomerge option is selected
      (when (rq::tempomerge (segment-data segment))
        (om-with-fg-color view color
          (om-with-font *om-default-font1b*
                        (om-draw-string x1 (- (h view) 45)  "Merge tempi"))))
       ;Highlight the segment correcponding to the current poly
       (when (= segment-index (rq::current-poly (rq::wp qs)))
         (draw-h-rectangle (list x1 20 x2 95) :fill t :color color)))))

(defmethod shorten-string (string &optional (n-caracters 5))
  "A function to shorten the long strings"
  (if (> (length string) n-caracters)
      (concatenate 'string (subseq string 0 (1+ n-caracters)) "...")
    string)
)

(defmethod analysis-key-event ((self k-best-analysis) panel char)
  "Key handler : up-down to change tempo, left-right to change position"
  (case char
    (:om-key-up (selected-segment-change-tempo self 1)) 
    (:om-key-down (selected-segment-change-tempo self -1))
    (:om-key-left (selected-segment-change-mrk-time self -5))
    (:om-key-right (selected-segment-change-mrk-time self 5))
    (otherwise (call-next-method))))
    

(defmethod compute-and-set-slur ((analyse k-best-analysis) segment)
  "Computes and sets the slur for a segment (the amount by which the last chord of the previous segment overlaps the segmentation mark)
When the segment is placed on a chord, the slur is nil (even if a previous chord overlaps)"
  (let (previous-begin 
        previous-end)
    (if (previous-segment segment)
        (setq previous-begin (segment-begin (previous-segment segment))
              previous-end (segment-end (previous-segment segment)))
      (setq previous-begin 0
            previous-end (segment-begin segment)))
     
    (let* ((chords (analysis-object analyse))
           (onsets (lonset chords))
           (durs (ldur chords))
           (on-dur-previous (loop for on in onsets for dur in durs when (and (>= on previous-begin)  (< on  previous-end)) collect (list on dur) ))
           (first-onset (find-if  #'(lambda (x) (>= x previous-end)) onsets )))

      (if (equalp first-onset previous-end)
          (setf (rq::slur (segment-data segment)) nil)
        (if  on-dur-previous
            (let ((overlap (- (+ (caar (last on-dur-previous)) (car (second (car (last on-dur-previous))))) (segment-begin segment))))
              (if (> overlap 0) ;when the last note of the previous segment ends in the current segment
                  (setf (rq::slur (segment-data segment)) overlap)
                (setf (rq::slur (segment-data segment)) nil)))
          (setf (rq::slur (segment-data segment)) nil))))))

(defmethod selected-segment-change-tempo ((self k-best-analysis) delta)
  "Adds DELTA to the tempo of the selected segments"
  (when (selected-segments self)    
    (loop for segment in (selected-segments self) do
          ;Only when the tempo is a number
          (unless (listp (rq::tempo (segment-data segment)))
            (setf (rq::tempo (segment-data segment)) (+ (rq::tempo (segment-data segment)) delta))
            (setf (rq::updateflag (segment-data segment)) nil)))))

(defmethod selected-segment-change-mrk-time ((self k-best-analysis) delta)
  "Moves the segmentation mark of DELTA ms"
  (when (selected-segments self)
    (loop for segment in (selected-segments self) do 
          (progn
            (setf (mrk-time  segment) (max (+ (mrk-time  segment) delta) 0))
            (compute-and-set-slur self segment)
            (setf (rq::updateflag (segment-data segment)) nil)
            (when (previous-segment segment)
              (setf (rq::updateflag (segment-data (previous-segment segment))) nil))))))

	
