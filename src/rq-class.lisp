;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            RQ-CLASS.LISP      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This file contains all the functions relative to the RQ class,
; the main class to operate the transcription system.
;
; The main functions are :
; - compute-k-best : to run the algorithm on all segments 
; (also compute-one-k-best to run it on one segment only)
; - make-working-poly : to build the multi-poly object holding all the
; possible transcriptions for each segment
; (also replace-one-poly to recompute one poly only)
; - init-solution : to init a solution from the choices made in the poly panel
;
;


(in-package :rq)

(om::defclass* rq ()
               ((chord-seq :accessor chord-seq :initform nil :initarg :chord-seq :documentation "chord-seq to quantify")
                (marks :accessor marks :initform '(0) :initarg :marks :documentation "list of segmentation marks")
                (solution :accessor solution :initform nil :documentation "the current solution")
                (wp :accessor wp :initform (make-instance 'quant-multi-poly) :documentation "working multi-poly (to browse the automaton)")
                (k-bests :accessor k-bests :initform nil) ;holds all the k-best objects (one per segment)
                (parent :accessor parent :initform nil) ;always nil, added to prevent some bugs
                (editor :accessor editor :initform nil) ;reference to the editor (user interface)
            ;Display parameters
                (display-mode :accessor display-mode :initform :close) ;open or close, depending on wether the right panel is open or not
                (color-flag :accessor color-flag :initform nil) ;true when in "Color mode" (displays the distance with colors)
                (pulse-flag :accessor pulse-flag :initform nil) ;true when "Show pulses" is ticked (to display the quantization grid used)
            ;Tempo-smoothing parameters
                (smooth-param :accessor smooth-param :initform 0.5) ;to balance between the smoothness of tempo and the weight of the solutions
                (tempo-jump-pen :accessor tempo-jump-pen :initform 5) ; to penalize tempo jumps
                )
               (:icon 252)
               (:documentation "
RQ - Rhythm Quantification


Inputs :
<chord-seq> : a chord-seq object to quantify
<marks>     : a list of instants of segmentation (in milliseconds)

Double-click to open the quantification interface.

To retrieve the quantified output, use the function get-voice

For instructions, tutorials and information : http://repmus.ircam.fr/cao/rq

"))


(defmethod initialize-instance :after ((self rq) &rest args)
  "Constructor for a `rq'."
  ;; Initializing the input CHORD-SEQ and making it a `quant-chord-seq':
  (if (chord-seq self)
      (setf (chord-seq self) (chord-seq->quant-chord-seq (chord-seq self)))
    (setf (chord-seq self) (make-instance 'quant-chord-seq)))
  ;; PARENT
  (setf (parent (chord-seq self)) self)
  ;; Creating a new `k-best-analysis', with segment defined by the input MARKS
  ;; Do nothing if there already is a k-best analysis in the input chord-seq
  (unless (get-k-best-analysis (chord-seq self))
    (if (marks self)
        (om::add-object-analysis (chord-seq self)
                             (make-instance 'om::k-best-analysis :analysis-segments
                                            (loop for mark in (marks self)
                                                  when (>= mark 0)
                                                  collect (make-instance 'om::marker-segment :mrk-time mark))))
      (om::add-object-analysis (chord-seq self) (make-instance 'om::k-best-analysis))))
  ;; SOLUTION
  (setf (solution self) (make-instance 'quant-voice :tree '(1 (((4 4) (-1))))))
  (setf (rq (solution self)) self)
 )


(defmethod compute-k-best ((self rq) &optional (warning t warning-supplied-p))
  "Runs the `k-best-analysis'"
  (let* ((k-best-analysis (get-k-best-analysis (chord-seq self)))
         (old-warning (if (om::warning-p k-best-analysis) t nil)));a trick to get a copy of (warning-p k-best-abalysis)
    ;Small hack : when warning is supplied, you want the analysis to use this argument.
    ;To do so, you set the value of (warning-p (editor self)) as you want, and you set it back as it was before afterwards
    (when warning-supplied-p 
      (setf (om::warning-p k-best-analysis) warning))
    (setf (k-bests self) (loop for segment in (analysis-segments k-best-analysis)
                               collect (om::analyse-one-segment k-best-analysis segment (chord-seq self))))
    (setf (k-bests (solution self)) (k-bests self))
    (setf (rq (solution self)) self)
    (when warning-supplied-p 
      (setf (om::warning-p k-best-analysis) old-warning))))


(defmethod get-k-best-analysis ((self chord-seq))
  (car (remove nil (loop for an in (om::analysis self) 
                    when (equal (type-of an) 'om::k-best-analysis)
                    collect an))))

;;; Output functions:

(defmethod get-chord-seq ((self rq))
  (chord-seq self))


(om::defmethod! get-voice ((rq rq) &optional (render t))
                :initvals '(nil t)
                :indoc '("rq" "render")
                :icon '(252)
                :doc  "Returns a voice corresponding to the chosen transcription in the rq"

  (make-instance 'voice
                 :tree (update-tree  (solution rq) render)
                 :chords (get-chords (chord-seq rq))
                 :tempo (get-tempo (solution rq))))


(om::defmethod! get-k-best-list ((self rq) &optional poly-indexes voice-indexes)
                :initvals '(nil nil nil)
                :indoc '("rq" "indexes of polys" "indexes of the voices")
                :icon '(252)
                :doc "Returns a list of transcriptions in the form of a list of polys/a poly.
If no optional arguments are provided, the function returns a list of all the polys in the k-best panel (one for each segment)

If the argument <poly-indexes> is a number, the function returns the corresponding poly.
If the argument <poly-indexes> is a list, the function returns a list of polys corresponding to the indexes provided.

If the argument <voices-indexes> is a number, the function returns the corresponding voice (a list of voices when <poly-indexes> is a list).
If the argument <voices-indexes> is a list, the function returns a poly containing only the corresponding voices (a list of polys when <poly-indexes> is a list).
If the argument <voices-indexes> is a list of lists, the function returns a list of poly containing the corresponding voices for each sublist. When a sublist is nil, all the voices are included.
"

(let ((polys (polys (wp self))))                 
  (cond ((null poly-indexes)
         polys)

        ((numberp poly-indexes)
         (cond ((null voice-indexes)
                (nth poly-indexes polys))

               ((numberp voice-indexes)
                (nth voice-indexes (om::voices (nth poly-indexes polys))))

               ((and (listp voice-indexes) (numberp (car voice-indexes)))
                (let ((current-poly (nth poly-indexes polys)))
                  (make-instance 'poly :voices (loop for index in voice-indexes collect
                                                     (nth index (om::voices current-poly))))))
               ((and (listp voice-indexes) (listp (car voice-indexes)))
                (let ((current-poly (nth poly-indexes polys)))
                  (make-instance 'poly :voices (loop for index in (car voice-indexes) collect
                                                     (nth index (om::voices current-poly))))))
               (t nil)
               ))

        ((listp poly-indexes)
         (cond ((null voice-indexes)
                (loop for index in poly-indexes collect (nth index polys)))

               ((numberp voice-indexes)
                (loop for index in poly-indexes collect (nth voice-indexes (om::voices (nth index polys)))))

               ((and (listp voice-indexes) (numberp (car voice-indexes)))
                (loop for poly-index in poly-indexes collect
                      (let ((current-poly (nth poly-index polys)))
                        (make-instance 'poly :voices (loop for index in voice-indexes collect
                                                           (nth index (om::voices current-poly)))))))

               ((and (listp voice-indexes) (listp (car voice-indexes)))
                (loop for poly-index in poly-indexes 
                      for lvoice-index in voice-indexes collect
                      (let ((current-poly (nth poly-index polys)))
                        (if lvoice-index
                            (make-instance 'poly :voices (loop for index in lvoice-index collect
                                                               (nth index (om::voices current-poly))))
                          current-poly)))))))                
))

;;; Computation functions:

(defmethod compute-one-k-best ((self rq) (segment om::segment))
  "Computes a k-best structure for one segment. Works only if the k-bests already have been computed once."
  (let* ((k-best-analysis (get-k-best-analysis (chord-seq self)))
         (k-best-segments (analysis-segments k-best-analysis))
         (segment-index (position segment k-best-segments)))
    (setf (nth segment-index (k-bests self)) (om::analyse-one-segment k-best-analysis segment (chord-seq self)))
    (setf (k-bests (solution self)) (k-bests self))
    (setf (rq (solution self)) self)
    ))

(defmethod init-solution ((self rq))
  "Initializes the solution from the chosen transcriptions."
  (let ((mp (wp self)))
    (update-ks-indexes (solution self) mp)
    (setf (rq (solution self)) self)
    (setf (slurs (solution self)) (get-slurs self))
    (init-all-choices (solution self))
    (set-tree (solution self) (update-tree (solution self) (not (editflag (solution self)))))))

;;
(defmethod get-list-n-solution ((self rq))
  (mapcar #'(lambda (segment) (n-solution (segment-data segment))) (get-k-best-segments self)))

;;
(defmethod update-ks-indexes ((voice quant-voice) (wp quant-multi-poly))
  "Updates in the voice the indexes and ks of the chosen transcriptions"
  (let* ((selections (selections wp))
         (index-k (get-indexes-of-solutions (rq voice) selections)))
    (setf (indexes voice) (mapcar #'first index-k))
    (setf (ks voice) (mapcar #'second index-k))))



;;
(defmethod get-k-best-segments ((self rq))
  (analysis-segments (get-k-best-analysis (chord-seq self))))

(defmethod get-k-best-analysis ((self rq))
  (get-k-best-analysis (chord-seq self)))

;;
(defmethod get-indexes-of-solutions ((self rq) selections)
  "Computes for each segment the indexes (in the list of inputs) 
and ks (for the corresponding input) of the chosen transcriptions"
  (let ((list-n-solution (get-list-n-solution self)))
    (loop for select in selections
          for n-solution in list-n-solution
          collect (list (floor select n-solution) (1+ (rem select n-solution))))))

;;
(defmethod get-slurs ((self rq))
  (mapcar #'(lambda (segment) (slur (segment-data segment))) (get-k-best-segments self)))


(defmethod get-smoothed-selections ((self rq))
  "To run the tempo smoothing algorithm.
Returns a list of selections (one for each segment, corresponds to the index of the selected transcription)"
  (let* ((wp (wp self))
         (polys (polys wp))
         (k-bests (k-bests self))
         (ltempi nil)
         (lweights nil)
         (lks nil)
         (indexes)
         )

    (loop for k-best in k-bests
          do
          (multiple-value-bind (tempi weights ks) (get-tempi-weigths-ks k-best)
            (setq ltempi (append ltempi (list tempi))
                  lweights (append lweights (list weights))
                  lks    (append lks (list ks)))))

    (setq indexes (ts::tempo-smooth ltempi lweights 
                                    :alpha (smooth-param self)
                                    :penalty (tempo-jump-pen self)))
          
    (loop for index in indexes 
          for ks in lks 
          collect (nth index ks)) 
  
))



;------ Poly functions ------;

;;; To build a poly from the automaton:
;;
(defmethod make-working-poly ((self rq))
  "Make the working poly with all the transcriptions for all segments from the k-bests structures"
  (let* ((k-bests (k-bests self))
         (segment-chords (get-k-best-segment-chords (chord-seq self)))
         (wp (make-instance 
              'quant-multi-poly
              :current-poly 0)))
    (setf (polys wp) (loop for k-best in k-bests 
                                  for chords in segment-chords collect
                                  (make-one-poly k-best chords)))
    ;(setf (selections wp) (get-smoothed-selections self))
    (setf (selections wp) (make-list (length k-bests) :initial-element 0))
    (setf (distances wp)  (get-distances k-bests))
    (setf (gracenotes wp) (mapcar #'(lambda (x) (reduce #'append x)) (mapcar #'gracenotes k-bests )))
    (setf (list-n-solution wp) (get-list-n-solution self)) wp))

;;
(defmethod replace-one-poly ((self rq) index)
  "Recomputes one single poly (works only if there is already an existing working poly)"
  (let* ((k-bests (k-bests self))
         (k-best (nth index k-bests))
         (segment (nth index (get-k-best-segments self)))
         (wp (wp self))
         (chords (get-segment-chords (chord-seq self) segment)))
    (setf (nth index (polys wp)) (make-one-poly k-best chords))
    (setf (nth index (selections wp)) 0)
    (setf (distances wp)  (get-distances k-bests))
    (setf (gracenotes wp) (mapcar #'(lambda (x) (reduce #'append x)) (mapcar #'gracenotes k-bests)))
    (setf (list-n-solution wp) (get-list-n-solution self))))

;;
(defmethod make-one-poly (k-best chords)
  (let* ((ktables (ktables k-best))
         (inputs (inputs k-best))
         (slur nil)
         (n-solution (n-solution k-best)) 
         (gracenotes (make-list (length inputs) :initial-element (make-list n-solution :initial-element 0))))
    (k-best-create-poly  ktables inputs n-solution slur gracenotes (tempomerge k-best) chords)))

;;;Misc.
;;
(defmethod k-best-analysis-update-p ((self rq))
  "TRUE iff at least one segment is not up to date (ie if we have to quantify again)"
  (let ((k-best-analysis (get-k-best-analysis (chord-seq self))))
    (find-if #'(lambda (segment) (null (updateflag (segment-data segment)))) (analysis-segments k-best-analysis))))

(defmethod distance-to-color (dist)
  "Converts a distance to a RGB color ranging from red (DIST>30) to green (DIST<5)."
  (cond ((null dist)  (om-make-color 0    0    0))
        ((< dist 0)   (om-make-color 0    0    0))
        ((> dist 30)  (om-make-color 1    0    0))
        ((> dist 25)  (om-make-color 1    0.33 0))
        ((> dist 20)  (om-make-color 1    0.67 0))
        ((> dist 15)  (om-make-color 0.90 0.90 0.2))
        ((> dist 10)  (om-make-color 0.66 1    0))
        ((> dist 5)   (om-make-color 0.33 1    0))
        (t            (om-make-color 0    1    0))))

;;; SAVE

(in-package :om)

(defmethod om::omng-save ((self rq::rq) &optional values?)
  `(let ((qs, (call-next-method)))
     
     (setf (rq::color-flag qs), (rq::color-flag self))
     (setf (rq::pulse-flag qs), (rq::pulse-flag self))
     (setf (rq::display-mode qs), (rq::display-mode self))
     
     ;K-bests
     ;Last argument = do not show warnings when loading a patch
     (rq::compute-k-best qs nil)
     ;WP
     (setf (rq::wp qs) (rq::make-working-poly qs))
     (setf (rq::current-poly (rq::wp qs)) ,(rq::current-poly (rq::wp self))
           (rq::selections (rq::wp qs)) ',(rq::selections (rq::wp self)))
     ;Solution
     (setf (rq::solution qs), (om::omng-save (rq::solution self)))
     (setf (rq::rq (rq::solution qs)) qs)
     (setf (rq::k-bests (rq::solution qs)) (rq::k-bests qs))
     (initialize-instance (rq::solution qs)
                         :tree (tree (rq::solution qs)) :chords (rq::get-chords (rq::chord-seq qs))
                         :tempo (rq::get-tempo (rq::wp qs)))
     (rq::update-grids (rq::solution qs))

     qs))














