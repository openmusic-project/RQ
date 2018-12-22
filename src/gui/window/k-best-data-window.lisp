(in-package :rq)

;;; This file holds all the functions relative to the various parameters dialog boxes.

(defmethod k-best-data-window ((kdata k-best-data))
  "Creates a dialog panel to modify the parameters of one segment only (does not include global parameters)"
  (let ((win (om-make-window 'om-dialog :position :centered 
                             :size (om-make-point 500 270))))

    ;create the panel to edit the data
    (multiple-value-bind (pane tempotxt schematxt n-solution-txt precistxt gracepentxt n-beatstxt)
        (make-k-best-data-panel kdata) 

    ;Add the cancel and OK buttons
    (om-add-subviews pane 
                     (om-make-dialog-item 'om-button (om-make-point (- (om::w pane) 200) (- (om::h pane) 50))
                                          (om-make-point 80 20)
                                          "Cancel"
                                          :di-action (om-dialog-item-act item 
                                                       (om-return-from-modal-dialog win nil)))

                     (om-make-dialog-item  'om-button (om-make-point (- (om::w pane) 100) (- (om::h pane) 50))
                                           (om-make-point 80 20)
                                           "OK"
                                           :di-action (om-dialog-item-act item 
                                                        (let ((tempo (ignore-errors (read-from-string (om-dialog-item-text tempotxt))))
                                                              (n-solution (ignore-errors (read-from-string (om-dialog-item-text n-solution-txt))))
                                                              (schema (ignore-errors (read-from-string (om-dialog-item-text schematxt))))
                                                              (precis (ignore-errors (read-from-string (om-dialog-item-text precistxt))))
                                                              (grace (ignore-errors (read-from-string (om-dialog-item-text gracepentxt))))
                                                              (n-beats (ignore-errors (read-from-string (om-dialog-item-text n-beatstxt)))))
                                                          (setf (tempo kdata) tempo
                                                                (n-solution kdata) n-solution
                                                                (precision kdata) precis
                                                                (gracepen kdata) grace
                                                                (schema kdata) schema
                                                                (n-beats-per-seg kdata) n-beats)
                                                          (setf (updateflag kdata) nil)
                                                          (om-return-from-modal-dialog win t)))))
    (om-add-subviews win pane)
    (om-modal-dialog win))))

(defmethod k-best-data-window-global ((kdata k-best-data) (analysis om::k-best-analysis) (rq rq))
  "Creates a dialog panel to modify the global parameters. Include both segment parameters (to be changed for each segment), and global parameters (segmentation and tempo-smoothing parameters)"
   (let ((win (om-make-window 'om-dialog :position :centered 
                             :size (om-make-point 500 450)))
         (smooth-paramtxt (om-make-dialog-item 'om-editable-text (om-make-point 190 270)  (om-make-point 37 13)
                                               (format nil "~D" (smooth-param rq)) 
                                       :font *om-default-font1*))
         (tempo-jump-pentxt (om-make-dialog-item 'om-editable-text (om-make-point 380 270)  (om-make-point 37 13)
                                                 (format nil "~D" (tempo-jump-pen rq)) 
                                       :font *om-default-font1*))
         segpentxt
         (warning (om::warning-p analysis)))

    ;create the panel to edit the data
    (multiple-value-bind (pane tempotxt schematxt n-solution-txt precistxt gracepentxt n-beatstxt)
        (make-k-best-data-panel kdata  
                                :message  "Set global quantification parameters:"
                                :size (om-make-point 470 420))

      (om-add-subviews pane
                       ;Add global parameters :
                       
                       ;Add segmentation parameters
                       (om-make-dialog-item 'om-static-text  (om-make-point 20 170) (om-make-point 300 20) "Set segmentation parameters:"
                                            :font *om-default-font2b*)

                       (om-make-dialog-item 'om-static-text  (om-make-point 50 200) (om-make-point 190 20) "New-segment penalty"
                                            :font *om-default-font1*)
                       (setf segpentxt (om-make-dialog-item 'om-editable-text (om-make-point 190 200)  (om-make-point 37 13)
                                                            (format nil "~D" (om::segpen analysis)) 
                                                         :font *om-default-font1*))

                       ;Add tempo smoothing parameters
                       (om-make-dialog-item 'om-static-text  (om-make-point 20 240) (om-make-point 300 20) "Set tempo smoothing parameters:"
                                            :font *om-default-font2b*)


                       (om-make-dialog-item 'om-static-text  (om-make-point 50 270) (om-make-point 180 20) "Smoothing (0.0-1.0)"
                                            :font *om-default-font1*)
                       smooth-paramtxt
                       (om-make-dialog-item 'om-static-text  (om-make-point 260 270) (om-make-point 180 20) "Tempo jump penalty"
                                            :font *om-default-font1*)
                       tempo-jump-pentxt

                       ;Add warning parameters
                       (om-make-dialog-item 'om-static-text  (om-make-point 20 310) (om-make-point 300 20) "Set warning parameters:"
                                            :font *om-default-font2b*)

                       (om-make-dialog-item 'om-check-box (om-make-point 50 340) (om-make-point 120 20) "Show warnings "
                                                                   :font *om-default-font1* 
                                                                   :checked-p warning
                                                                   :di-action #'(lambda (b) 
                                                                                  (setf (om::warning-p analysis)  (not (om::warning-p analysis)))
                                                                                  ))
                       
                       

                       ;Add the cancel and OK buttons
                       (om-make-dialog-item 'om-button (om-make-point (- (om::w pane) 200) (- (om::h pane) 50))
                                            (om-make-point 80 20)
                                            "Cancel"
                                            :di-action (om-dialog-item-act item 
                                                         (om-return-from-modal-dialog win nil)))
                       
                       (om-make-dialog-item  'om-button (om-make-point (- (om::w pane) 100) (- (om::h pane) 50))
                                             (om-make-point 80 20)
                                             "OK"
                                             :di-action (om-dialog-item-act item 
                                                          (let ((tempo (ignore-errors (read-from-string (om-dialog-item-text tempotxt))))
                                                                (n-solution (ignore-errors (read-from-string (om-dialog-item-text n-solution-txt))))
                                                                (schema (ignore-errors (read-from-string (om-dialog-item-text schematxt))))
                                                                (precis (ignore-errors (read-from-string (om-dialog-item-text precistxt))))
                                                                (grace (ignore-errors (read-from-string (om-dialog-item-text gracepentxt))))
                                                                (segpen (ignore-errors (read-from-string (om-dialog-item-text segpentxt))))
                                                                (n-beats (ignore-errors (read-from-string (om-dialog-item-text n-beatstxt))))
                                                                (smooth-param (ignore-errors (read-from-string (om-dialog-item-text smooth-paramtxt))))
                                                                (tempo-jump-pen (ignore-errors (read-from-string (om-dialog-item-text tempo-jump-pentxt))))
                                                                )
                                                            (setf (tempo kdata) tempo
                                                                  (n-solution kdata) n-solution
                                                                  (precision kdata) precis
                                                                  (gracepen kdata) grace
                                                                  (schema kdata) schema
                                                                  (n-beats-per-seg kdata) n-beats)
                                                            (setf (om::segpen analysis) segpen
                                                                  )
                                                            (setf (smooth-param rq) smooth-param
                                                                  (tempo-jump-pen rq) tempo-jump-pen)
                                                            (om-return-from-modal-dialog win t))))
                       )
   
      (om-add-subviews win pane)
      (om-modal-dialog win)
)))


(defmethod make-k-best-data-panel ((kdata k-best-data) &key (message nil) (size (om-make-point 470 250)))
  "Creates a panel to edit all the parameters of a `k-best-data'"
  (let ((pane (om-make-view 'om-view
                            :size size
                            :position (om-make-point 10 10)
                            :bg-color *om-white-color*))
        (i 0)
        tempotxt schematxt n-solution-txt precistxt gracepentxt
        (tempomerge (tempomerge kdata))
        (n-beatstxt (om-make-dialog-item 'om-editable-text (om-make-point 380 135)  (om-make-point 37 13)
                                       (format nil "~D" (n-beats-per-seg kdata)) 
                                       :font *om-default-font1*)))
    (om-add-subviews pane
                     (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 16))
                                          (om-make-point 380 40)
                                          (if message 
                                              message
                                            "Set quantification parameters for selected segment:")
                                          :font *om-default-font2b*)
     
                     (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 30)) (om-make-point 120 20) "Tempi"
                                          :font *om-default-font1*)
                     (setf tempotxt (om-make-dialog-item 'om-editable-text (om-make-point 140 i)  (om-make-point 37 13)
                                                         (format nil "~D" (tempo kdata)) 
                                                         :font *om-default-font1*))

                     (om-make-dialog-item 'om-static-text  (om-make-point 260 i) (om-make-point 120 20) "Schema"
                                          :font *om-default-font1*)
                     (setf schematxt (om-make-dialog-item 'om-editable-text (om-make-point 330 i) (om-make-point 97 13)
                                                          (format nil "~D" (schema kdata)) 
                                                          :font *om-default-font1*))
                      
                     (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 26)) (om-make-point 120 20) "Nb. solutions"
                                          :font *om-default-font1*)
                     (setf n-solution-txt (om-make-dialog-item 'om-editable-text (om-make-point 140 i) (om-make-point 37 13)
                                                          (format nil "~D" (n-solution kdata)) 
                                                          :font *om-default-font1*))

                     (om-make-dialog-item 'om-static-text  (om-make-point 260 i) (om-make-point 120 20) "Precision (0.0-1.0)"
                                          :font *om-default-font1*)
                     (setf precistxt (om-make-dialog-item 'om-editable-text (om-make-point 380 i) (om-make-point 37 13)
                                                          (format nil "~D" (precision kdata)) 
                                                          :font *om-default-font1*))

                     (setf tempomergecheck (om-make-dialog-item 'om-check-box (om-make-point 50 (incf i 26)) (om-make-point 120 20) "Merge tempi"
                                                                   :font *om-default-font1* 
                                                                   :checked-p tempomerge
                                                                   :di-action #'(lambda (b) 
                                                                                  (setf (tempomerge kdata)  (not (tempomerge kdata)))
                                                                                  )))

                     (om-make-dialog-item 'om-static-text  (om-make-point 260 i) (om-make-point 120 20) "Grace notes penalty"
                                          :font *om-default-font1*)
                     (setf gracepentxt (om-make-dialog-item 'om-editable-text (om-make-point 380 i) (om-make-point 37 13)
                                                          (format nil "~D" (gracepen kdata)) 
                                                          :font *om-default-font1*))

                     (om-make-dialog-item 'om-check-box (om-make-point 50 135) (om-make-point 140 20) "Segments = beats"
                                                                   :font *om-default-font1* 
                                                                   :checked-p (segments=beats kdata)
                                                                   :di-action #'(lambda (b) 
                                                                                  (setf (segments=beats kdata)  (not (segments=beats kdata)))
                                                                                  (om-enable-dialog-item n-beatstxt (segments=beats kdata))
                                                                                  ))
                       
                     (om-make-dialog-item 'om-static-text  (om-make-point 260 135) (om-make-point 180 20) "Nb. beats/segment"
                                          :font *om-default-font1*)
                     n-beatstxt

                     )
    (om-enable-dialog-item n-beatstxt (segments=beats kdata))
    (values pane tempotxt schematxt n-solution-txt precistxt gracepentxt n-beatstxt tempomergecheck )))


(defmethod set-global-k-best-parameters ((rq rq))
  "This function creates a global-parameters dialog panel, and once closed, updates all the k-best data for each segment"
  (let* ((chord-seq (chord-seq rq))
         (k-best-analysis (get-k-best-analysis chord-seq))
         (segments (analysis-segments k-best-analysis))
         (datas (mapcar #'segment-data segments))
         (new-k-best-data (k-best-data-copy (first datas))))  
    (when (k-best-data-window-global new-k-best-data k-best-analysis rq)
      (loop for data in datas do
            (unless (k-best-data-equal new-k-best-data data)
              (setf (tempo data) (tempo new-k-best-data)
                    (n-solution data) (n-solution new-k-best-data)
                    (schema data) (schema new-k-best-data)
                    (precision data) (precision new-k-best-data)
                    (gracepen data) (gracepen new-k-best-data)
                    (tempomerge data) (tempomerge new-k-best-data))
              (setf (updateflag data) nil)))
      )))


