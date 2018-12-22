;;;; Test code chunks found in other source files.

;;; From quant-system.lisp:
(defun test (&optional (n nil))
  (if n
      (print (format nil "test ~d" n))
    (print "test")))

;;; From k-best-analysis.lisp:
(defmethod compute-analysis-segments-test (entry) 
      (let* ((lonsets entry)
             (ldurs (x->dx lonsets))
             (nb-chords (length lonsets))
             (marks nil))
        ;onsets contient les onsets à l'intérieur du segment
        (labels ((estim-tempo ( dur-segment durs on-segment onsets &optional  (ltempo nil))
                   (if (null durs)
                       (let* ((tempo  (if (null ltempo) (to-tempo dur-segment) (average ltempo nil))))
                         (setq marks (append marks (list (make-instance 'marker-segment
                                                                        :mrk-time (first on-segment) 
                                                                        :color (om-random-color)
                                                                        :segment-data (make-instance 'k-best-data :tempo tempo))))))
                   (let ((new-tempo (to-tempo dur-segment)))
                     (if (null ltempo)
                         ;si début du segment
                         (estim-tempo (append dur-segment (list (first durs))) (rest durs) (append on-segment (list (first onsets))) (rest onsets) (list new-tempo))
                       (let ((old-tempo (average ltempo nil)))
                         (if (> (/ (abs (- new-tempo old-tempo)) old-tempo) 0.1)
                           ;on découpe
                             (progn 
                               (setq marks (append marks (list (make-instance 'marker-segment
                                                                  :mrk-time (first on-segment)
                                                                  :color (om-random-color)
                                                                  :segment-data (make-instance 'k-best-data :tempo old-tempo)))))
                               (estim-tempo (list (car (last dur-segment)) (first durs)) (cdr durs) (list (car (last on-segment)) (first onsets)) (cdr onsets)))
                         ;on continue
                           (estim-tempo (append dur-segment (list (first durs))) (rest durs) (append on-segment (list (first onsets))) (rest onsets)  (append ltempo (list new-tempo)))))))))
                 (compute-error (dur-segment gcd &optional (error 0))
                   (if (null dur-segment)
                       error
                     (compute-error (rest dur-segment) gcd (+ error (abs (nth-value 1 (round (first dur-segment) gcd)))))))
                 (to-tempo (list)
                   (let* ((P1 (agcd list 0.05))
                          (P2 (agcd (mapcar #'(lambda (x) (/ x P1)) list) 0.2))
                          (P (* P1 (round P2)))
                          (tempo (/ 60000 P)))

                     (loop while (> tempo 300)
                           do (setq tempo (/ tempo 2)))
                     tempo)))
          (estim-tempo (subseq ldurs 0 2) (cddr ldurs) (subseq lonsets 0 2) (cddr lonsets)))))


;(compute-analysis-segments-test '(0 110 230 500 600 700 820))
;(compute-analysis-segments-test '(1619 2207 2430 2670 2875 3134 3628 4129 4614 5043 5200 5466 5693 5926 6182 6695 7195 7616 7753 8222 8447 8714 8942 9213 9700 9950 10195 10413 10672 11162 11393 11655 11876 12095 12212 12652 12888 13121 13364 13648 14148 14372 14599 14830 15097 15571 16072 16577 16976 17216 17446 17700 17942 18216 18697 18949 19203 19426 19605 19747 20202 20433 20686 20920 21180 21645 21885 22129 22368 22646 23108 23359 23609 23858 24169 25244 25976))

(defun draw-grid-test (grid &optional (depth 1) (begin 0) (dur 1000) (current-path nil))
  (if (listp grid)
      (let ((n (length grid)))
        (loop for i from 0 to (1- n)
              for time = begin then (+ time (round dur n))
              do
              (progn
                (if (= depth 1)
                    (format nil "level 1  : ~d"  time)
                  (unless (= time begin)
                    (format nil "level ~d  : ~d" depth time)))
                (draw-grid-test (nth i grid) (1+ depth) time (round dur n) (append current-path (list i))))))
    (print current-path)))


;(draw-grid-test '((1/4 (1/8 1/8)) 1/2))

;;; From rq.lisp:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;   TESTS    ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(defun keypath (list schema &optional (path (path-empty *input2*)))
;  (if (null list)
;      path
;    (keypath (cdr list) (cdr schema) (path-addi path (car schema) (car list)))
;))

;(defun get-entry (list schema)
;  (gethash (keypath list schema))
;)


;(defun test (k flag)
;  (let* ((input (input-make ;'(( ((2 3) (2 3) 2) (5 (2 3) 2)))
;                            '(2 2 2)
;                            #(0.0 0.45 0.6 0.9 1) 
;                            #(1 1 1 1 -1)))
;         (ktable (init (transitions input)))
;         (p (path-empty input)))

;    (values (rq-best ktable input p k flag) (rq-to-omtree ktable input k flag))

;    )
;)

;(test 2 nil)



;;; From subdivision.lisp:
;; tests

(setq *testschema1* '((((2 5) 2 3) (2 3 2) (3 2 2))) )
(setq *testschema2* '(((2 2 2) (3 (2 3) (2 3)))) )
(setq *testschema3* '((2 2) 3 4)) ;schéma mauvais : "2 ou 2, puis 3, puis 4"



;;; From kbest-segment-quantification.lisp:
;;;===========
;;;   tests
;;;===========

;(kbest-segment-cut-measures '(500 2500 5500) '(1000 2000 2000) '(2 4) 60 6750 250)



;;Seg 1
;(0 494 995 1480 1884)
;((485) (214) (168) (404))
;10
;((((2 3) (2 3) 2) (5 (2 3) 2) ((7 11 13))))
;(3 4)
;120.645455
;1490
;0.5
;nil
;2


;;Seg 2
;(419 576 842 1069 1302 1563)
;((104 55) (310) (302) (254) (261))
;10
;((((2 3) (2 3) 2) (5 (2 3) 2) ((7 11 13))))
;(3 4);;;===========
;;;   tests
;;;===========

;(kbest-segment-cut-measures '(500 2500 5500) '(1000 2000 2000) '(2 4) 60 6750 250)



;;Seg 1
;(0 494 995 1480 1884)
;((485) (214) (168) (404))
;10
;((((2 3) (2 3) 2) (5 (2 3) 2) ((7 11 13))))
;(3 4)
;120.645455
;1490
;0.5
;nil
;2


;;Seg 2
;(419 576 842 1069 1302 1563)
;((104 55) (310) (302) (254) (261))
;10
;((((2 3) (2 3) 2) (5 (2 3) 2) ((7 11 13))))
;(3 4)
;108.86856
;1557
;0.5
;nil
;394


;;Seg chelou
;OM > (26 500 1001 1143)
;OM > ((448) (157) (142))
;OM > 10
;OM > ((((2 3) (2 3) 2) (5 (2 3) 2) ((7 11 13))))
;OM > (3 4)
;OM > 119.121445
;OM > 1495
;OM > 0.5
;OM > nil
;OM > 23



;(kbest-segment-quantify '(0 494 995 1480 1884) '((485) (214) (168) (404)) 10 '((((2 3) (2 3) 2) (5 (2 3) 2) ((7 11 13)))) '(3 4) 120.645455 1490 0.5 nil 2)

;(kbest-segment-quantify '(419 576 842 1069 1302 1563) '((104 55) (310) (302) (254) (261)) 10 '((((2 3) (2 3) 2) (5 (2 3) 2) ((7 11 13)))) '(3 4) 108.86856 1557 0.5 nil 394)

;(kbest-segment-quantify '(26 500 1001 1143) '((448) (157) (142)) 10 '((((2 3) (2 3) 2) (5 (2 3) 2) ((7 11 13)))) '(3 4) 119.121445 1495 '0.5 nil 23)

;(kbest-segment-quantify-automata   '(0 494 995 1480 1884) '((485) (214) (168) (404)) 10 '((((2 3) (2 3) 2) (5 (2 3) 2) ((7 11 13))))  1490 0.5 2)

;(kbest-segment-quantify-automata   '(0 806 1625 2417 3208 4208) '((1000) (1000) (1000) (1000) (1000)) 10 '((((2 3) (2 3) 2) (5 (2 3) 2) ((7 11 13))))  4208 0.5 2)


;(kbest-to-poly (kbest-segment-quantify-automatas    '(0) nil 1 '((((2 3) (2 3) 2) (5 (2 3) 2) ((7 11 13))))  3278 0 991))




;(defun test ()

;  (let* ((kbest (kbest-segment-quantify-automatas '(0 494 995 1480 1884) '((485) (214) (168) (404)) 10 '((((2 3) (2 3) 2) (5 (2 3) 2) ((7 11 13))))  2590 0.5 2))
;         (ktable (first (ktables kbest)))
;         (input (first (inputs kbest))))

    
;    (rq-to-omtree ktable input 1 nil 0 (path-addi (path-empty input) 2 1))

;    (let ((qv  (make-instance 'om::quant-voice :k-bests kbest)))

;      (om::init-choices qv)
    


;)))

;(test)
;108.86856
;1557
;0.5
;nil
;394


;;Seg chelou
;OM > (26 500 1001 1143)
;OM > ((448) (157) (142))
;OM > 10
;OM > ((((2 3) (2 3) 2) (5 (2 3) 2) ((7 11 13))))
;OM > (3 4)
;OM > 119.121445
;OM > 1495
;OM > 0.5
;OM > nil
;OM > 23



;(kbest-segment-quantify '(0 494 995 1480 1884) '((485) (214) (168) (404)) 10 '((((2 3) (2 3) 2) (5 (2 3) 2) ((7 11 13)))) '(3 4) 120.645455 1490 0.5 nil 2)

;(kbest-segment-quantify '(419 576 842 1069 1302 1563) '((104 55) (310) (302) (254) (261)) 10 '((((2 3) (2 3) 2) (5 (2 3) 2) ((7 11 13)))) '(3 4) 108.86856 1557 0.5 nil 394)

;(kbest-segment-quantify '(26 500 1001 1143) '((448) (157) (142)) 10 '((((2 3) (2 3) 2) (5 (2 3) 2) ((7 11 13)))) '(3 4) 119.121445 1495 '0.5 nil 23)

;(kbest-segment-quantify-automata   '(0 494 995 1480 1884) '((485) (214) (168) (404)) 10 '((((2 3) (2 3) 2) (5 (2 3) 2) ((7 11 13))))  1490 0.5 2)

;(kbest-segment-quantify-automata   '(0 806 1625 2417 3208 4208) '((1000) (1000) (1000) (1000) (1000)) 10 '((((2 3) (2 3) 2) (5 (2 3) 2) ((7 11 13))))  4208 0.5 2)


;(kbest-to-poly (kbest-segment-quantify-automatas    '(0) nil 1 '((((2 3) (2 3) 2) (5 (2 3) 2) ((7 11 13))))  3278 0 991))




;(defun test ()

;  (let* ((kbest (kbest-segment-quantify-automatas '(0 494 995 1480 1884) '((485) (214) (168) (404)) 10 '((((2 3) (2 3) 2) (5 (2 3) 2) ((7 11 13))))  2590 0.5 2))
;         (ktable (first (ktables kbest)))
;         (input (first (inputs kbest))))

    
;    (rq-to-omtree ktable input 1 nil 0 (path-addi (path-empty input) 2 1))

;    (let ((qv  (make-instance 'om::quant-voice :automaton kbest)))

;      (om::init-choices qv)
    


;)))

;(test)



(defun testsavetable ()
  (let* ((k-best (rq::k-best-segment-quantify-automatas '(0 1000) '((1000)) 3 '((((2 3) (2 3) 2) (5 (2 3) 2) ((7 11 13)))) 1000 0  nil))
         (ktable (car (rq::ktables k-best))))
    (omng-save ktable)))
    
;(testsavetable)
;(gethash (path-addi (path-empty (input-make '(2 3 3) #(0 1000) #(1 -1)) ) 2 1) (eval (testsavetable)))
