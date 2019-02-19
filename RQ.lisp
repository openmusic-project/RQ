(in-package :om)

(defpackage "RQ"
    (:use "COMMON-LISP" "CL-USER" "OM-API"  "LISPWORKS" "OM-LISP" "OPENMUSIC")
    (:import-from "CL-USER")   
    )

;----------------------
; Loading files
;----------------------
(mapc 
 #'(lambda (file) (om::compile&load (om-relative-path (car file) (cadr file))))
  '((("src" "algorithm" "structure") "complexity")
    (("src" "algorithm" "structure") "distance")
    (("src" "algorithm" "structure") "heap")
    (("src" "algorithm" "structure") "input")
    (("src" "algorithm" "structure") "path")
    (("src" "algorithm" "structure") "weight")
    (("src" "algorithm" "structure") "k-list")
    (("src" "algorithm" "structure") "k-best")
    
    (("src" "algorithm") "subdivision")
    (("src" "algorithm") "ratio")
    (("src" "algorithm") "rq")
    (("src" "algorithm") "interface")
    (("src" "algorithm") "k-best-segment-quantification")
    (("src" "algorithm") "k-best-segment-quantification-automaton-with-tempo")
    (("src" "algorithm") "k-best-segment-quantification-automaton")
    (("src" "algorithm") "k-best-segment-quantification-automatons")
    (("src" "algorithm") "tatum-seg-algorithm")
    (("src" "algorithm") "tempo-smoothing")
    
    (("src" "gui" "panel") "multi-poly-panel")
    (("src" "gui" "panel") "quant-voice-panel")
    (("src" "gui" "panel") "quant-chord-seq-panel")
    (("src" "gui" "panel") "tree-view")
    (("src" "gui") "quant-multi-poly")
    (("src" "gui" "editor") "quant-voice-editor")
    (("src" "gui" "editor") "multi-poly-editor")
    (("src" "gui" "editor") "quant-chord-seq-editor")
    (("src" "gui") "choice-table")
    (("src" "gui") "quant-voice")
    (("src" "gui") "quant-chord-seq")
    (("src" "gui" "analysis") "k-best-data")
    (("src" "gui" "analysis") "k-best-analysis")
    (("src" ) 	"rq-class")
    (("src" "gui" "window") "k-best-data-window")
    (("src" "gui" "box") "quant-box")
    (("src" "gui" "editor") "quant-editor")))




;--------------------------------------------------
; Setting the menu and sub-menu structure, and filling packages
; The sub-list syntax:
; ("sub-pack-name" subpackage-lists class-list function-list class-alias-list)
;--------------------------------------------------
(om::fill-library
 '(("RQ" nil (rq::rq) (rq::get-voice rq::get-k-best-list) nil)
   )
 (find-library "RQ"))

(set-lib-release 1.1) 


(print "
;;;===================================================
;;;
;;; RQ - Rhythm Quantification for OM
;;;
;;;===================================================
")


;----------------------
; Export functions
;----------------------

(in-package :rq)

(export '(
          rq
          get-voice
          get-k-best-list
          ))

;To make these symbols usable in the OM package
(import '(
          rq
          get-voice
          get-k-best-list
          )
        :om)
