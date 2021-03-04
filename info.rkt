#lang info
(define collection "scribble-math")
(define deps '("base"
               "rackunit-lib"
               "scribble-lib"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "at-exp-lib"
                     "scribble-doc"))
(define compile-omit-paths '("MathJax" "katex"))
(define test-omit-paths '("MathJax" "katex"))
(define scribblings '(("scribblings/scribble-math.scrbl" ())))
(define pkg-desc "Typesetting math and Asymptote figures in Scribble documents")
(define version "0.10")
(define pkg-authors '(|Suzanne Soy|
                      |Jens Axel Søgaard|))
