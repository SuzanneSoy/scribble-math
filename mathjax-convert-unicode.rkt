#lang at-exp racket/base
(require racket/string
         scriblib/render-cond
         "katex-convert-unicode.rkt")

(provide mathjax-convert-unicode)

(define (mathjax-convert-unicode str* mathmode?)
  (define more-sym→*
    `([⩴ ,(string-append
           "\\ifmathjax{\\mathrel{{\\raise0.9mu{::}\\hspace{-4mu}=}}}"
           "\\iflatex{\\Coloneqq}")]
      [∈ "\\in{}"]
      [κ "\\kappa"]
      [⁺ "{}^+"]
      [⁻ "{}^-"]
      ;; TODO:
      [❲ "\\ifmathjax{\\unicode{x2272}}\\iflatex{❲}"]
      [❳ "\\ifmathjax{\\unicode{x2273}}\\iflatex{❳}"]
      ))
  (katex-convert-unicode str* mathmode? more-sym→*))