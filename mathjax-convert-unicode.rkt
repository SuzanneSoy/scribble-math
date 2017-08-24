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
      [⁺ "{}^+"]
      [⁻ "{}^-"]
      ;; TODO:
      [❲ "\\ifmathjax{\\unicode{x2772}}\\iflatex{❲}"]
      [❳ "\\ifmathjax{\\unicode{x2773}}\\iflatex{❳}"]
      [φ "\\phi"]
      [▷ "\\triangleright"]
      [∄ "\\nexists"]
      [≠ "\\neq"]
      [❬ ,(string-append
           "\\ifmathjax{\\hspace{-0.2ex}\\unicode{x276C}\\hspace{-0.2ex}}"
           "\\iflatex{❬}")]
      [❭ ,(string-append
           "\\ifmathjax{\\hspace{-0.2ex}\\unicode{x276D}\\hspace{-0.2ex}}"
           "\\iflatex{❭}")]))
  (katex-convert-unicode str* mathmode? more-sym→*))