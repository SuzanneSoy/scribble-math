#lang racket

(require scribble/manual
         file/md5
         racket/system)

(provide asymptote)

(define (asymptote #:cache [cache? #t] s . strs)
  (define single-str
    (string-append (apply ~a #:separator "\n" s strs) "\n"))
  (if cache?
      ;; cache:
      (let* ([asymptote-dir "asymptote-images"]
             [md (bytes->string/utf-8 (md5 single-str))]
             [asy-name (string-append md ".asy")]
             [asy-path (build-path asymptote-dir asy-name)]
             [png-name (string-append md ".png")]
             [png-path (build-path asymptote-dir png-name)]
             [eps-name (string-append md ".eps")]
             [eps-path (build-path asymptote-dir eps-name)]
             [pdf-name (string-append md ".pdf")]
             [pdf-path (build-path asymptote-dir pdf-name)]
             [svg-name (string-append md ".svg")]
             [svg-path (build-path asymptote-dir svg-name)])
        ;; create dir if neccessary
        (unless (directory-exists? asymptote-dir)
          (make-directory asymptote-dir))
        ;; save asymptote code to <md5-of-input>.asy
        (with-output-to-file asy-path
          (lambda () (display single-str))
          #:exists 'replace)
        (parameterize ([current-directory (build-path (current-directory)
                                                      asymptote-dir)])
          ;; run asymptote to generate eps
          (unless (file-exists? svg-name)
            (system (format "asy -v -f svg ~a" asy-name)))
          ;; run asymptote to generate pdf
          (unless (file-exists? pdf-name)
            (system (format "asy -v -f pdf ~a" asy-name)))
          ;; run asymptote to generate png
          (unless (file-exists? png-name)
            (system (format "asy -v -f png ~a" asy-name)))
          (image (build-path asymptote-dir md)
                 #:suffixes (list ".pdf" ".svg" ".png"))))
      ;; no cache:
      (let ([tmp-file (make-temporary-file "asy-~a.png")])
        (with-input-from-string
         single-str
         (λ ()
           ;(with-output-to-string
           (system (format "asy -v -f png -o ~a" tmp-file))))
        tmp-file))) ; HTML png PDF pdf
