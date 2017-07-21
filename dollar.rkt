#lang racket/base

(require scribble/manual
         scribble/core
         scribble/html-properties
         scribble/latex-properties
         scriblib/render-cond
         racket/runtime-path
         setup/collects
         "katex-convert-unicode.rkt"
         "mathjax-convert-unicode.rkt"
         racket/list)

(provide $
         $$
         $-html-handler
         $$-html-handler
         $-katex
         $$-katex
         $-mathjax
         $$-mathjax
         use-katex
         use-mathjax
         with-html5)

;; KaTeX does not work well with the HTML 4.01 Transitional loose DTD,
;; so we define a style modifier which replaces the prefix for HTML rendering.
(define (with-html5 doc-style)
  (define has-html-defaults? (memf html-defaults? (style-properties doc-style)))
  (define new-properties
    (if has-html-defaults?
        (map (λ (s)
               (if (html-defaults? s)
                   (html-defaults (path->collects-relative
                                   (collection-file-path "html5-prefix.html"
                                                         "scribble-math"))
                                  (html-defaults-style-path s)
                                  (html-defaults-extra-files s))
                   s))
             (style-properties doc-style))
        (cons (html-defaults (path->collects-relative
                              (collection-file-path "html5-prefix.html"
                                                    "scribble-math"))
                             #f
                             '()))))
  (style (style-name doc-style)
         new-properties))


;; Other possible sources for MathJax:
;"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
;"http://c328740.r40.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=default"
;"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-HTML"

(define-runtime-path mathjax-dir "MathJax")
(define-runtime-path katex-dir "katex")
#|
(define mathjax-dir
  (path->collects-relative
   (collection-file-path "MathJax" "scribble-math")))
|#


;; take into account last scroll event, to avoid locking up the page
;; TODO: should actually pause the whole MathJax queue, as it still locks
;; up the browser between "processing Math" and "rendering math".
;; + set the #MathJax_Message CSS to opacity: 0.5.
(define gradually-rename-texMath-to-texMathX-js #<<EOJS
(function() {
	var lastScroll = -1001;
	scrollEventHandler = function(e) {
		lastScroll = new Date().getTime();
	};
	window.addEventListener("scroll", scrollEventHandler);
	var e = document.getElementsByClassName("texMath");
    var pos = 0;
 	var o = {};
    process = function() {
		if (pos < e.length){
			if (new Date().getTime() - lastScroll > 1000) {
				if (e.id == "") { e.id = "texMathElement" + pos; }
 				e[pos++].className = "texMathX";
				MathJax.Hub.Queue(["Typeset",MathJax.Hub,e.id]);
				MathJax.Hub.Queue(["next",o]);
            } else {
				window.setTimeout(process, 100);
            }
        } else {
			window.removeEventListener("scroll", scrollEventHandler);
        }
    };
	o.next = function() {
		window.setTimeout(process, 100);
    }
	process();
})();
EOJS
  )

(define (load-script-string src [async-defer #f])
  (string-append
   #<<EOJS
(function() {
  document.write('<scr' + 'ipt type="text/javascript" src="
EOJS
   src
   "\""
   (if async-defer " async=\"async\" defer=\"defer\" " "")
   #<<EOJS
></scr' + 'ipt>');
})();
EOJS
   ))

(define (load-style-string src)
  (string-append
   #<<EOJS
(function() {document.write('<link rel="stylesheet" href="
EOJS
   src
   #<<EOJS
" />');})();
EOJS
   ))

(define load-mathjax-code
  (string->bytes/utf-8
   ;; To avoid the need to alter the MathJax configuration, add:
   ;; <script type="text/x-mathjax-config">
   ;;   MathJax.Hub.Config({ tex2jax: {inlineMath: [['$','$']]} });
   ;; </script>
   (load-script-string "MathJax/MathJax.js?config=default")))

#;(define load-mathjax-code
  (string->bytes/utf-8
   (string-append (load-script-string "MathJax/MathJax.js?config=default")
                  #<<EOJS
(function(f) {
  // A "simple" onLoad function
  if (window.document.readyState == "complete") {
    f();
  } else if (window.document.addEventListener) {
    window.document.addEventListener("DOMContentLoaded", f, false);
  } else if (window.attachEvent) {
    window.attachEvent("onreadystatechange", function() {
      if (window.document.readyState == "complete") {
        f();
      }
    });
  } else {
    var oldLoad = window.onload;
    if (typeof(oldLoad) == "function") {
      window.onload = function() {
        try {
          oldLoad();
        } finally {
          f();
        }
      };
    } else {
      window.onload = f;
    }
  }
})(function() {
  var wrap = function(elts, opn, cloz) {
    var eltsX = [];
    for (var i = 0; i < elts.length; i++) { eltsX[i] = elts[i]; }
    for (var i = 0; i < eltsX.length; i++) {
      var opntxt = document.createTextNode(opn);
      var cloztxt = document.createTextNode(cloz);
      var e = eltsX[i];
      e.insertBefore(opntxt, e.firstChild);
      e.appendChild(cloztxt);
      e.className = e.className.replace(/\b(texMathInline|texMathDisplay)\b/g,
                                        "texMath");
    }
  };
  wrap(document.getElementsByClassName("texMathInline"), "\\(", "\\)");
  wrap(document.getElementsByClassName("texMathDisplay"), "\\[", "\\]");
});
EOJS
                  )))

(define load-katex-code+style
  (string->bytes/utf-8
   (string-append (load-style-string "katex/katex.min.css")
                  (load-script-string "katex/katex.min.js")
                  #<<EOJS
(function(f) {
  // A "simple" onLoad function
  if (window.document.readyState == "complete") {
    f();
  } else if (window.document.addEventListener) {
    window.document.addEventListener("DOMContentLoaded", f, false);
  } else if (window.attachEvent) {
    window.attachEvent("onreadystatechange", function() {
      if (window.document.readyState == "complete") {
        f();
      }
    });
  } else {
    var oldLoad = window.onload;
    if (typeof(oldLoad) == "function") {
      window.onload = function() {
        try {
          oldLoad();
        } finally {
          f();
        }
      };
    } else {
      window.onload = f;
    }
  }
})(function() {
  // This is an ugly way to change the doctype, in case the scribble document
  // did not use (with-html5).
  if (!(document.doctype && document.doctype.publicId == '')) {
    if (console && console.log) {
      console.log("Re-wrote the document to use the HTML5 doctype.\n"
                  + "  Consider using the following declaration:\n"
                  + "      @title[#:style (with-html5 manual-doc-style)]{…}");
    }
    var wholeDoc = '<!doctype HTML>\n' + document.documentElement.outerHTML;
    document.open();
    document.clear();
    document.write(wholeDoc);
  }
  var inlineElements = document.getElementsByClassName("texMathInline");
  for (var i = 0; i < inlineElements.length; i++) {
    var e = inlineElements[i];
    katex.render(e.textContent, e, { displayMode:false, throwOnError:false });
  }
  var displayElements = document.getElementsByClassName("texMathDisplay");
  for (var i = 0; i < displayElements.length; i++) {
    var e = displayElements[i];
    katex.render(e.textContent, e, { displayMode:true, throwOnError:false });
  }
});
EOJS
                  )))

(define tex-commands
  (string->bytes/utf-8 #<<EOTEX
\def\texMath#1{#1}
\def\texMathInline#1{\ensuremath{#1}}
\def\texMathDisplay#1{\ifmmode #1\else\[#1\]\fi}
EOTEX
                       ))

(define math-inline-style-mathjax
  (style "math"
         (list (alt-tag "span")
               #;(make-css-addition math-inline.css)
               (install-resource mathjax-dir)
               (js-addition load-mathjax-code)
               'exact-chars)))

(define math-display-style-mathjax
  (style "math"
         (list (alt-tag "div")
               #;(make-css-addition math-inline.css)
               (install-resource mathjax-dir)
               (js-addition load-mathjax-code)
               'exact-chars)))

(define math-inline-style-katex
  (style "texMathInline"
         (list (install-resource katex-dir)
               (js-addition load-katex-code+style)
               'exact-chars)))

(define math-display-style-katex
  (style "texMathDisplay"
         (list (install-resource katex-dir)
               (js-addition load-katex-code+style)
               'exact-chars)))

(define math-inline-style-latex
  (style "texMathInline"
         (list (tex-addition tex-commands)
               'exact-chars)))

(define math-display-style-latex
  (style "texMathDisplay"
         (list (tex-addition tex-commands)
               'exact-chars)))

(define ($-mathjax strs)
  (elem #:style math-inline-style-mathjax strs))

(define ($-katex strs)
  (elem #:style math-inline-style-katex
                (map (λ (s) (katex-convert-unicode s #t)) (flatten strs))))

(define ($$-mathjax strs)
  (elem #:style math-display-style-mathjax strs))

(define ($$-katex strs)
  (elem #:style math-display-style-katex
                (map (λ (s) (katex-convert-unicode s #t)) (flatten strs))))

(define $-html-handler (make-parameter $-katex))
(define $$-html-handler (make-parameter $$-katex))

(define (use-katex)
  ($-html-handler $-katex)
  ($$-html-handler $$-katex)
  (void))

(define (use-mathjax)
  ($-html-handler $-mathjax)
  ($$-html-handler $$-mathjax)
  (void))

(define ($ . strs)
  (let ([$- ($-html-handler)])
    (cond-element
     [html ($- strs)]
     [latex (elem #:style math-inline-style-latex strs)]
     ;; TODO: use a unicode representation of math, e.g. x^2 becomes x²
     [else strs])))

(define ($$ . strs)
  (let ([$$- ($$-html-handler)])
    (cond-element
     [html ($$- strs)]
     [latex (elem #:style math-display-style-latex strs)]
     ;; TODO: use a spatial representation of display math, e.g.
     ;; \sum_{i=0}^n x_i^2
     ;; becomes:
     ;;      n
     ;;     ───
     ;;     ╲     2
     ;;      〉   x
     ;;     ╱     i
     ;;     ───
     ;;     i=0
     ;; Or use a spatial unicode representation, so that the above becomes:
     ;;  n
     ;;  ∑  xᵢ²
     ;; i=0
     [else strs])))
