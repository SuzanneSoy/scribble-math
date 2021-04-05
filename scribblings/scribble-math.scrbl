#lang scribble/manual
@require[@for-label[scribble-math
                    racket/base
                    scribble/core]
         @for-syntax[racket/base
                     syntax/parse]
         scribble-math]


@(define-syntax scribbleblock
   (syntax-parser
     [(_ (~optional (~seq #:keep-lang-line? keep-lang))
         str ...+)
      #`(codeblock
         #:keep-lang-line? #,(if (attribute keep-lang) #'keep-lang #'#f)
         "#lang scribble/base" "\n"
         str ...)]))

@(define-syntax scribblecode
   (syntax-parser
     [(_ str ...+)
      #`(code #:lang "scribble/base"
              str ...)]))

@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)]{@racketmodname[scribble-math]}
@author[
 @author+email["Jens Axel Søgaard" "jensaxel@soegaard.net"]
 @author+email["Suzanne Soy" "racket@suzanne.soy"]]

@defmodule[scribble-math]

This library allows typesetting math and Asymptote figures
in Scribble documents.

@(local-table-of-contents #:style 'immediate-only)

@section{Typesetting math with @racket[$] and @racket[$$]}
@defmodule[scribble-math/dollar]

@(define title-html5-code
   @scribblecode|{@title[#:style (with-html5 manual-doc-style)]{…}}|)

The following functions help with typesetting mathematical
equations. The main functions are @racket[$] for
@tech{inline mode} math and @racket[$$] for @tech{display
 mode} math, respectively. The functions @racket[use-katex]
and @racket[use-mathjax] change the rendering engine used,
the default being @racket[katex]. To use @racket[katex], it
is necessary to use
@title-html5-code or a similar configuration; for more
details see the documentation of @racket[with-html5].

@defproc[($ [str string?] ...) element?]{
 Renders the given strings as @deftech{inline mode} math.
 Inline mode math is typeset as part of the surrounding
 text. Either MathJax or KaTeX is used for the HTML output,
 depending on the current configuration. For the LaTeX
 output, the code is simply passed as-is. For example, when
 using MathJax, @racket[($ "x^2")] renders as
 @(use-mathjax) @${x^2}.

 The syntax accepted by @racket[$] is a subset of the
 commands supported by LaTeX, and depends on the backend
 used (MathJax should support more commands than KaTeX). For
 details, see their respective documentation.}

@defproc[($$ [str string?] ...) element?]{
 Renders the given strings as @deftech{display mode} math. 
 Display mode math is typeset alone on its line, and is
 centered. Some symbols like @${\sum} are larger in display
 mode than in @tech{inline mode}, which makes the former better for
 complex equations. Either MathJax or KaTeX is used for the
 HTML output, depending on the current configuration. For
 the LaTeX output, the code is simply passed as-is. For
 example, when using MathJax,
 
 @racketblock[($$ "\\sum_{i=0}^n x_i^3")]

 renders as:

 @(use-mathjax)
 @$${\sum_{i=0}^n x_i^3}

 The syntax accepted by @racket[$$] is a subset of the
 commands supported by LaTeX, and depends on the backend
 used (MathJax should support more commands than KaTeX). For
 details, see their respective documentation.}

@defproc[(with-html5 [doc-style style?]) style?]{
 Alters the given document style, so that the resulting
 document uses HTML5.

 This function should be called to alter the 
 @racket[#:style] argument for @racket[title] when KaTeX is
 used, as KaTeX is incompatible with the default scribble 
 @tt{DOCTYPE} (the HTML 4.01 Transitional loose DTD). The
 scribble document should therefore contain code similar to
 the following:
 
 @scribbleblock|{
  @title[#:style (with-html5 manual-doc-style)]{...}
  }|

 This function works by changing the existing 
 @racket[html-defaults] property or adding a new one, so
 that it uses an HTML5
 @tech[#:doc '(lib "scribblings/scribble/scribble.scrbl")]{prefix file}
 (the @tech[#:doc '(lib "scribblings/scribble/scribble.scrbl")]{prefix file}
 contains the @tt{DOCTYPE} line).}

@defparam[$-html-handler handler (→ (listof? string?) element?)
          #:value $-katex]{
 A parameter whose value is a function called by @racket[$],
 to transform the math code into HTML. The @racket[$]
 function uses this parameter only when rendering the
 document as HTML.}

@defparam[$$-html-handler handler (→ (listof? string?) element?)
          #:value $$-katex]{
 A parameter whose value is a function called by 
 @racket[$$], to transform the math code into HTML. The 
 @racket[$$] function uses this parameter only when
 rendering the document as HTML. }

@defproc[($-katex [math (listof? string?)]) element?]{
 Produces an @racket[element?] which contains the given 
 @racket[math] code, so that it is rendered as @tech{inline
  mode} math using KaTeX. More precisely, the resulting
 element uses several scribble properties to add scripts and
 stylesheets to the document. The resulting element also
 uses a specific CSS class so that when the page is loaded
 into a browser, KaTeX can recognise it and render it in 
 @tech{inline mode}.

 @racket[($-katex "x^2")] renders as @$-katex{x^2}.}

@defproc[($$-katex [math (listof? string?)]) element?]{
 Produces an @racket[element?] which contains the given 
 @racket[math] code, so that it is rendered as @tech{display
  mode} math (centered, alone on its line) using KaTeX. More
 precisely, the resulting element uses several scribble
 properties to add scripts and stylesheets to the document.
 The resulting element also uses a specific CSS class so
 that when the page is loaded into a browser, KaTeX can
 recognise it and render it in @tech{display mode}.

 @racketblock[($$-katex "\\sum_{i=0}^n x_i^3")]

 renders as:

 @$$-katex{\sum_{i=0}^n x_i^3}}

@defproc[($-mathjax [math (listof? string?)]) element?]{
 Produces an @racket[element?] which contains the given 
 @racket[math] code, so that it is rendered as @tech{inline
  mode} math using MathJax. More precisely, the resulting
 element uses several scribble properties to add scripts and
 stylesheets to the document. The resulting element also
 uses a specific CSS class so that when the page is loaded
 into a browser, MathJax can recognise it and render it in 
 @tech{inline mode}.

 @racket[($-mathjax "x^2")] renders as @$-mathjax{x^2}.}

@defproc[($$-mathjax [math (listof? string?)]) element?]{
 Produces an @racket[element?] which contains the given 
 @racket[math] code, so that it is rendered as @tech{display
  mode} math (centered, alone on its line) using KaTeX. More
 precisely, the resulting element uses several scribble
 properties to add scripts and stylesheets to the document.
 The resulting element also uses a specific CSS class so
 that when the page is loaded into a browser, MathJax can
 recognise it and render it in @tech{display mode}.

 @racketblock[($$-mathjax "\\sum_{i=0}^n x_i^3")]

 renders as:

 @$$-mathjax{\sum_{i=0}^n x_i^3}}


@defproc[($-tex2svg [math (listof? string?)]) element?]{
 Produces an @racket[element?] which contains the given
 @racket[math] rendered as an HTML SVG literal.
 It is rendered in @tech{inline mode} math using @tt{tex2svg}.
 More precisely, the resulting element uses the @racket[xexpr-property] to
 render the SVG directly to the HTML document.
 This means no new scripts or stylesheets are added to the document.
 It also has no style, so its style cannot be customized.

@; @racket[($-tex2svg "x^2")] renders as @$-tex2svg{x^2}.

 This procedure requires that @code{tex2svg} be installed via
 @code{npm install mathjax-node-cli}.

 This procedure requires Racket 6.12 or later.}

@defproc[($$-tex2svg [math (listof? string?)]) element?]{
 Produces an @racket[element?] which contains the given
 @racket[math] rendered as an HTML SVG literal.
 It is rendered in @tech{display mode} math using @tt{tex2svg}.
 More precisely, the resulting element uses the @racket[xexpr-property] to
 render the SVG directly to the HTML document.
 This means no new scripts or stylesheets are added to the document.
 It also has no style, so its style cannot be customized.


@; @racketblock[($$-tex2svg "\\sum_{i=0}^n x_i^3")]

@; renders as:

@; @$$-tex2svg{\sum_{i=0}^n x_i^3}

 This procedure requires that @code{tex2svg} be installed via
 @code{npm install mathjax-node-cli}.

 This procedure requires Racket 6.12 or later.}

@defproc[(use-katex) void?]{
 This shorthand calls @racket[($-html-handler $-katex)]
 and @racket[($$-html-handler $$-katex)]. The mathematical
 formulas passed to @racket[$] and @racket[$$] which appear
 later in the document will therefore be typeset using
 KaTeX.

 The KaTeX library will be added to the HTML document only
 if it uses the result of one of @racket[$], @racket[$$], 
 @racket[$-katex] or @racket[$$-katex]. It is therefore safe
 to call this function in libraries to change the default
 handler, without the risk of adding extra resources to the
 page if the user changes the default before typesetting any
 math.}

@defproc[(use-mathjax) void?]{
 This shorthand calls @racket[($-html-handler $-mathjax)]
 and @racket[($$-html-handler $$-mathjax)]. The mathematical
 formulas passed to @racket[$] and @racket[$$] which appear
 later in the document will therefore be typeset using
 MathJax.

 The MathJax library will be added to the HTML document only
 if i uses the result of one of @racket[$], @racket[$$], 
 @racket[$-katex] or @racket[$$-katex]. It is therefore safe
 to call this function in libraries to change the default
 handler, without the risk of adding extra resources to the
 page if the user changes the default before typesetting any
 math.}

@defproc[(use-tex2svg) void?]{
 This shorthand calls @racket[($-html-handler $-tex2svg)] and
 @racket[($$-html-handler $$-tex2svg)]. The mathematical formulas passed to
 @racket[$] and @racket[$$] which appear later in the document will therefore be
 typeset using @tt{tex2svg}.

 No new CSS or JavaScript libraries will be added to the document. Instead, the
 generated HTML document have the math embedded directly an @tt{svg}.

 This requires that @tt{tex2svg} is installed on the system. You can install it
 globally via @tt{sudo npm install --global mathjax-node-cli} or locally with
 @tt{npm install mathjax-node-cli}. The backend will attempt to find the
 @tt{tex2svg}, preferring local sources. You can set the path manually with
 the parameter @racket[current-tex2svg-path].

 @tt{tex2svg} will only be used when rendering an HTML document, and only if it
 uses @racket[$] or @racket[$$] to render math. It is therefore safe to call
 this function in libraries to change the default handler.

 This procedure requires Racket 6.12 or later.}

@defparam[current-tex2svg-path path path? #:value #f]{
 A parameter whose value is the path to the @tt{tex2svg} binary.
 This binary is used to transform math code into HTML when using the @tt{tex2svg}
 backend.
 
 The functions @racket[$-tex2svg] and @racket[$$-tex2svg] use this parameter only
 when rendering the document as HTML.

 This parameter requires Racket 6.12 or later.}

@defparam[use-external-mathjax URL (or/c #f string?) #:value #f]{
                                                       
 A parameter whose value is the URL to the MathJax script
 to use. The URL must be absolute, or relative to the URL
 used to display the document.

 For example, if the HTML document is accessed via @tt{
  file:///home/user/docs/document1/index.html}, and
 @racket[(use-external-mathjax "../common/MathJax/MathJax.js?config=default")] was
 used, then MathJax will be loaded from @tt{
  file:///home/user/docs/common/MathJax/MathJax.js?config=default}.

 An URL to a CDN is also valid, but may be a poor choice
 regarding the privacy of your users.

 This feature is in beta and might not work, please report
 any issue.}

@defparam[use-external-katex URLs (or/c #f (list/c string? string?)) #:value #f]{
                                                       
 A parameter whose value is a list containing the URL to the
 KaTeX script and the URL to KaTeX CSS to use. The URLs must
 be absolute, or relative to the URL used to display the
 document.

 For example, if the HTML document is accessed via @tt{
  file:///home/user/docs/document1/index.html}, and
 @racket[(use-external-katex (list "../common/KaTeX/katex.min.js" "../common/KaTeX/katex.min.css"))]
 was used, then the KaTeX script will be loaded from @tt{
  file:///home/user/docs/common/KaTeX/katex.min.js} and the
 KaTeX stylesheet from @tt{
  file:///home/user/docs/common/KaTeX/katex.min.css}.

 An URL to a CDN is also valid, but may be a poor choice
 regarding the privacy of your users.

 Please note that using a .js and a .css file which are not
 in the same directory is unsupported (it has not been tested
 and may or may not work).

 This feature is in beta and might not work, please report
 any issue.}

@;@$${\sum_{i=0}ⁿ xᵢ³}

When using MathJax, @racket[$] and @racket[$$] wrap their
content with @racket["$…$"] and @racket["\\[…\\]"]
respectively, and insert it in an element with the class 
@racket["tex2jax_process"]. MathJax is configured to only
process elements with this class, so it is safe to use
@tt{$} signs in the source document. For example, the text
$\sum x^3$ is typeset as-is, like the rest of the text.

When using @tt{tex2svg}, no additional JavaScript processing is done on the
page, so it is safe to use @tt{$} signs in the source document. For example, the
text $\sum x^3$ is typeset as-is, like the rest of the text.

@section{Drawing figures with Asymptote}

@defmodule[scribble-math/asymptote]

@defproc[(asymptote [#:cache cache? any/c #t] [str string?] ...+) image?]{
 Renders the figure described by the given strings using
 Asymptote. If @racket[cache?] is @racket[#f], then the
 resulting images are generated into temporary PNG, SVG and
 PDF files using @racket[make-temporary-file]. Otherwise, to
 improve compilation speed, the result is cached in the 
 @filepath{asymptote-images} directory, based on a checksum
 of the strings. It is a good idea to clean up the working
 directory after experimenting a lot with a figure, as it
 will be cluttered with stale cached files.

 If the Asymptote code is dynamically generated, make sure
 that the result is always the same, or use 
 @racket[#:cache #f]. Otherwise, each compilation would
 cause a new file to be generated.

 The @tt{asy} executable must be installed on the
 machine that renders the figures. If the results are
 already cached, then the scribble document can be compiled
 without installing Asymptote.

 As an example, the code
 
 @scribbleblock|{
  @asymptote{
   import drawtree;
   size(4cm, 0);
   TreeNode root = makeNode("let");
   TreeNode bindings = makeNode(root, "bindings");
   TreeNode binding = makeNode(bindings, "binding");
   TreeNode bid = makeNode(binding, "id");
   TreeNode bexpr = makeNode(binding, "expr");
   TreeNode bindingddd = makeNode(bindings, "\vphantom{x}\dots");
   TreeNode body = makeNode(root, "body");
   TreeNode bodyddd = makeNode(root, "\vphantom{x}\dots");

   draw(root, (0,0));
   shipout(scale(2)*currentpicture.fit());
  }
 }|
 
 renders as:
 
 @asymptote{
  import drawtree;
  size(4cm, 0);
  TreeNode root = makeNode("let");
  TreeNode bindings = makeNode(root, "bindings");
  TreeNode binding = makeNode(bindings, "binding");
  TreeNode bid = makeNode(binding, "id");
  TreeNode bexpr = makeNode(binding, "expr");
  TreeNode bindingddd = makeNode(bindings, "\vphantom{bg}\dots");
  TreeNode body = makeNode(root, "body");
  TreeNode bodyddd = makeNode(root, "\vphantom{bg}\dots");

  draw(root, (0,0));
  shipout(scale(2)*currentpicture.fit());
 }
}
