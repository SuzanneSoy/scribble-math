[![Build Status,](https://img.shields.io/travis/jsmaniac/scribble-math/main.svg)](https://travis-ci.org/jsmaniac/scribble-math)
[![Coverage Status,](https://img.shields.io/coveralls/jsmaniac/scribble-math/main.svg)](https://coveralls.io/github/jsmaniac/scribble-math)
[![Build Stats,](https://img.shields.io/badge/build-stats-blue.svg)](http://jsmaniac.github.io/travis-stats/#jsmaniac/scribble-math)
[![Online Documentation.](https://img.shields.io/badge/docs-online-blue.svg)](http://docs.racket-lang.org/scribble-math/)

scribble-math
=============

This library allows typesetting math and Asymptote figures in
[Scribble](https://docs.racket-lang.org/scribble/) documents.

Installation
============

To install this package, use `raco`:

    raco pkg install --deps search-auto scribble-math

Documentation
=============

See the [online documentation](http://docs.racket-lang.org/scribble-math/)
for more information about the math syntax and the
functionality of this library.

Usage example
=============

The syntax used for mathematical formulas is a subset of the
one used by LaTeX.

    #lang scribble/manual

    @require[scribble-math]
    
    @title[#:style (with-html5 manual-doc-style)]{Example}
    
    The derivative of @${x^2} is @${2x}. Complex formulas
    look best when typeset in display mode:
    
    @$${\sum_{i=0}^n x_i^3}