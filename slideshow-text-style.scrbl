#lang scribble/manual

@(require (for-label slideshow
                     slideshow-text-style))

@title{slideshow-text-style: a small library to improve text formatting in slideshow}

@(defmodule slideshow-text-style)

This module provides a single macro that's intended for use with Scribble's
at-expression reader in a Slideshow document. It makes it easier to use
fancy text formatting in code for slides.

@defform[(with-text-style maybe-defaults (style-spec ...) body ...)
         #:grammar
         [(maybe-defaults (code:line)
                          (code:line #:defaults (style-spec-item ...)))
          (style-spec (name style-spec-item ...)
                      ((name parent) spec-item ...))
          (style-spec-item (code:line #:face expr)
                           (code:line #:color expr)
                           (code:line #:size expr)
                           (code:line #:line-sep expr)
                           (code:line #:left-pad expr))]]{

This form binds each @racket[name] in the @racket[style-spec]s to functions which
produce formatted text using Slideshow's @racket[text] function. Each function
uses the style specifications indicated by the keywords.

If the second form with @racket[parent] is used, the @racket[name] function uses
the style of the parent function but with optionally overriden styles.

If provided, @racket[maybe-defaults] specifies the default style specifications that
are shared by all of the styling functions.
}

Here is a full example that you can run:

@codeblock|{
#lang at-exp slideshow

(require slideshow-text-style)

(with-text-style
  #:defaults [#:face "Bitstream Vera Sans, Bold"
              #:size 27
              #:line-sep 7]
  ([a #:face "Bistream Vera Sans"
      #:color "DimGray"]
   [b #:color "CornflowerBlue"]
   [c #:color "Tomato"]
   ;; a kind of inheritance
   [(d c) #:face "Bitstream Vera Sans"])

  (slide
    @a{Whereas recognition of the @b{inherent dignity} and of the @b{equal}
       and @b{inalienable rights} of all members of the human family is
       the foundation of @c{freedom}, @c{justice} and @c{peace} in the world}))
}|
