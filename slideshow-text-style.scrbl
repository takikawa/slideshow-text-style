#lang scribble/manual

@(require scribble/example
          (for-label pict
                     slideshow
                     slideshow-text-style))

@(define sl-eval (make-base-eval))
@(sl-eval '(require pict slideshow-text-style))

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
                           (code:line #:bold? expr)
                           (code:line #:italic? expr)
                           (code:line #:color expr)
                           (code:line #:size expr)
                           (code:line #:line-sep expr)
                           (code:line #:left-pad expr)
                           (code:line #:transform expr))]]{

This form binds each @racket[name] in the @racket[style-spec]s to functions which
produce formatted text using Slideshow's @racket[text] function. Each function
uses the style specifications indicated by the keywords.

If the second form with @racket[parent] is used, the @racket[name] function uses
the style of the parent function but with optionally overriden styles.

If provided, @racket[maybe-defaults] specifies the default style specifications that
are shared by all of the styling functions.

The specifications have the following effects:

@itemlist[
  @item{@racket[#:face]: use the specified font face (e.g., @racket["Times New Roman"]).}
  @item{@racket[#:bold?]: a boolean argument to turn bolding on or off}
  @item{@racket[#:italic?]: a boolean argument to turn italics on or off}
  @item{@racket[#:color]: colorize the text with the given color string}
  @item{@racket[#:size]: use the given size for the text}
  @item{@racket[#:line-sep]: a numeric argument specifying the sepration between
        lines in the text in pict units}
  @item{@racket[#:left-pad]: pad the produced pict on the left by the specified
        numeric amount in pict units}
  @item{@racket[#:transform]: apply the given pict to pict function to the result}
  @item{@racket[#:h-append]: the given function is used to append picts that are
        formatted in the same line. Defaults to @racket[hbl-append].}
  @item{@racket[#:v-append]: the given function is used to append each line of
        the formatted pict. Defaults to @racket[vl-append].}
]

@examples[#:eval sl-eval
(with-text-style ([t] [b #:color "blue"])
  (code:comment "This uses at-exps, though you can't tell in the rendered docs")
  @t{Hello @b{World}})
(define (do-fishy p)
  (hc-append
   10
   (standard-fish 30 20 #:color "PaleGreen")
   p
   (standard-fish 30 20 #:direction 'right #:color "PaleGreen")))
(with-text-style ([t] [fishy #:transform do-fishy])
  @t{@fishy{One}, @fishy{Two}, @fishy{Three}})
(with-text-style ([t]
                  [ti #:transform
                    (lambda (p) @t[#:h-append hc-append #:left-pad 30]{• @p})])
  @ti{Like a bulleted list})
]
}

Here is a more interesting example that you can try out:

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
