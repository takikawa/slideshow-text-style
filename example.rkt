#lang at-exp slideshow

(require slideshow-text-style)

(with-stylers
  #:defaults [#:face "Bitstream Vera Sans, Bold"
              #:size 27
              #:line-sep 7]
  ([a #:face "Bistream Vera Sans"
      #:color "DimGray"]
   [b #:color "CornflowerBlue"]
   [c #:color "Tomato"])

  (slide
    @a{Whereas recognition of the @b{inherent dignity} and of the @b{equal}
       and @b{inalienable rights} of all members of the human family is
       the foundation of @c{freedom}, @c{justice} and @c{peace} in the world})

  ;; variations, you can interchange a, b, c, etc.
  (slide
    @b[#:face "Bitstream Vera Sans"]{
       Whereas recognition of the @c{inherent dignity} and of the @b{equal}
       and @c{inalienable rights} of all members of the human family is
       the foundation of @c{freedom}, @c{justice} and @c{peace} in the world
    })

  (slide
    @c[#:face "Bistream Vera Sans"]{
      Whereas recognition of the @b{inherent dignity} and of the @b{equal}
      and @b{inalienable rights} of all members of the human family is
      the foundation of @b{freedom}, @b{justice} and @b{peace} in the world
    })

  ;; you can override anything at specific call-sites
  (define p "LavenderBlush")
  (define l "LightYellow")
  (slide
    (cc-superimpose
      (colorize (filled-rectangle client-w (* 0.5 client-h)) "DimGray")
      @a[#:color "white"]{
        Whereas recognition of the @b[#:color p]{inherent dignity} @;
        and of the @b[#:color p]{equal}
        and @b[#:color p]{inalienable rights} of all members of the @;
        human family is
        the foundation of @c[#:color l]{freedom}, @c[#:color l]{justice} @;
        and @c[#:color l]{peace} in the world}))

  ;; in-line picts are fine
  (define my-fish @standard-fish[70 35 #:color "LavenderBlush"])
  (slide
    @a{Whereas recognition of the @b{inherent dignity} and of the @b{equal}
       and @b{inalienable rights} of all members of the @my-fish family is
       the foundation of @b{freedom}, @b{justice} and @b{peace} in the world}))
