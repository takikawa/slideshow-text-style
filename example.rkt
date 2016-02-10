#lang at-exp slideshow

(require slideshow-style)

(with-style
  #:defaults [#:face "Bitstream Vera Sans, Bold"
              #:size 27]
  ([a #:face "Bistream Vera Sans"
      #:color "DimGray"]
   [b #:color "CornflowerBlue"]
   [c #:color "Tomato"])

  (slide
    @a{Whereas recognition of the @b{inherent dignity} and of the equal
       and @b{inalienable rights} of all members of the human family is
       the foundation of @c{freedom}, @c{justice} and @c{peace} in the world}))
