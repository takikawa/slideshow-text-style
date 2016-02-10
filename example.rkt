#lang at-exp slideshow

(require slideshow-style)

(with-style
  ([a #:face "Bistream Vera Sans"
      #:color "DimGray"
      #:size 27]
   [b #:face "Bitstream Vera Sans, Bold"
      #:color "CornflowerBlue"
      #:size 27]
   [c #:face "Bistream Vera Sans, Bold"
      #:color "Tomato"
      #:size 27])

  (slide
    @a{Whereas recognition of the @b{inherent dignity} and of the equal
       and @b{inalienable rights} of all members of the human family is
       the foundation of @c{freedom}, @c{justice} and @c{peace} in the world}))
