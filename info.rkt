#lang info

(define deps '("base" "pict-lib" "slideshow-lib" "scribble-text-lib"))
(define build-deps '("scribble-lib" "at-exp-lib" "pict-doc"
                     "slideshow-doc" "racket-doc" "scribble-doc"))
(define scribblings '(("slideshow-text-style.scrbl" () ("Slideshow Libraries"))))
(define test-omit-paths (if (getenv "PLT_PKG_BUILD_SERVICE") '("example.rkt") '()))
