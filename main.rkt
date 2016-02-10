#lang racket/base

;; A pict text styling library for slideshow

(require pict
         slideshow/base
         (only-in scribble/text split-lines)
         (for-syntax racket/base
                     syntax/parse))

(provide with-style)

(begin-for-syntax
  (define-syntax-class style-spec
    #:attributes (binder)
    (pattern (name:id options:style-options)
             #:with binder #'[name (styled-text . options)])
    ;; TODO: implement this case
    #;
    (pattern font-object-or-style))

  (define-splicing-syntax-class style-options
    (pattern (~seq (~or (~optional (~seq #:face face:expr))
                        (~optional (~seq #:color color:expr))
                        (~optional (~seq #:size size:expr)))
                   ...))))

(define-syntax (with-style stx)
  (syntax-parse stx
    [(_ (style:style-spec ...)
        body)
     #'(let (style.binder ...) body)]))

(define ((styled-text #:size [size (current-font-size)]
                      #:color [color "black"]
                      #:face [face #f])
         . strs-or-picts)
  (define font-style
    ;; TODO: expand this further
    face)
  (define lines (split-lines strs-or-picts))
  (for/fold ([txt (blank 0 0)])
            ([line (in-list lines)])
    (define line-pict
      (apply hbl-append
             (for/list ([str-or-pict (in-list line)])
               (cond [(string? str-or-pict)
                      ;; TODO: handle angle?
                      (colorize (text str-or-pict font-style size)
                                color)]
                     [(pict? str-or-pict)
                      str-or-pict]
                     [else
                      (error "expected a string or pict argument")]))))
    (vl-append txt line-pict)))
