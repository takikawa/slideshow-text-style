#lang racket/base

;; A pict text styling library for slideshow

(require pict
         slideshow/base
         (only-in scribble/text split-lines)
         (for-syntax racket/base
                     syntax/parse))

(provide with-stylers)

(begin-for-syntax
  (define-syntax-class style-spec
    #:attributes (result)
    (pattern (name:id options:style-options)
             #:with result #'[#:kws name options])
    ;; TODO: implement this case, how should it interact with defaults?
    #;
    (pattern font-object-or-style))

  (define-splicing-syntax-class style-options
    (pattern (~seq (~or (~optional (~seq #:face face:expr))
                        (~optional (~seq #:color color:expr))
                        (~optional (~seq #:size size:expr))
                        (~optional (~seq #:line-sep line-sep:expr)))
                   ...)))

  (define-splicing-syntax-class style-defaults
    #:attributes (options)
    (pattern (~seq) #:with options #'())
    (pattern (~seq #:defaults [options:style-options]))))

(define-syntax (with-stylers stx)
  (syntax-parse stx
    [(_ maybe-defaults:style-defaults
        (style:style-spec ...)
        body:expr ...)
     (with-syntax ([(st) (generate-temporaries '(st))])
       #'(let ()
           (define st (styled-text . maybe-defaults.options))
           (with-stylers* st (style.result ...) body ...)))]))

(define-syntax with-stylers*
  (syntax-rules ()
    [(_ st () body ...)
     (begin body ...)]
    [(_ st ([#:kws name options] style ...) body ...)
     (let ([name (st . options)])
       (with-stylers* st (style ...) body ...))]))

;; A double-curried function that produces styled text picts
;; The first level of currying is for default arguments.
;; The second level is for style settings for a given styler.
(define (((styled-text #:size [default-size (current-font-size)]
                       #:color [default-color "black"]
                       #:face [default-face null]
                       #:line-sep [default-ls 0])
          #:size [*size default-size]
          #:color [*color default-color]
          #:face [*face default-face]
          #:line-sep [*line-sep default-ls])
         ;; for overriding at specific call sites
         #:size [size *size]
         #:color [color *color]
         #:face [face *face]
         #:line-sep [line-sep *line-sep]
         ;; the actual strings/picts provided
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
    (vl-append line-sep txt line-pict)))
