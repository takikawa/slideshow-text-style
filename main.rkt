#lang racket/base

;; A pict text styling library for slideshow

(require pict
         (only-in scribble/text split-lines)
         (for-syntax racket/base
                     syntax/parse))

(provide with-text-style)

(begin-for-syntax
  (define-syntax-class style-spec
    #:description "style specification"
    #:attributes (result)
    (pattern (name:id options:style-options)
             #:with result #'[#:kws name #f options])
    (pattern ((name:id super:id) options:style-options)
             #:with result #'[#:kws name super options])
    ;; TODO: implement this case, how should it interact with defaults?
    #;
    (pattern font-object-or-style))

  (define-splicing-syntax-class style-options
    (pattern (~seq (~or (~optional (~seq #:face face:expr))
                        (~optional (~seq #:italic? italic?:expr))
                        (~optional (~seq #:bold? bold?:expr))
                        (~optional (~seq #:color color:expr))
                        (~optional (~seq #:size size:expr))
                        (~optional (~seq #:line-sep line-sep:expr))
                        (~optional (~seq #:left-pad left-pad:expr))
                        (~optional (~seq #:transform xform:expr))
                        (~optional (~seq #:h-append h-append:expr))
                        (~optional (~seq #:v-append v-append:expr)))
                   ...)))

  (define-splicing-syntax-class style-defaults
    #:attributes (options)
    (pattern (~seq) #:with options #'())
    (pattern (~seq #:defaults [options:style-options]))))

(define-syntax (with-text-style stx)
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
    [(_ st ([#:kws name #f options] style ...) body ...)
     (let ([name (st . options)])
       (with-stylers* st (style ...) body ...))]
    [(_ st ([#:kws name super options] style ...) body ...)
     (let ([name
            (make-keyword-procedure
             (λ (kws kw-args . rst)
               (keyword-apply super kws kw-args rst . options)))])
       (with-stylers* st (style ...) body ...))]))

;; A double-curried function that produces styled text picts
;; The first level of currying is for default arguments.
;; The second level is for style settings for a given styler.
(define (((styled-text ;; NB: size defaults to 32 since (current-font-size)
                       ;;     defaults to it. We don't directly use that parameter
                       ;;     here to avoid an unnecessary dependency on slideshow
                       #:size [default-size 32]
                       #:color [default-color "black"]
                       #:face [default-face null]
                       #:line-sep [default-ls 0]
                       #:left-pad [default-lp 0]
                       #:bold? [default-bold? #f]
                       #:italic? [default-italic? #f]
                       #:transform [default-transform values]
                       #:h-append [default-h-append hbl-append]
                       #:v-append [default-v-append vl-append])
          #:size [*size default-size]
          #:color [*color default-color]
          #:face [*face default-face]
          #:line-sep [*line-sep default-ls]
          #:left-pad [*left-pad default-lp]
          #:bold? [*bold? default-bold?]
          #:italic? [*italic? default-italic?]
          #:transform [*transform default-transform]
          #:h-append [*h-append default-h-append]
          #:v-append [*v-append default-v-append])
    ;; for overriding at specific call sites
    #:size [size *size]
    #:color [color *color]
    #:face [face *face]
    #:line-sep [line-sep *line-sep]
    #:left-pad [left-pad *left-pad]
    #:bold? [bold? *bold?]
    #:italic? [italic? *italic?]
    #:transform [transform *transform]
    #:h-append [h-append *h-append]
    #:v-append [v-append *v-append]
    ;; the actual strings/picts provided
    . strs-or-picts)
  (define font-style
    `(,@(if bold? '(bold) '())
      ,@(if italic? '(italic) '())
      .
      ,face))
  (define lines (split-lines strs-or-picts))
  (define text-fn
    (cond [(number? size) text]
          [(and (pair? size)
		(eq? (car size) 'auto)
		(number? (cdr size)))
	   (λ (str f sz)
	     (auto-text str (cdr sz) f))]
	  [else (error "invalid size argument")]))
  (define base
    (for/fold ([txt (blank 0 0)])
              ([line (in-list lines)])
      (define line-pict
        (apply h-append
               (for/list ([str-or-pict (in-list line)])
                 (cond [(string? str-or-pict)
                        ;; TODO: handle angle?
                        (colorize (text-fn str-or-pict font-style size)
                                  color)]
                       [(pict? str-or-pict)
                        str-or-pict]
                       [else
                        (error "expected a string or pict argument")]))))
      (hbl-append (blank left-pad 1) ; just for padding
                  (v-append line-sep txt line-pict))))
  (transform base))

;; Works like `text` but tries to fit the rendered text in a space that is
;; `target` pixels wide
(define (auto-text str target [style null] [angle 0])
  (define initial-min 0)
  (define initial-max 1000)

  (define (close-enough? width target)
    (< (abs (- width target)) 10))

  (define (bin-search cur-min cur-max)
    (define next-size
      (inexact->exact
       (round (+ cur-min (/ (- cur-max cur-min) 2)))))
    (define width (pict-width (text str style next-size angle)))
    (cond [(close-enough? width target)
           (text str style next-size angle)]
          [(> width target)
           (bin-search cur-min next-size)]
          [(< width target)
           (bin-search next-size cur-max)]))

  (bin-search initial-min initial-max))
