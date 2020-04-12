(define-module (specwriter))
(use-modules (ice-9 match))

;; dbl: define block
;; usage: (dbl block-name)
;;
;; Defines a block construct. A function is defined with the given name:
;;   (block-name . args)
;; where args are the children of the block.
(define-syntax dbl
  (syntax-rules ()
    ((dbl sym)
     (define (sym . rest) (cons 'sym rest)))))

;; dblt: define block with title
;; usage: (dblt block-name)
;;
;; Defines a block construct. A function is defined with the given name:
;;   (block-name title . args)
;; where title is the title of the block and args are the children of the
;; block.
(define-syntax dblt
  (syntax-rules ()
    ((dblt sym)
     (define (sym title . rest) (cons 'sym (cons (list 'hdr (list 'title title)) rest))))))

;; dpword
(define-syntax dpword
  (syntax-rules ()
    ((dpword sym name)
     (define sym (list 'proword (list '@ (list 'type 'sym)) name)))))

;; This stores an index of defined terms.
(define sw-terms '())

;; dt: define term
;; usage: (dt sym name . body)
;;
;; Defines a term. sym is a symbol used to refer to the term; name is a natural
;; langauge string for the term; body is the body of the term and contains an
;; explanation of it.
;;
;; sym is defined, as well as the symbol sym with an 's' appended to it, which
;; can be used to generate plural references. sym itself should be singular.
(define-syntax dt
  (syntax-rules ()
    ((dt sym name . rest)
     (dt-dyn 'sym 'name (quote rest)))))
(define (dt-dyn-add sym value)
  (set! sw-terms (cons value sw-terms)))
(define (dt-dyn sym name rest)
  (dt-dyn* sym name 'singular rest)
  (dt-dyn* (string->symbol (string-append (symbol->string sym) "s")) (string-append name "s") 'plural rest))
(define (dt-dyn* sym name sp rest)
  (eval `(define ,sym (list 'term (list '@ (list 'sym (quote ,sym)) (list 'sp (quote ,sp))) ,name (lambda () (list ,@rest)))) (interaction-environment))
  (dt-dyn-add sym (eval sym (interaction-environment))))

(export dbl dblt dpword dt sw-terms)
