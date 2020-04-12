(define-module (schspec-regdef))
(use-modules (ice-9 match))

(define (reg-field-item->sxml rfi)
  (match rfi
         (('bitrange lo hi)
          `(bitrange (br-lo ,lo) (br-hi ,hi)))
         (('bitrange b)
          `(bitrange (br-single ,b)))
         (('title t)
          `(rb-title ,t))
         (('access a)
          `(rb-access ,a))
         (('desc d)
          `(rb-desc ,d))
         (_ rfi)))

(define (reg-field->sxml rf)
  (match rf
         (('field . xs)
          (cons 'field (map reg-field-item->sxml xs)))))

(define* (rb-offset->sxml ro #:optional (ind #f))
  (match ro
         (('+ x y)
          (string-append "(" (rb-offset->sxml x) "+" (rb-offset->sxml y) ")"))
         (('* x y)
          (string-append (rb-offset->sxml x) "*" (rb-offset->sxml y)))
         (('* x . xs)
          (string-append (rb-offset->sxml x) "*" (rb-offset->sxml (cons '* xs))))
         (('lsh x y)
          (string-append "(" (rb-offset->sxml x) "<<" (rb-offset->sxml y) ")"))
         ((? symbol? x)
          (symbol->string x))
         ((? number? x)
          (if ind (format #f "~:@(~4x~)h" x) (format #f "~:@(~x~)h" x)))))

(define (reg-item->sxml ri)
  (match ri
         (('field bitrange (? symbol? mnem) . xs)
          (reg-field->sxml (cons 'field (cons (list 'mnem mnem) (cons (if (pair? bitrange) (cons 'bitrange bitrange) (list 'bitrange bitrange)) xs)))))
         (('offset o)
          `(rb-offset ,(rb-offset->sxml o #t)))
         (('width w)
          `(rb-width ,w))
         (('access a)
          `(rb-access ,a))
         (_ ri)))

(define (reg->sxml r)
  (match r
         (('reg (? symbol? s) . xs)
          (reg->sxml (cons 'reg (cons (list 'mnem s) xs))))
         (('reg . xs)
          (cons 'reg (map reg-item->sxml xs)))))

(define (regblock-item->sxml rbi)
  (match rbi
         (('reg . xs)
          (reg->sxml rbi))
         (_ rbi)))

(define (regblock->sxml rb)
  (match rb
         (('regblock . xs)
          (cons 'regblock (map regblock-item->sxml xs)))))

(export regblock->sxml)
