(define-module (schspec-iso))
(use-modules (specwriter) (ice-9 match) (sxml2))


;;                                                                          {{{1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(dbl top)


;; Document-Level Constructs                                                {{{1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (doc . rest) (sxml-with-attribute (cons 'doc rest) 'kind 'iso-specification))
  (dbl docctl)
  (dbl docbody)
    (dbl doccontent)
      (dbl docfront)
      (dbl docproper)
        (dbl docmain)
        (dbl docannex)
      (dbl docafter)


;; Document Metadata                                                        {{{1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dbl docinfo)
  (dbl doctitle)
    (dbl doctitleelem)
  (dbl docid)
    (dbl docidbase)
    (dbl docidrev)
  (dbl docedition)
  (dbl doctimestamp)


;; Structural Constructs                                                    {{{1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dbl preface)
(dblt clause)
(dbl uclause)
(dblt annex)
(dbl asec)
(dbl hdr)
(dbl uhdr)
(dbl ahdr)
(dbl termdef)

;; Normative titled clause/subclause
(define (§ title . rest)
  (sxml-with-attribute (apply clause (cons title rest)) 'kind 'normative))

;; Informative titled clause/subclause
(define (§i title . rest)
  (sxml-with-attribute (apply clause (cons title rest)) 'kind 'informative))

;; Normative annex
(define (§a title . rest)
  (sxml-with-attribute (apply annex (cons title rest)) 'kind 'normative))

;; Informative annex
(define (§ai title . rest)
  (sxml-with-attribute (apply annex (cons title rest)) 'kind 'informative))

;; Normative untitled clause/subclause
(define (¶ . rest)
  (sxml-with-attribute (apply uclause rest) 'kind 'normative))

;; Informative untitled clause/subclause
(define (¶i . rest)
  (sxml-with-attribute (apply uclause rest) 'kind 'informative))


;; Block-Level Constructs                                                   {{{1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dbl p)
(dbl ul)
(dbl ol)
  (dbl li)
(dblt figure)
(dblt table)
  (dbl tablekey)
(dbl mathb)
(dbl note)
(dbl example)


;; Block-Level Constructs: Tabulars                                         {{{1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dbl tabular)
  (dbl tr)
    (dbl td)
    (dbl th)


;; Block-Level Constructs: Graphics and Visualisation                       {{{1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ...


;; Inline-Level Constructs                                                  {{{1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dbl em)
(dbl tt)
(dbl procn)
(dbl kw)
(dbl math)
(dbl footnote)
(dbl footnoteref)


;; Prowords                                                                 {{{1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dpword shall       "shall")      ;; requirement
(dpword shall-not   "shall not")  ;;
(dpword should      "should")     ;; recommendation
(dpword should-not  "should not") ;;
(dpword may         "may")        ;; permission
(dpword may-not     "may not")    ;;
(dpword can         "can")        ;; possibility
(dpword cannot      "cannot")     ;;
(dpword must        "must")       ;; external constraint


;; Numbering                                                                {{{1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (step-context-element elem is-annex?)
  (if (not is-annex?) (1+ elem)
      (if (number? elem) #\A (integer->char (1+ (char->integer elem))))))

(define (step-context ctx is-annex?)
  (match ctx
         ((x . xs)
          (cons (step-context-element x is-annex?) xs))
         (_ (error (list "cannot step" ctx)))))

(define (enter-context ctx)
  (cons 0 ctx))

(define (output-context ctx)
  (string-join (map (lambda (e)
                      (cond
                        ((number? e) (number->string e))
                        ((char? e) (string e))
                        ((string? e) e)))
                    (reverse ctx)) "."))

(define* (number-clauses s #:optional (ctx '(0)))
  (match s
         (( ((and sym (or 'clause 'annex 'termdef)) ('@ . attrs) ('hdr ('title t)) . xs) . ys)
          (let* ((is-annex? (eq? sym 'annex))
                 (new-ctx (step-context ctx is-annex?))
                 (number (output-context new-ctx)))
            `((,sym (@ (number ,number) . ,attrs)
                   (hdr
                     (number ,number)
                     (sp ". ")
                     (title ,t))
               ,@(number-clauses xs (enter-context new-ctx)))
              . ,(number-clauses ys new-ctx))))
         (( ((? symbol? h) . xs) . ys)
          (cons (cons h (number-clauses xs ctx))
                (number-clauses ys ctx)))
         (((? symbol? x) . xs)
          (cons x
                (number-clauses xs ctx)))
         (("" . xs)
          (number-clauses xs ctx))
         (_ s)))

(define* (number-figures s #:optional (ctx '(0)))
  (match s
         (( ((and f (or 'table 'figure)) ('hdr ('title t)) . xs) . ys)
          (let* ((new-ctx (step-context ctx #f))
                 (number  (output-context new-ctx)))
            `((,f (@ (number ,number))
                      (hdr
                        (pfx ,(if (eq? f 'figure) "Figure " "Table "))
                        (number ,number)
                        (sp ". ")
                        (title ,t))
              ,@(number-figures xs (enter-context new-ctx)))
              . ,(number-figures ys new-ctx))))
         (( ((? symbol? h) . xs) . ys)
          (cons (cons h (number-figures xs ctx)) (number-figures ys ctx)))
         (((? symbol? x) . xs)
          (cons x
                (number-figures xs ctx)))
         (_ s)))

(define (number-things s) (number-figures (number-clauses s)))


;;                                                                          {{{1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (xml-top document)
  `(*TOP*
     (*PI* xml-stylesheet "href=\"schspec-iso-xml.css\" type=\"text/css\"")
     (top (@ (@ (*NAMESPACES* (*IMPLICIT* "https://www.devever.net/ns/schspec-iso" *IMPLICIT*))))
          ,document)))

(export top doc docctl docbody doccontent docfront docproper docmain docannex docafter docinfo doctitle doctitleelem docid docidbase docidrev docedition doctimestamp preface clause uclause annex asec hdr uhdr ahdr termdef § §i §a §ai ¶ ¶i p ul ol li figure table tablekey mathb note example tabular tr td th em tt procn kw math footnote footnoteref shall shall-not should should-not may may-not can cannot must number-clauses number-figures number-things xml-top)
