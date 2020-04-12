(define-module (specwriter-xml))
(use-modules (ice-9 match) (sxml2))

;; xlate-inline-desc
(define (sw-xml-xlate-inline-desc desc)
  (match desc
         (('term sym name . rest)
          name)
         (((? symbol? sym) . rest)
          (cons sym (map sw-xml-xlate-inline-desc rest)))
         (_
          desc)))

;; sw-xml-xlate
;;
;; specwriter-document -> sxml-document
(define (sw-xml-xlate e)
  (match e

         ;; TERM: ('term sym name 'sigular|'plural desc . rest)
         #;(('term ('@ ('sym sym) ('sp sp)) name desc . rest)
          `(term (@ (sym ,sym)
                    (sp ,sp)
                    (desc ,(map sw-xml-xlate-inline-desc (desc))))
                 ,name))

         ;; SECTION: ('clause title kind . rest)
         #;(((and f (or 'clause 'annex)) ('@ . attrs) . rest)
          `(,f (@ . ,attrs)
                ,@(map sw-xml-xlate rest)))

         ;; BLOCK: (sym . rest) => (sym . xlate rest)
         #;(((? symbol? sym) . rest)
          (cons sym (map sw-xml-xlate rest)))

         ;; LIST: (lists...) => (xlated lists...)
         #;((? list? xs)
          (map sw-xml-xlate xs))

         ;; OTHER
         (_
           e)))

;; sxml-document -> sxml-top
(define (sw-xml-top document)
  `(*TOP*
     (*PI* xml-stylesheet "href=\"schspec-xml.css\" type=\"text/css\"")
     (top (@ (@ (*NAMESPACES* (*IMPLICIT* "https://www.devever.net/ns/schspec-iso" *IMPLICIT*))))
          ,(sw-xml-xlate document))))

(export sw-xml-top sw-xml-xlate)
