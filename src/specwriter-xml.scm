(define-module (specwriter-xml))
(use-modules (ice-9 match) (sxml2))

;; sxml-document -> sxml-top
(define (sw-xml-top document)
  `(*TOP*
     (*PI* xml-stylesheet "href=\"schspec-xml.css\" type=\"text/css\"")
     (top (@ (@ (*NAMESPACES* (*IMPLICIT* "https://www.devever.net/ns/schspec-iso" *IMPLICIT*))))
          ,document)))

(export sw-xml-top)
