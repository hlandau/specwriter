;;;; (sxml2) -- a revision of (sxml simple)
;;;; vim: fdm=marker
;;;;
;;;; 	Copyright (C) 2009, 2010, 2013  Free Software Foundation, Inc.
;;;;    Modified 2018 by Hugo Landau <hlandau@devever.net>.
;;;;    Modified 2004 by Andy Wingo <wingo at pobox dot com>.
;;;;    Originally written by Oleg Kiselyov <oleg at pobox dot com> as SXML-to-HTML.scm.
;;;; 
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;; This module defines interfaces for XML parsing and serialization. It is
;; derived from (sxml simple) but modified to actually handle namespaces
;; properly.
(define-module (sxml2))
(use-modules
  (sxml ssax input-parse)
  (sxml ssax)
  (sxml transform)
  (ice-9 match)
  (srfi srfi-1)
  (srfi srfi-13))

;; TODO:
;;   - Fix xmlns autoattachment to more optimally autodeclare namespaces


;; SXML Specification                                                      {{{1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ;; Top level document.
;;  <TOP>               ::=  ('*TOP* <annotations>? <pi>* <comment>* <element>)
;;
;;  ;; Processing instruction.
;;  <pi>                ::=  ('*PI* <pi-target> <annotations>? <pi-content>)
;;  <pi-target>         ::=  <fsymbol>
;;  <pi-content>        ::=  string
;;
;;  ;; Entity reference.
;;  <entity>            ::=  ('*ENTITY* <entity-name>)
;;  <entity-name>       ::=  fstring   ;; e.g. "amp", "#1234"
;;
;;  ;; Comment.
;;  <comment>           ::=  ('*COMMENT* string)
;;
;;  ;; Annotations are used to describe metadata which is not serialized.
;;  <annotations>       ::=  ('@ <annotation>*)
;;  <annotation>        ::=  <annotation-ns> / <annotation-unk>
;;
;;  <annotation-ns>     ::=  ('*NAMESPACES* <ns-assoc>*)
;;
;;  <annotation-unk>    ::=  (<annotation-name> . <annotation-data>)
;;  <annotation-name>   ::=  fsymbol
;;  <annotation-data>   ::=  any
;;
;;  ;; Things which may be in an element.
;;  <item>              ::=  <element> / <pi> / <entity> / <comment> / <itemable> / <stringable> / <raw>
;;
;;  ;; Element.
;;  <element>           ::=  (<element-name> <attributes>? <item>*)
;;  <element-name>      ::=  <name>
;;
;;  ;; Element attributes.
;;  <attributes>        ::=  ('@ <attribute>* <annotations>?)
;;  <attribute>         ::=  (<attribute-name> <attribute-value>? <annotations>?)
;;  <attribute-name>    ::=  <name>
;;  <attribute-value>   ::=  <stringable>
;;
;;  ;; Names.
;;  <name>              ::=  <name-unq> / <name-qual>
;;  <name-unq>          ::=  symbol         ;; A symbol which does not contain a colon.
;;  <name-qual>         ::=  symbol(<ns-ref> ":" <name-unq>)
;;
;;  ;; Namespace references.
;;  <ns-ref>            ::=  <ns-ref-uri> / <ns-ref-shortcut> / '*IMPLICIT*
;;  <ns-ref-uri>        ::=  symbol(URI)    ;; A symbol which is a URI string.
;;  <ns-ref-shortcut>   ::=  symbol         ;; A symbol which does not contain a colon.
;;
;;  ;; Namespace definitions.
;;  <ns-assoc>          ::=  (<ns-ref-in> <ns-assoc-uri> <ns-ref-out>)
;;  <ns-assoc-uri>      ::=  string
;;  <ns-ref-in>         ::=  symbol / '*IMPLICIT*
;;  <ns-ref-out>        ::=  symbol / '*IMPLICIT*
;;
;;  ;; Raw output.
;;  ;;     Enables normal serialization processes to be bypassed and unescaped output
;;  ;;     to be generated. Useful if you already have serialized XML. WARNING: Use of
;;  ;;     this allows malformed output to be generated.
;;  <raw>               ::=  ('*RAW* string)
;;
;;  ;; Itemable.
;;  ;;     Something which can be converted to <item> for serialization.
;;  ;;     Any such thing can be used inside an element.
;;  <itemable>          :=   TODO
;;
;;  ;; Stringable.
;;  ;;     Something which can be converted to a string for serialization.
;;  ;;     Any such thing can be used inside an element or as an attribute value.
;;  <stringable>        ::=  string / number
;;
;;  ;; fstring indicates something which is always parsed as a string and for
;;  ;; which string representation is preferred, but for which serializers
;;  ;; should accept symbols also. fsymbol indicates the opposite.
;;  <fstring>           ::=  string / symbol
;;  <fsymbol>           ::=  symbol / string
;;
;; Changes from the traditional SXML standard:
;;
;;   Namespace annotations go on the top-level element, not in *TOP*.
;;   i.e., this is not supported: (*TOP* (@ (*NAMESPACES* ...)))
;;                  instead, use: (*TOP* (foo (@ (@ (*NAMESPACES* ...)))))
;;
;;   Also, namespace declarations are now used like this:
;;
;;     (pfx-in uri pfx-out)
;;
;;   where pfx-in is a symbol representing the shortcut prefix which may be
;;   used in the SXML representation, pfx-out is a symbol representing a prefix
;;   which should be used in the serialized XML representation, and uri is a
;;   string containing the namespace URI.
;;
;; Principles of namespace processing:
;;   Unless otherwise specified, always parse namespaces to explicit qualifications
;;   on names:
;;
;;       <html xmlns="http://www.w3.org/1999/xhtml" lang="en"><head/></html>
;;
;;     becomes
;;
;;       (http://www.w3.org/1999/xhtml:html
;;         (@ (http://www.w3.org/1999/xhtml:lang "en"))
;;         (http://www.w3.org/1999/xhtml:head))
;;
;;     which may be serialized as-is.
;;
;;   In all cases, parsing to SXML and then serializing the result (with either
;;   the default options, or the same namespace options passed to both the
;;   parse and serialization functions) MUST result in semantically identical
;;   XML.
;;
;;   Serialization is also highly explicit, for example the above SXML would
;;   (by default) serialize as:
;;
;;     <a:html xmlns:a="http://www.w3.org/1999/xhtml" a:lang="en">
;;       <a:head/>
;;     </a:html>
;;
;;   With the default options, namespace prefixes are automatically generated
;;   (a, b, c, etc.) and are meaningless. All of the namespaces used in the
;;   SXML have their prefixes declared at the root element. An implicit
;;   namespace is not set.
;;
;;   This serializer does not permit the non-use of namespaces. If an unqualified
;;   name appears in the input to the serializer, it attempts to convert it
;;   to a qualified name based on the passed options; if it fails to do so,
;;   it generates an error. The default options to the serializer cause all
;;   qualification attempts to fail, so invocation of the serializer on the
;;   following with the default options will produce an error:
;;
;;     (html)
;;
;;   Similarly, a shortcut-qualified name will be resolved to a fully URI-qualified
;;   name by the serializer. Thus, when invoked with the default options, this is
;;   also an error:
;;
;;     (h:html)
;;
;;   In other words, the serializer rewrites any input which uses
;;   <ns-ref-shortcut> or <name-unq> in the grammar above to use a <name-qual>
;;   based on a <ns-ref-uri>, and always errors if it is unable to do so.
;;
;;   Because shortcuts are used only to resolve to a fully-qualified internal
;;   representation during serialization, the use of a shortcut prefix in a
;;   name does not guarantee that a prefix of that name will be used in the
;;   output. For example:
;;
;;     (*TOP*
;;       (h:html
;;         (@ (@ (*NAMESPACES*
;;                 (h "http://www.w3.org/1999/xhtml")
;;                 (g "http://www.w3.org/1999/xhtml"))))))
;;
;;   could be serialized as
;;
;;     <h:html xmlns:h="http://www.w3.org/1999/xhtml" xmlns:g="http://www.w3.org/1999/xhtml"/>
;;
;;   or as
;;
;;     <g:html xmlns:h="http://www.w3.org/1999/xhtml" xmlns:g="http://www.w3.org/1999/xhtml"/>
;;
;;   Note that the serializer will NOT elide namespace prefix declarations that
;;   do not appear to be used in the output, as there exist XML-based standards
;;   which reference namespace prefixes in other contexts, such as in attribute
;;   values. Thus, the serializer is unable to know with certainty that
;;   removing a prefix declaration is safe.
;;
;;   Where the use of an implicit namespace is desired in consumed input
;;   the following form should be used:
;;
;;     (*TOP*
;;       (html
;;         (@ (@ (*NAMESPACES*
;;                 (*IMPLICIT* "http://www.w3.org/1999/xhtml"))))))
;;
;;   Note that use of an implicit namespace for serializer input and use for the serializer
;;   output are decoupled, so this would still generate:
;;
;;     <a:html xmlns:a="http://www.w3.org/1999/xhtml"/>
;;
;;   If the use of an implicit namespace is desired in the output,
;;   the following form should be used:
;;
;;     (*TOP*
;;       (h:html
;;         (@ (@ (*NAMESPACES*
;;                 (h "http://www.w3.org/1999/xhtml" *IMPLICIT*))))))
;;
;;   which would produce the following output:
;;
;;     <html xmlns="http://www.w3.org/1999/xhtml"/>
;;
;;   Of course, the above two examples may be combined using a namespace
;;   declaration (*IMPLICIT* "http://www.w3.org/1999/xhtml" *IMPLICIT*).
;;
;;   In other words, the third argument in a namespace declaration determines
;;   what is used in the generated output; the first argument determines what
;;   is expected in the passed input. If the third argument is omitted, a
;;   suitable prefix name is autogenerated. The serializer reserves the right
;;   in future to autogenerate any prefix name which does not conflict with
;;   another declared namespace, including *IMPLICIT*.
;;
;;   In some cases, it is necessary to use an XML serializer to generate
;;   suboptimal output with regard to closing tag elision, namely where it is
;;   desired that the document also parse as valid legacy HTML. For example, an
;;   HTML <textarea> must always be closed explicitly even if it is empty. This
;;   may be facilitated by placing (*RAW* "") (an empty raw item) inside the
;;   element. Such an element will never have its closing tag elided. Thus, the
;;   serializer could be used with e.g. a preprocessing pass which looks for
;;   HTML elements which must always be explicitly closed and inserts such an
;;   item.
;;
;;   The internal representations used by the serializer are in three phases:
;;
;;     Phase 1  - User input. elements may have a mixture of fully-qualified
;;                names, unqualified names and shortcut-qualified names.
;;                Examples:
;;
;;                  (http://www.w3.org/1999/xhtml:html)
;;                  (html)
;;                  (h:html)
;;
;;     Phase 2  - All elements have fully-qualified names, without exception.
;;                Examples:
;;
;;                  (http://www.w3.org/1999/xhtml:html)
;;
;;                A function expecting phase 2 data will reject:
;;
;;                  (html)
;;                  (h:html)
;;
;;     Phase 2a - A transformation of phase 2 data in which all undeclared namespaces
;;                have declarations added with autogenerated prefixes. Any
;;                namespaces which were already declared without prefix names
;;                specified have one autogenerated. In this representation, all
;;                elements and attributes have URI-qualified names, and output
;;                prefixes exist in the namespace declarations for all URI
;;                prefixes found in the tree, ready for use by the phase 2->3
;;                mapping process.
;;
;;     Phase 3  - Elements have shortcut-qualified names matching
;;                the prefix which will actually be used for output;
;;                elements which will have an implicit namespace have
;;                unqualified names. Examples:
;;
;;                  (h:html)
;;                  (html)
;;
;;                A function expecting phase 3 data will reject:
;;
;;                  (http://www.w3.org/1999/xhtml:html)
;;
;;                NOTE: It is erroneous to pass a tree to a function expecting
;;                phase 3 input where that tree contains elements or attributes
;;                with shortcut prefixes which are not registered in namespace
;;                declarations in the tree. However, such a function is not
;;                obliged to verify that the input is valid in this way.
;;                Moreover, functions accepting phase 3 input are entitled to
;;                assume that the shortcut prefixes match those which are to be
;;                generated in the output XML and output them verbatim.
;;
;;                In other words, phase 3 data is ready for direct
;;                serialization to XML and functions accepting phase 3 data are
;;                entitled to assume that this is the case. Namespace
;;                declarations will be converted to xmlns declarations, it is
;;                assumed that all necessary namespace declarations exist,
;;                otherwise invalid XML may be produced.


;; Access Utilities                                                        {{{1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Given an element, returns a list of its attributes or '().
;;
;; (tree:<element>) -> (<attribute>...)
(define (sxml-attributes tree)
  (match tree
         (((? symbol? _) ('@ . attrs) . _)  attrs)
         (_  '())))

;; Given an element and a symbol, returns the attribute value
;; for the attribute with that attribute name, or #f.
;;
;; (k:symbol tree:<element>) -> <attribute-value> | #f
(define (sxml-attribute k tree)
  (let ((r (assq k (sxml-attributes tree))))
    (if r (cdr r) r)))

;; Given an element, returns the annotations list for the
;; element, or '().
;;
;; (tree:<element>) -> (<annotation>*)
(define (sxml-annotations tree)
  (or (sxml-attribute '@ tree) '()))

;; Given an element and a symbol, returns the annotation data for the
;; annotation with that name, or #f.
;;
;; (k:symbol tree:<element>) -> <annotation-data> | #f
(define (sxml-annotation k tree)
  (let ((r (assq k (sxml-annotations tree))))
    (if r (cdr r) r)))

;; Given an element, returns the list of namespace declarations attached
;; directly to that element, or '().
(define (sxml-namespaces tree)
  (or (sxml-annotation '*NAMESPACES* tree) '()))

;; Given a symbol, returns true iff that symbol is for a special construct
;; and is not a valid element tag name.
;;
;; (sym: symbol) -> bool
(define (special-symbol? sym)
  (or (eq? sym  '*PI*)
      (eq? sym  '*COMMENT*)
      (eq? sym  '*ENTITY*)
      (eq? sym  '*RAW*)
      (eq? sym  '*TOP*)))

;; Given a value, returns a boolean indicating whether the element is a SXML
;; element (and not a special construct).
;;
;; (tree:any) -> bool
(define (element? tree)
  (and (pair? tree)
       (symbol? (car tree))
       (not (special-symbol? (car tree)))))

;; Given a value, returns a boolean indicating whether the value is a special
;; construct with the given symbol.
;;
;; (sym:symbol tree:any) -> bool
(define (element=? sym tree)
  (and (pair? tree)
       (eq? sym (car tree))))

;; Returns a function which returns a higher integer each time it is called.
;;
;; [impure] () -> int
(define* (make-counter #:optional (init -1))
  (lambda ()
    (set! init (1+ init))
    init))

;; Returns the root element given a <TOP>.
;;
;; (top:<TOP>) -> <element>
(define (sxml-top-element top)
  (match top (('*TOP* _ ... body) body)))

;; Given an <element>, switches out the attributes
;; for the given list of attributes.
;;
;; (elem:<element> new-attrs:(<attribute>...)) -> <element>
(define (sxml-with-attributes elem new-attrs)
  (match elem
         (( (? symbol? name) ('@ . attrs) . body)
          `(,name (@ ,@new-attrs) ,@body))
         (( (? symbol? name) . body)
          `(,name (@ ,@new-attrs) ,@body))))

;; Given an <element> and a symbol, switches out the
;; attribute with the given name for the given value
;; and returns the resulting <element>.
;;
;; (elem:<element> name:<symbol> value:<attribute-value>) -> <element>
(define (sxml-with-attribute elem name value)
  (sxml-with-attributes elem
    (cons (list name value)
            (remove (lambda (x)
                      (and (pair? x) (eq? (car x) name)))
                    (sxml-attributes elem)))))

;; Given an <element>, switches out the annotations
;; for the given list of annotations.
;;
;; (elem:<element> new-annotations:(<annotation>...)) -> <element>
(define (sxml-with-annotations elem new-annotations)
  (sxml-with-attributes elem
    (cons (cons '@ new-annotations)
          (remove (lambda (x)
              (match x
                     (('@ . _)  #t)
                     (_         #f)))
              (sxml-attributes elem)))))

;; Given an <element> and a symbol, switches out the
;; annotation with the given name for the given value
;; and returns the resulting <element>. (name . data).
;;
;; (elem:<element> name:<symbol> data:<annotation-data>) -> <element>
(define (sxml-with-annotation elem name data)
  (sxml-with-annotations elem
    (cons (cons name data)
      (remove (lambda (x)
                (and (pair? x) (eq? (car x) name)))
              (sxml-annotations elem)))))

;; Given a <TOP>, switches out the body for new-root
;; and returns the new <TOP>.
;;
;; (top:<TOP> new-root:<element>) -> <TOP>
(define (sxml-with-top-element top new-root)
  (match top
         (('*TOP* mid ... old-root)
          (cons '*TOP* (append mid (list new-root))))))

;; Skips to the body of an element, after the attributes.
;;
;; (elem:<element>) -> list
(define (sxml-body elem)
  (match elem
         (((? symbol? s) ('@ . _) . rest)   rest)
         (((? symbol? s) . rest)            rest)
         (_                                 elem)))


;; Serialization                                                           {{{1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Serialization Preprocessing: General {{{2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lookup a prefix in an nsctx.
(define (nsctx-lookup nsctx prefix)
  (assoc prefix nsctx))

;; Given an evolve function, returns a function which descends into a new
;; namespace scope (i.e., a level in the XML hierarchy in which the set of
;; valid prefixes is potentially different — in other words, a new element). A
;; new nsctx is returned which is evolved by a call to the evolve function.
;;
;; (evolve-func:(k v) -> y) -> (nsctx:nsctx attrs) -> nsctx':nsctx
(define (nsctx-descend evolve-func)
  (define (descend nsctx attrs)
    (cond
      ((pair? attrs)
       (let* ((attr  (car attrs))
              (aname (car attr)))
         (case aname
           ((@)    (let rec ((annos (cdr attr)))
                     (cond
                       ((pair? annos)
                        (let* ((anno       (car annos))
                               (anno-name  (car anno)))
                          (case anno-name
                            ((*NAMESPACES*)
                              (let rec2 ((nsdefs (cdr anno)))
                                (if (pair? nsdefs)
                                  (evolve-func (car nsdefs) (rec2 (cdr nsdefs)))
                                  ;(acons (caar nsdefs) (cdar nsdefs)
                                  ;       (rec2 (cdr nsdefs)))
                                  nsctx)))
                            (else
                              (rec (cdr annos)))))))))

           (else   (descend nsctx (cdr attrs))))))
      (else nsctx)))
  descend)

;; The passed nsctx should already have been descended into these attributes.
;;
;; (attrs:(<attribute>...) name-func:"(attr-name:symbol nsctx:nsctx) -> symbol" nsctx:nsctx) -> (<attribute>...)
(define (sxml-preprocess-attributes attrs name-func nsctx)
  (map (lambda (attr)
    (cond
      ((not (pair? attr))         attr)   ;; ???
      ((eq? '@ (car attr))        attr)   ;; annotation, skip
      (else                       (cons (name-func (car attr) nsctx) (cdr attr)))))
   attrs))

;; nsctx is an alist mapping a prefix symbol or '*IMPLICIT* to URI strings.
(define* (sxml-preprocess tree descend-func name-func #:optional (nsctx '()))
  (cond
    ((pair? tree)
     (if (symbol? (car tree))
         ;; An element or special directive.
         (let ((tag (car tree)))
           (case tag
             ((*TOP*)     (cons '*TOP* (map (lambda (k) (sxml-preprocess k descend-func name-func nsctx)) (cdr tree))))
             ((*COMMENT*) tree)
             ((*RAW*)     tree)
             ((*ENTITY*)  tree)
             ((*PI*)      tree)
             (else
               (let* ((elems (cdr tree))
                      (attrs (and (pair? elems) (pair? (car elems))
                                  (eq? '@ (caar elems))
                                  (cdar elems)))
                      (nsctx' (descend-func nsctx attrs)))
                 (cons (name-func tag nsctx')
                       (if attrs (cons (cons '@ (sxml-preprocess-attributes attrs name-func nsctx'))
                                       (sxml-preprocess (cdr elems) descend-func name-func nsctx'))
                                 (sxml-preprocess elems descend-func name-func nsctx')))))))
         ;; A nodelist.
         (map (lambda (x) (sxml-preprocess x descend-func name-func nsctx)) tree)))
     (else tree)))

;; The passed nsctx should already have been descended into this element.
(define (sxml-preprocess-name transform-func)
  (lambda (name nsctx)
    (cond
      ((symbol? name)
       (let* ((sname (symbol->string name))
              (idx   (string-rindex sname #\:)))
         (if (eq? idx #f)
           (transform-func '*IMPLICIT* sname nsctx)
           (transform-func
             (string->symbol (substring sname 0 idx))
             (substring sname (1+ idx))
             nsctx))))
      (else
        (error "name must be a symbol" name)))))

(define (sxml-get-prefix name)
  (let* ((sname (symbol->string name))
         (idx   (string-rindex sname #\:)))
    (if (eq? idx #f)
      #f
      (string->symbol (substring sname 0 idx)))))


;; Serialization Preprocessing: Phase 1->Phase 2 {{{2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sxml-phase-1-name-to-phase-2 prefix sname nsctx)
  (if (nil? (string-index (symbol->string prefix) #\:))
    (let ((result (nsctx-lookup nsctx prefix)))
      (or (and result (string->symbol (string-append (cadr result) ":" sname)))
          (error "cannot find namespace prefix in context" prefix sname)))
    (string->symbol (string-append (symbol->string prefix) ":" sname))))

(define* (sxml-phase-1-to-phase-2 tree #:optional (nsctx '()))
         (sxml-preprocess tree
           (nsctx-descend (lambda (nsdef rest) (acons (car nsdef) (cdr nsdef) rest)))
                                  ;(acons (caar nsdefs) (cdar nsdefs)
           (sxml-preprocess-name sxml-phase-1-name-to-phase-2)
           nsctx))


;; Serialization Preprocessing: Phase 2->Phase 2a {{{2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Scans for namespace prefixes in the form "z[1-9]*[0-9]" and returns the
;; highest numeric value found. If none are found, returns -1.
;;
;; (tree:sxml-tree) -> highest-number:number
(define sxml-find-highest-autogenerated-prefix-number
  (letrec
        ((charset-19      (string->char-set "123456789"))

         (for-namespaces  (lambda (namespaces)
                            (fold (lambda (q prev) (max prev (for-namespace q))) -1 namespaces)))

         (for-symbol      (lambda (sym)
                            (let ((str (symbol->string sym)))
                              (or (and (> (string-length str) 1)
                                       (eq?                #\z        (string-ref str 0))
                                       (char-set-contains? charset-19 (string-ref str 1))
                                       (string->number (substring str 1)))
                                  -1))))

         (for-namespace   (lambda (namespace)
                            (match namespace
                                   ((pfx-in uri (? symbol? pfx-out))   (for-symbol pfx-out))
                                   (_                                 -1))))

         (for-elems       (lambda (elems)
                            (fold (lambda (q prev) (max prev (for-elem q))) -1 elems)))

         (for-elem        (lambda (elem)
                            (match elem
                              (((? symbol? _) ('@ . _) . body)
                               (max (for-namespaces (sxml-namespaces elem))
                                    (for-elems body)))
                              (((? symbol? _) . body)
                               (for-elems body))
                              (_ -1)))))
    for-elem))

;; Find all URI-qualified names and ensure namespace declarations are available
;; for them, autogenerating prefixes on new or existing namespace declarations
;; where necessary. All auto-added namespace declarations go on the top-level
;; element.
;;
;; (tree:<TOP>) -> <TOP>
(define (sxml-phase-2-to-phase-2a tree)
  (let* ((*namespaces*            (make-hash-table))
         (body                    (sxml-top-element tree))
         (next-count              (make-counter
                                    (sxml-find-highest-autogenerated-prefix-number body)))
         (observe-uri             (lambda (uri)
                                    (hashq-set! *namespaces* uri 'undeclared)))
         (observe-name            (lambda (name)
                                    (if (not (special-symbol? name))
                                      (let ((pfx (sxml-get-prefix name)))
                                        (if (and pfx (> (string-index (symbol->string pfx) #\:) 0))
                                          (observe-uri pfx)))))))

    (let rec ((tree tree))
      (match tree
        (( (? symbol? name) ('@ . attrs) . body)
         (observe-name name)
         (for-each (lambda (attr)
           (if (not (eq? (car attr) '@))
             (observe-name (car attr))))
           attrs)
         (for-each rec body))
        (( (? symbol? name) . body)
         (observe-name name)
         (for-each rec body))
        (else #f)))

    (for-each (lambda (ns)
      (match ns
        ((pfx-in uri pfx-out)
         (hashq-set! *namespaces* (string->symbol uri) 'declared))))
      (sxml-namespaces body))

    (sxml-with-top-element tree
      (sxml-with-annotation body '*NAMESPACES*
        (hash-fold (lambda (k v prior)
          (if (eq? v 'undeclared)
            (let ((new-prefix (string->symbol (string-append "z" (number->string (next-count))))))
              (cons (list new-prefix (symbol->string k) new-prefix) prior))
            prior))
          (sxml-namespaces body) *namespaces*)))))


;; Serialization Preprocessing: Phase 2a->Phase 3 {{{2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sxml-phase-2a-name-to-phase-3 prefix sname nsctx)
  (let ((result (nsctx-lookup nsctx prefix)))
    (or (and result
             (if (eq? (caddr result) '*IMPLICIT*)
                 (string->symbol sname)
                 (string->symbol (string-append (symbol->string (caddr result)) ":" sname))))
        (error "cannot find namespace prefix in context phase3" prefix sname nsctx))))

(define* (sxml-phase-2a-to-phase-3 tree #:optional (nsctx '()))
         (sxml-preprocess tree
           (nsctx-descend (lambda (nsdef rest) 
                            (let ((p (and (pair? (cddr nsdef)) (caddr nsdef))))
                              (if p (acons (string->symbol (cadr nsdef)) (cdr nsdef) rest)
                                    rest))))
           (sxml-preprocess-name sxml-phase-2a-name-to-phase-3)
           nsctx))


;; Serialization Actual {{{2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Serialization function accepting phase-1 input.
(define* (sxml->xml tree #:optional (port (current-output-port)))
  (sxml->xml/phase-2 (sxml-phase-1-to-phase-2 tree)))

;; Serialization function accepting phase-2 input.
(define* (sxml->xml/phase-2 tree #:optional (port (current-output-port)))
  (sxml->xml/phase-3 (sxml-phase-2a-to-phase-3 (sxml-phase-2-to-phase-2a tree))))

;; Final serialization function accepting phase-3 input.
(define* (sxml->xml/phase-3 tree #:optional (port (current-output-port)))
  (cond
    ((pair? tree)
     (if (symbol? (car tree))
         ;; An element or special directive.
         (let ((tag (car tree)))
           (case tag
             ((*TOP*)
              (sxml->xml/phase-3 (cdr tree) port))
             ((*COMMENT*)
              (comment->xml (cadr tree) port))
             ((*RAW*)
              (raw->xml (cadr tree) port))
             ((*ENTITY*)
              (if (and (list? (cdr tree)) (= (length (cdr tree)) 1))
                  (entity->xml (cadr tree) port)
                  (error "bad *ENTITY* args" (cdr tree))))
             ((*PI*)
              (if (and (list? (cdr tree)) (= (length (cdr tree)) 2))
                  (pi->xml (cadr tree) (caddr tree) port)
                  (error "bad *PI* args" (cdr tree))))
             (else
              (let* ((elems (cdr tree))
                     (attrs (and (pair? elems) (pair? (car elems))
                                 (eq? '@ (caar elems))
                                 (cdar elems))))
                (element->xml tag attrs (if attrs (cdr elems) elems) port)))))
         ;; A nodelist.
         (for-each (lambda (x) (sxml->xml/phase-3 x port)) tree)))
    ((string? tree)
     (string->escaped-xml tree port))
    ((null? tree) *unspecified*)
    ((not tree) *unspecified*)
    ((eqv? tree #t) *unspecified*)
    ((procedure? tree)
     (with-output-to-port port tree))
    (else
     (string->escaped-xml
      (call-with-output-string (lambda (port) (display tree port)))
      port))))


;; Serialization Helpers {{{2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following two functions serialize tags and attributes. They are being
;; used in the node handlers for the post-order function, see below.

(define (attribute-value->xml value port)
  (cond
   ((pair? value)
    (attribute-value->xml (car value) port)
    (attribute-value->xml (cdr value) port))
   ((null? value)
    *unspecified*)
   ((string? value)
    (string->escaped-xml value port))
   ((procedure? value)
    (with-output-to-port port value))
   (else
    (string->escaped-xml
     (call-with-output-string (lambda (port) (display value port)))
     port))))

(define check-name
  (let ((*good-cache* (make-hash-table)))
    (lambda (name)
      (if (not (hashq-ref *good-cache* name))
          (let* ((str  (symbol->string name))
                 (i    (string-index str #\:))
                 (head (or (and i (substring str 0 i)) str))
                 (tail (and i (substring str (1+ i)))))
            (and i (string-index (substring str (1+ i)) #\:)
                 (error "Invalid QName: more than one colon" name))
            (for-each
             (lambda (s)
               (and s
                    (or (char-alphabetic? (string-ref s 0))
                        (eq? (string-ref s 0) #\_)
                        (error "Invalid name starting character" s name))
                    (string-for-each
                      (lambda (c)
                        (or (char-alphabetic? c) (string-index "0123456789.-_" c)
                            (error "Invalid name character" c s name)))
                      s)))
             (list head tail))
            (hashq-set! *good-cache* name #t))))))

(define (attribute->xml attr value port)
  (check-name attr)
  (display attr   port)
  (display "=\""  port)
  (attribute-value->xml value port)
  (display #\"    port))

(define (element->xml tag attrs body port)
  (check-name tag)
  (display #\< port)
  (display tag port)
  (if attrs
      (let lp ((attrs attrs))
        (if (pair? attrs)
            (let ((attr (car attrs)))
              (if (pair? attr)
                  (if (eq? (car attr) '@)
                    (annotations->xml (cdr attr) port)
                    (begin
                      (display #\space port)
                      (attribute->xml (car attr) (cdr attr) port)))
                  (error "bad attribute" tag attr))
              (lp (cdr attrs)))
            (if (not (null? attrs))
                (error "bad attributes" tag attrs)))))
  (if (pair? body)
      (begin
        (display #\> port)
        (let lp ((body body))
          (cond
            ((pair? body)
             (sxml->xml/phase-3 (car body) port)
             (lp (cdr body)))
            ((null? body)
             (display "</" port)
             (display tag port)
             (display #\> port))
            (else
              (error "bad element body" tag body)))))
      (display "/>" port)))

;; FIXME: ensure name is valid
(define (entity->xml name port)
  (display #\&      port)
  (display name     port)
  (display #\;      port))

;; FIXME: ensure tag and str are valid
(define (pi->xml tag str port)
  (display "<?"     port)
  (display tag      port)
  (display #\space  port)
  (display str      port)
  (display "?>"     port))

;; FIXME: ensure str doesn't contain ending
(define (comment->xml str port)
  (display "<!--"   port)
  (display str      port)
  (display "-->"    port))

(define (raw->xml str port)
  (display str      port))

(define (namespace-decl->xml namespace port)
  (match namespace
         ((pfx-in uri pfx-out)
          (if (eq? pfx-out '*IMPLICIT*)
            (display " xmlns=\"" port)
            (begin
              (display " xmlns:" port)
              (display pfx-out port)
              (display "=\"" port)))
          (attribute-value->xml uri port)
          (display #\" port))))

(define (annotations->xml annotations port)
  (for-each (lambda (annotation)
    (match annotation
      (('*NAMESPACES* . namespaces)
       (for-each (lambda (namespace)
         (namespace-decl->xml namespace port)) namespaces))
      (_ #f))) annotations))


;; Escaping                        {{{2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-char-quotator char-encoding)
  (let ((bad-chars (list->char-set (map car char-encoding))))

    ;; Check to see if str contains one of the characters in charset, from the
    ;; position i onwards. If so, return that character's index. Otherwise,
    ;; return #f.
    (define (index-cset str i charset)
      (string-index str charset i))

    ;; The body of the function.
    (lambda (str port)
      (let ((bad-pos (index-cset str 0 bad-chars)))
        (if (not bad-pos)
            (display str port)      ; str had all good chars
            (let loop ((from 0) (to bad-pos))
              (cond
                ((>= from (string-length str)) *unspecified*)
                ((not to)
                 (display (substring str from (string-length str)) port))
                (else
                  (let ((quoted-char
                         (cdr (assv (string-ref str to) char-encoding)))
                        (new-to
                          (index-cset str (+ 1 to) bad-chars)))
                    (if (< from to)
                        (display (substring str from to) port))
                    (display quoted-char port)
                    (loop (1+ to) new-to))))))))))

;; Given a string, check to make sure it does not contain characters such as
;; '<' or '&' that require encoding. Return either the original string, or a
;; list of string fragments with special characters replaced by appropriate
;; character entities.
(define string->escaped-xml
  (make-char-quotator
    '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;"))))


;; Structure-Stripping Serialization                                       {{{1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sxml->string sxml)
  "Detags an SXML tree into a string. Does not perform any formatting."
  (string-concatenate-reverse
    (foldts
      (lambda (seed tree)             ; fdown
        '())
      (lambda (seed kid-seed tree)    ; fup
        (append! kid-seed seed))
      (lambda (seed tree)             ; fhere
        (if (string? tree) (cons tree seed) seed))
        '() sxml)))


;; Deserialization                                                         {{{1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helpers from upstream/SSAX.scm.
;;

;     ssax:reverse-collect-str LIST-OF-FRAGS -> LIST-OF-FRAGS
; given the list of fragments (some of which are text strings)
; reverse the list and concatenate adjacent text strings.
; We can prove from the general case below that if LIST-OF-FRAGS
; has zero or one element, the result of the procedure is equal?
; to its argument. This fact justifies the shortcut evaluation below.
(define (ssax:reverse-collect-str fragments)
  (cond
    ((null? fragments) '())	; a shortcut
    ((null? (cdr fragments)) fragments) ; see the comment above
    (else
      (let loop ((fragments fragments) (result '()) (strs '()))
	(cond
	  ((null? fragments)
	    (if (null? strs) result
	      (cons (string-concatenate/shared strs) result)))
	  ((string? (car fragments))
	    (loop (cdr fragments) result (cons (car fragments) strs)))
	  (else
	    (loop (cdr fragments)
	      (cons
		(car fragments)
		(if (null? strs) result
		  (cons (string-concatenate/shared strs) result)))
	      '())))))))

(define (read-internal-doctype-as-string port)
  (string-concatenate/shared
    (let loop ()
      (let ((fragment
	     (next-token '() '(#\]) "reading internal DOCTYPE" port)))
	(if (eqv? #\> (peek-next-char port))
	    (begin
	      (read-char port)
	      (cons fragment '()))
	    (cons* fragment "]" (loop)))))))

;; Ideas for the future for this interface:
;;
;;  * Allow doctypes to provide parsed entities
;;
;;  * Allow validation (the ELEMENTS value from the DOCTYPE handler
;;    below)
;;
;;  * Parse internal DTDs
;;
;;  * Parse external DTDs
;;
(define* (xml->sxml #:optional (string-or-port (current-input-port)) #:key
                    (namespaces '())
                    (declare-namespaces? #t)
                    (trim-whitespace? #f)
                    (entities '())
                    (default-entity-handler #f)
                    (doctype-handler #f))
  "Use SSAX to parse an XML document into SXML. Takes one optional
argument, @var{string-or-port}, which defaults to the current input
port."
  ;; NAMESPACES: alist of PREFIX -> URI.  Specifies the symbol prefix
  ;; that the user wants on elements of a given namespace in the
  ;; resulting SXML, regardless of the abbreviated namespaces defined in
  ;; the document by xmlns attributes.  If DECLARE-NAMESPACES? is true,
  ;; these namespaces are treated as if they were declared in the DTD.

  ;; ENTITIES: alist of SYMBOL -> STRING.

  ;; NAMESPACES: list of (DOC-PREFIX . (USER-PREFIX . URI)).
  ;; A DOC-PREFIX of #f indicates that it comes from the user.
  ;; Otherwise, prefixes are symbols.
  (define (munge-namespaces namespaces)
    (map (lambda (el)
           (match el
             ((prefix . uri-string)
              (cons* (and declare-namespaces? prefix)
                     prefix
                     (ssax:uri-string->symbol uri-string)))))
         namespaces))

  (define (user-namespaces)
    (munge-namespaces namespaces))

  (define (user-entities)
    (if (and default-entity-handler
             (not (assq '*DEFAULT* entities)))
        (acons '*DEFAULT* default-entity-handler entities)
        entities))

  (define (name->sxml name)
    (match name
      ((prefix . local-part)
       (symbol-append prefix (string->symbol ":") local-part))
      (_ name)))

  (define (doctype-continuation seed)
    (lambda* (#:key (entities '()) (namespaces '()))
      (values #f
              (append entities (user-entities))
              (append (munge-namespaces namespaces) (user-namespaces))
              seed)))

  ;; The SEED in this parser is the SXML: initialized to '() at each new
  ;; level by the fdown handlers; built in reverse by the fhere parsers;
  ;; and reverse-collected by the fup handlers.
  (define parser
    (ssax:make-parser
     NEW-LEVEL-SEED ; fdown
     (lambda (elem-gi attributes namespaces expected-content seed)
       '())
   
     FINISH-ELEMENT ; fup
     (lambda (elem-gi attributes namespaces parent-seed seed)
       (let ((seed (if trim-whitespace?
                       (ssax:reverse-collect-str-drop-ws seed)
                       (ssax:reverse-collect-str seed)))
             (attrs (attlist-fold
                     (lambda (attr accum)
                       (cons (list (name->sxml (car attr)) (cdr attr))
                             accum))
                     '() attributes)))
         (acons (name->sxml elem-gi)
                (if (null? attrs)
                    seed
                    (cons (cons '@ attrs) seed))
                parent-seed)))

     CHAR-DATA-HANDLER ; fhere
     (lambda (string1 string2 seed)
       (if (string-null? string2)
           (cons string1 seed)
           (cons* string2 string1 seed)))

     DOCTYPE
     ;; -> ELEMS ENTITIES NAMESPACES SEED
     ;;
     ;; ELEMS is for validation and currently unused.
     ;;
     ;; ENTITIES is an alist of parsed entities (symbol -> string).
     ;;
     ;; NAMESPACES is as above.
     ;;
     ;; SEED builds up the content.
     (lambda (port docname systemid internal-subset? seed)
       (call-with-values
           (lambda ()
             (cond
              (doctype-handler
               (doctype-handler docname systemid
                                (and internal-subset?
                                     (read-internal-doctype-as-string port))))
              (else
               (when internal-subset?
                 (ssax:skip-internal-dtd port))
               (values))))
         (doctype-continuation seed)))

     UNDECL-ROOT
     ;; This is like the DOCTYPE handler, but for documents that do not
     ;; have a <!DOCTYPE!> entry.
     (lambda (elem-gi seed)
       (call-with-values
           (lambda ()
             (if doctype-handler
                 (doctype-handler #f #f #f)
                 (values)))
        (doctype-continuation seed)))

     PI
     ((*DEFAULT*
       . (lambda (port pi-tag seed)
           (cons
            (list '*PI* pi-tag (ssax:read-pi-body-as-string port))
            seed))))))

  (let* ((port (if (string? string-or-port)
                   (open-input-string string-or-port)
                   string-or-port))
         (elements (reverse (parser port '()))))
    `(*TOP* ,@elements)))

(define check-name
  (let ((*good-cache* (make-hash-table)))
    (lambda (name)
      (if (not (hashq-ref *good-cache* name))
          (let* ((str (symbol->string name))
                 (i (string-index str #\:))
                 (head (or (and i (substring str 0 i)) str))
                 (tail (and i (substring str (1+ i)))))
            (and i (string-index (substring str (1+ i)) #\:)
                 (error "Invalid QName: more than one colon" name))
            (for-each
             (lambda (s)
               (and s
                    (or (char-alphabetic? (string-ref s 0))
                        (eq? (string-ref s 0) #\_)
                        (error "Invalid name starting character" s name))
                    (string-for-each
                     (lambda (c)
                       (or (char-alphabetic? c) (string-index "0123456789.-_" c)
                           (error "Invalid name character" c s name)))
                     s)))
             (list head tail))
            (hashq-set! *good-cache* name #t))))))

;}}}1


(export xml->sxml sxml->xml sxml->xml/phase-2 sxml->xml/phase-3 sxml->string
        sxml-phase-1-to-phase-2 sxml-phase-2-to-phase-2a sxml-phase-2a-to-phase-3 sxml-phase-2-to-phase-3
        sxml-attribute sxml-with-attribute make-counter
        sxml-body)
