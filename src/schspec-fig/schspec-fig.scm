(define-module (schspec-fig))
(use-modules (ice-9 match) (ice-9 popen) (gcrypt hash) (gcrypt base16))

(define (write-str x)
  (call-with-output-string (lambda (p) (write x p))))

(define (sha256-str x)
  (bytevector->base16-string (call-with-input-string x (lambda (p) (port-sha256 p)))))

(define (mime-to-ext x)
  (if (string? x) (mime-to-ext (string->symbol x))
      (match x
             ('image/svg+xml "svg")
             ('image/png "png")
             (x (error "unknown MIME type" x)))))

(define (fig-type-from-build build)
        (cadr (assq 'fig-build-output-type (cdr build))))

(define (fig-hash-from-build build src)
  (sha256-str (write-str (list build src))))

(define (fig-path-from-build build src)
  (let* ((hash (fig-hash-from-build build src))
         (type (fig-type-from-build build))
         (path (string-append "figs/" hash "." (mime-to-ext type))))
    path))

(define (compile-fig-build-new-cmd-file build src)
  (let* ((type (fig-type-from-build build))
         (path (fig-path-from-build build src))
         (pathx (string-append "build/" path))
         ;; Workaround for a bug where graphviz emits SVG with
         ;; stroke="transparent" which is not a valid SVG value for 'stroke',
         ;; should be 'none'. fop balks at this so fudge it. (dot also has a
         ;; cairo-based SVG renderer but its output is not of great quality and
         ;; vectorises all text glyphs, so we don't want to use it.)
         (cmd  (string-append (cadr (assq 'fig-build-cmd (cdr build))) " > " pathx ".tmp && sed -i 's/stroke=\"transparent\"/stroke=\"none\"/g' " pathx".tmp && mv " pathx ".tmp " pathx))
         (pipe (open-output-pipe cmd)))
    (display src pipe)
    (when (not (status:exit-val (close-pipe pipe)))
          (error (list "subprocess failed" cmd src)))
    `(fig-output (@ (href ,path) (type ,type)))))

(define (compile-fig-build-new build src)
  (match (cadr (assq 'fig-build-type (cdr build)))
         ('cmd/file
          (compile-fig-build-new-cmd-file build src))
         (x
          (error (list "unsupported fig build type" x)))))

(define (compile-fig-build build src)
  (let ((path (fig-path-from-build build src))
        (type (fig-type-from-build build)))
    (if (stat path #f)
        `(fig-output (@ (href ,path) (type ,type)))
        (compile-fig-build-new build src))))

(define (compile-fig fig)
  (let
    ((builds (cdr  (assq 'fig-builds (cdr fig))))
     (src    (cadr (assq 'fig-src    (cdr fig)))))
    (append fig `((fig-outputs ,@(map (lambda (b) (compile-fig-build b src)) builds))))))

;; (figlet
;;   (fig-build
;;     (fig-build-type "cmd")
;;     (fig-build-output-type "file/svg") ;; file/svg, file/png, inline/svg
;;     (fig-build-cmd "dot ..."))
;;   (fig-out ...)
;;   (fig-src "..."))
(define (dot-raw* src)
  `(fig (@ (type dot))
     (fig-builds
       (fig-build
         (fig-build-type cmd/file)
         (fig-build-output-type image/svg+xml)
         (fig-build-cmd "dot -Tsvg"))
       (fig-build
         (fig-build-type cmd/file)
         (fig-build-output-type image/png)
         (fig-build-cmd "dot -Tpng")))
     (fig-src ,src)))

(define (dot-raw src) (compile-fig (dot-raw* src)))

;; <fig-outputs>
;;   <fig-output href="foo.svg" type="image/svg"/>
;;   <fig-output href="foo.png" type="image/png"/>
;;   <fig-output type="image/svg">
;;     <svg .../>
;;   </fig-output>
;; </fig-outputs>
;;
;; <object data="foo.svg" type="image/svg">
;;   <object data="foo.png" type="image/png">
;;     ...
;;   </object>
;; </object>

(export dot-raw write-str)
