(##include "~~/lib/gambit#.scm")
(##include "~~/lib/_gambit#.scm")

(declare
 (extended-bindings)
 (separate))

(define (##fail-check-foreign . rest) (error "FOREIGN object expected"))

; -------------------------------------------------------------------------
; conversion functions
; -------------------------------------------------------------------------

(define-procedure (foreign->js (obj foreign))
  (##inline-host-expression "g_foreign2host(@1@);" obj))

(define (js->foreign obj)
  (##inline-host-expression "g_host2foreign(@1@);" obj))

(define (scm->js obj)
  (begin
    (##inline-host-statement "var result;")
    (##inline-host-statement "try {result = g_scm2host(@1@);} catch (e) {result = Error(e);}" obj)
    (let ((result (##inline-host-expression "result;")))
      (if (js->scm (##inline-host-expression "@1@ instanceof Error;" result))
          (error (js->scm result))
          result))))

(define (js->scm obj)
  (##inline-host-expression "g_host2scm(@1@);" obj))

; -------------------------------------------------------------------------
; basic functions
; -------------------------------------------------------------------------
(define (document-obj)
  (js->foreign (##inline-host-expression "document;")))

(define (null-obj)
  (js->foreign (##inline-host-expression "null;")))

(define (undefined-obj)
  (js->foreign (##inline-host-expression "undefined;")))

(define (extract-error obj)
  (error (js->scm (##inline-host-expression "(@1@).name + ' : ' + (@1@).message;" obj))))

(define (instanceof obj type)
  (if (string? type)
      (begin
        (##inline-host-statement "var result;")
        (##inline-host-statement "try {result = ((@1@) instanceof eval(@2@));} catch (e) {result = Error(e);}" obj (scm->js type))
        (let ((result (##inline-host-expression "result;")))
          (if (js->scm (##inline-host-expression "(@1@) instanceof Error;" result))
              (extract-error result)
              (js->scm result))))
      (error "instanceof : type, param 2, is not a string")))

(define (js-error? obj)
  (instanceof obj "Error"))

; -------------------------------------------------------------------------
; functions to output in browser
; -------------------------------------------------------------------------

(define-procedure (document.write (content string))
    (##inline-host-statement "(@1@).write(@2@);" (foreign->js (document-obj)) (scm->js content)))

(define (console.log obj)
  (begin
    (##inline-host-statement "var result;")
    (##inline-host-statement "try {result = console.log(@1@);} catch (e) {result = e;}" obj)
    (let ((result (##inline-host-expression "result;")))
      (if (js-error? result)
          (extract-error result)))))

; -------------------------------------------------------------------------
; functions to get browser elements
; -------------------------------------------------------------------------

(define-procedure (document.getElementById (id string))
  (js->foreign (##inline-host-expression "(@1@).getElementById(@2@);" (foreign->js (document-obj)) (scm->js id))))

(define-procedure (.querySelector (obj foreign) (selector string))
  (begin
    (##inline-host-statement "var result;")
    (##inline-host-statement "try {result = (@1@).querySelector(@2@);} catch (e) {result = e;}" (foreign->js obj) (scm->js selector))
    (let ((result (##inline-host-expression "result;")))
      (if (js-error? result)
          (extract-error result)
          (js->foreign result)))))

(define-procedure (.querySelectorAll (obj foreign) (selector string))
  (begin
    (##inline-host-statement "var result;")
    (##inline-host-statement "try {result = (@1@).querySelectorAll(@2@);} catch (e) {result = e;}" (foreign->js obj) (scm->js selector))
    (let ((result (##inline-host-expression "result;")))
      (if (js-error? result)
          (extract-error result)
          (js->foreign result)))))

(define-procedure (element.firstChild (obj foreign))
  (js->foreign (##inline-host-expression "(@1@).firstChild;" (foreign->js obj))))

(define-procedure (element.firstElementChild (obj foreign))
  (js->foreign (##inline-host-expression "(@1@).firstElementChild;" (foreign->js obj))))

(define-procedure (element.parentNode (obj foreign))
  (js->foreign (##inline-host-expression "(@1@).parentNode;" (foreign->js obj))))

(define-procedure (document.createElement (type string))
  (js->foreign (##inline-host-expression "(@1@).createElement(@2@);" (foreign->js (document-obj)) (scm->js type))))

(define (get-new-div-node)
  (document.createElement "div"))

; -------------------------------------------------------------------------
; functions to manipulate a browser element's attributes
; -------------------------------------------------------------------------

(define-procedure (element.getAttribute (obj foreign) (att string))
  (begin
    (##inline-host-statement "var result;")
    (##inline-host-statement "try {result = (@1@).getAttribute(@2@);} catch (e) {result = e;}" (foreign->js obj) (scm->js att))
    (let ((result (##inline-host-expression "result;")))
      (if (js-error? result)
          (extract-error result)
          (js->scm result)))))

(define-procedure (element.hasAttribute (obj foreign) (att string))
  (begin
    (##inline-host-statement "var result;")
    (##inline-host-statement "try {result = (@1@).hasAttribute(@2@);} catch (e) {result = e;}" (foreign->js obj) (scm->js att))
    (let ((result (##inline-host-expression "result;")))
      (if (js-error? result)
          (extract-error result)
          (js->scm result)))))

(define-procedure (element.setAttribute (obj foreign) (att string) (val string))
  (begin
    (##inline-host-statement "var result;")
    (##inline-host-statement "try {result = (@1@).setAttribute(@2@, @3@);} catch (e) {result = e;}" (foreign->js obj) (scm->js att) (scm->js val))
    (let ((result (##inline-host-expression "result;")))
      (if (js-error? result)
          (extract-error result)))))

(define-procedure (element.removeAttribute (obj foreign) (att string))
  (begin
    (##inline-host-statement "var result;")
    (##inline-host-statement "try {result = (@1@).removeAttribute(@2@);} catch (e) {result = e;}" (foreign->js obj) (scm->js att))
    (let ((result (##inline-host-expression "result;")))
      (if (js-error? result)
          (extract-error result)))))

; -------------------------------------------------------------------------
; functions to manipulate a browser element's other caracteristics
; -------------------------------------------------------------------------

(define-procedure (element.innerHTML (obj foreign) (content string ""))
  (let ((is-get-call (string=? content ""))
        (js-obj (foreign->js obj)))
    (if is-get-call
        (js->scm (##inline-host-expression "(@1@).innerHTML;" js-obj))
        (##inline-host-statement "(@1@).innerHTML = @2@;" js-obj (scm->js content)))))

(define-procedure (element.outerHTML (obj foreign) (content string ""))
  (let ((is-get-call (string=? content ""))
        (js-obj (foreign->js obj)))
    (if is-get-call
        (js->scm (##inline-host-expression "(@1@).outerHTML;" js-obj))
        (begin
          (##inline-host-statement "var result;")
          (##inline-host-statement "try {(@1@).outerHTML = @2@;} catch (e) {result = e;}" js-obj (scm->js content))
          (let ((result (##inline-host-expression "result;")))
            (if (js-error? result)
                (extract-error result)))))))

; -------------------------------------------------------------------------
; functions to modify a browser element's location in the document
; -------------------------------------------------------------------------

(define-procedure (element.removeChild (parent foreign) (child foreign))
  (begin
    (##inline-host-statement "var result;")
    (##inline-host-statement "try {result = (@1@).removeChild(@2@);} catch (e) {result = e;}" (foreign->js parent) (foreign->js child))
    (let ((result (##inline-host-expression "result;")))
      (if (js-error? result)
          (extract-error result)
          (js->foreign result)))))

(define-procedure (element.appendChild (parent foreign) (child foreign))
  (begin
    (##inline-host-statement "var result;")
    (##inline-host-statement "try {result = (@1@).appendChild(@2@);} catch (e) {result = e;}" (foreign->js parent) (foreign->js child))
    (let ((result (##inline-host-expression "result;")))
      (if (js-error? result)
          (extract-error result)
          (js->foreign result)))))

(define-procedure (element.insertBefore (parent foreign) (new-node foreign) (reference foreign))
  (begin
    (##inline-host-statement "var result;")
    (##inline-host-statement "try {result = (@1@).insertBefore(@2@, @3@);} catch (e) {result = e;}" (foreign->js parent) (foreign->js new-node) (foreign->js reference))
    (let ((result (##inline-host-expression "result;")))
      (if (js-error? result)
          (extract-error result)
          (js->foreign result)))))

(define-procedure (element.replaceChild (parent foreign) (new-node foreign) (old-node foreign))
  (begin
    (##inline-host-statement "var result;")
    (##inline-host-statement "try {result = (@1@).replaceChild(@2@, @3@);} catch (e) {result = e;}" (foreign->js parent) (foreign->js new-node) (foreign->js old-node))
    (let ((result (##inline-host-expression "result;")))
      (if (js-error? result)
          (extract-error result)
          (js->foreign result)))))

