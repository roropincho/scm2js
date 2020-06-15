(##include "~/gambit/lib/gambit#.scm")
(##include "~/gambit/examples/web-server/html.scm")

(declare
 (extended-bindings))

; -------------------------------------------------------------------------
; FUNCTIONS
; -------------------------------------------------------------------------
(define (document-obj)
  (##inline-host-expression "document;"))

(define (null-obj)
  (##inline-host-expression "null;"))

(define (undefined-obj)
  (##inline-host-expression "undefined;"))

(define (undefined? obj)
  (##inline-host-expression "g_host2scm(@1@ === @2@);" obj (undefined-obj)))

(define (defined? obj)
  (not (undefined? obj)))

(define (usable-obj? obj)
  (and (not (null? obj))
       (defined? obj)))

(define (class-of obj)
  (if (usable-obj? obj)
      (##inline-host-expression "g_host2scm((@1@).constructor.name);" obj)
      "hasNoClass"))

(define (scm-obj? obj)
  (string=? "G_" (substring (class-of obj) 0 2)))

(define (get-slots obj)
  (if (usable-obj? obj)
      (##inline-host-expression "(@1@).slots;" obj)
      (undefined-obj)))

(define (has-slots? obj)
  (defined? (get-slots obj)))

(define (html-obj? obj)
  (if (and
       (scm-obj? obj)
       (has-slots? obj))
      (let ((first-slot (##inline-host-expression "(@1@)[0];" (get-slots obj))))
        (if (has-slots? first-slot)
            (let ((sec-level-slots (get-slots first-slot)))
              (if (>=
                   (##inline-host-expression "g_host2scm((@1@).length);" sec-level-slots)
                   3)
                  (let ((obj-symbol (##inline-host-expression "(@1@)[2];" sec-level-slots)))
                    (let ((obj-symbol-name (##inline-host-expression "(@1@).name;" obj-symbol)))
                      (if (defined? obj-symbol-name)
                          (string=?
                           "html"
                           (##inline-host-expression "g_host2scm(@1@);" obj-symbol-name))
                          #f)))
                  #f))
            #f))
      #f))

(define (html->string html)
  (if (html-obj? html)
      (call-with-output-string
       (lambda (port)
         (write-html html port)))
      #f))

(define (js->scm obj)
  (if (scm-obj? obj)
      obj
      (##inline-host-expression "g_host2scm(@1@);" obj)))

(define (scm->js obj)
  (if (scm-obj? obj)
      (##inline-host-expression "g_scm2host(@1@);" obj)
      obj))

(define (type-of obj)
  (js->scm
   (##inline-host-expression "typeof (@1@);" obj)))

(define (document.write html)
  (if (usable-obj? html)
      (##inline-host-statement
       "(@1@).write(@2@);"
       (document-obj)
       (if (scm-obj? html)
           (scm->js
            (if (html-obj? html)
                (html->string html)
                html))
           html))
      #f))

(define (console.log obj)
  (##inline-host-statement
   "console.log(@1@);"
   (if (scm-obj? obj)
       (scm->js
        (if (html-obj? obj)
            (html->string obj)
            obj))
       obj)))

(define (dom-obj? obj)
  (if (not (scm-obj? obj))
      (js->scm
       (##inline-host-expression "(@1@) instanceof Node;" obj))
      #f))

(define (document-obj? obj)
  (##inline-host-expression "(@1@) === document;" obj))

; getElementById / querySelector / querySelectorAll

(define (get-element-by-id id)
  (if (and
       (scm-obj? id)
       (string? id))
      (##inline-host-expression "(@1@).getElementById(@2@);" (document-obj) (scm->js id))
      (null-obj)))

(define (query-selector selector #!optional (frame (document-obj)))
  (if (and
       (scm-obj? selector)
       (string? selector)
       (or (dom-obj? frame)
           (document-obj? frame)))
      (##inline-host-expression "(@1@).querySelector(@2@);" frame (scm->js selector))
      (null-obj)))

(define (query-selector-all selector #!optional (frame (document-obj)))
  (if (and
       (scm-obj? selector)
       (string? selector)
       (or (dom-obj? frame)
           (document-obj? frame)))
      (##inline-host-expression "(@1@).querySelectorAll(@2@);" frame (scm->js selector))
      (null-obj)))

; add / get / modify / remove / test attribute

(define (get-attribute obj att)
  (if (and
       (dom-obj? obj)
       (not (document-obj? obj))
       (string? att))
      (js->scm
       (##inline-host-expression "(@1@).getAttribute(@2@);" obj (scm->js att)))
      #f))

(define (has-attribute obj att)
  (if (string? att)
      (let ((att-val (get-attribute obj att)))
        (and att-val
             (not (null? att-val))))
      #f))

(define (set-attribute obj att val)
  (if (and
       (dom-obj? obj)
       (not (document-obj? obj))
       (string? att)
       (scm-obj? val))
      (##inline-host-statement "(@1@).setAttribute(@2@, @3@);" obj (scm->js att) (scm->js val))
      #f))

(define (remove-attribute obj att)
  (if (and
       (dom-obj? obj)
       (not (document-obj? obj))
       (string? att))
      (##inline-host-statement "(@1@).removeAttribute(@2@);" obj (scm->js att))
      #f))

; insert (before / after) / modify / remove / test (presence / type) html element

(define (get-element-type node)
  (if (not (scm-obj? node))
      (js->scm
       (##inline-host-expression "(@1@).nodeName;" node))
      #f))

(define (element-of-type? node type)
  (if (and
       (not (scm-obj? node))
       (string? type))
      (let ((node-type (get-element-type node)))
        (and (defined? node-type)
             (string=? type(get-element-type node))))
      #f))

(define (first-child obj)
  (if (dom-obj? obj)
      (let ((first-given (##inline-host-expression "(@1@).firstChild;" obj)))
        (if (or
             (null? first-given)
             (dom-obj? first-given)
             (and (element-of-type? first-given "#text")
                  (or
                   (> (js->scm
                       (##inline-host-expression "(@1@).length;" first-given))
                      1)
                   (not (string=?
                         "\n"
                         (js->scm
                          (##inline-host-expression "(@1@).wholeText;" first-given)))))))
            first-given
            (query-selector "*" obj)))
      #f))

(define (parent-node obj)
  (if (dom-obj? obj)
      (##inline-host-expression "(@1@).parentNode;" obj)
      #f))

(define (get-new-node)
  (##inline-host-expression "(@1@).createElement('DIV');" (document-obj)))

(define (remove-node node)
  (let ((parent-temp (parent-node node)))
    (if (and
         (dom-obj? node)
         (usable-obj? parent-temp))
        (##inline-host-statement "(@1@).removeChild(@2@);" parent-temp node)
        #f)))

(define (set-inner-html obj html-obj)
  (if (dom-obj? obj)
      (##inline-host-statement
       "(@1@).innerHTML = @2@;"
       obj
       (if (scm-obj? html-obj)
           (scm->js
            (if (html-obj? html-obj)
                (html->string html-obj)
                html-obj))
           html-obj))
      #f))

(define (append-node parent child)
  (if (and
       (dom-obj? parent)
       (dom-obj? child))
      (##inline-host-statement "(@1@).appendChild(@2@);" parent child)
      #f))

(define (append-html parent child-html)
  (let ((parent-temp (get-new-node))
        (child-is-dom (dom-obj? child-html)))
    (if (set-inner-html
         parent-temp
         (if child-is-dom
             (##inline-host-expression "(@1@).outerHTML;" child-html)
             child-html))
        (let ((node-temp (first-child parent-temp)))
          (if node-temp
              (let ((append-returned (append-node parent node-temp)))
                (begin
                  (if (undefined? append-returned)
                      (remove-node child-html))
                  append-returned))
              #f))
        #f)))

(define (insert-node-before parent child ref-child)
  (if (and
       (dom-obj? parent)
       (dom-obj? child)
       (dom-obj? ref-child)
       (eq? parent (parent-node ref-child)))
      (##inline-host-statement "(@1@).insertBefore(@2@, @3@);" parent child ref-child)
      #f))

(define (insert-html-before parent child-html ref-child)
  (let ((parent-temp (get-new-node))
        (child-is-dom (dom-obj? child-html)))
    (if (set-inner-html
         parent-temp
         (if child-is-dom
             (##inline-host-expression "(@1@).outerHTML;" child-html)
             child-html))
        (let ((node-temp (first-child parent-temp)))
          (if node-temp
              (let ((insert-returned (insert-node-before parent node-temp ref-child)))
                (begin
                  (if (undefined? insert-returned)
                      (remove-node child-html))
                  insert-returned))
              #f))
        #f)))

(define (element-present-by-query? selector #!optional (frame (document-obj)))
  (not (null? (query-selector selector frame))))

(define (replace-node old new)
  (let ((parent-temp (parent-node old)))
    (if (and
         (dom-obj? old)
         (usable-obj? parent-temp)
         (dom-obj? new))
        (##inline-host-statement "(@1@).replaceChild(@2@, @3@);" parent-temp new old)
        #f)))

(define (replace-html old new-html)
  (let ((parent-temp (get-new-node))
        (new-is-dom (dom-obj? new-html)))
    (if (set-inner-html
         parent-temp
         (if new-is-dom
             (##inline-host-expression "(@1@).outerHTML;" new-html)
             new-html))
        (let ((node-temp (first-child parent-temp)))
          (if node-temp
              (let ((replace-returned (replace-node old node-temp)))
                (begin
                  (if (undefined? replace-returned)
                      (remove-node new-html))
                  replace-returned))
              #f))
        #f)))

(define (lst->string lst)
  (call-with-output-string
   (lambda (port)
     (map
      (lambda (elem)
        (display elem port))
      lst))))
