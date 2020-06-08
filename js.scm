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

; -------------------------------------------------------------------------
; TESTS
; -------------------------------------------------------------------------
(define page-main
  (<div>
   (<div>
    id: "wrapper"
    class: "'wrapper'"
    (<h1> "HELLO WORLD!")
    (<h2>
     style: "'color: red; font-family: sans-serif; font-size: 50px; font-weight: normal; text-transform: uppercase;'"
     "Gros h2")
    (<ul>
     (<li>
      class: "'uneClasse deuxClasse'"
      id: "'firstLI'"
      onclick: "'javascript: void(0);'"
      "First")
     (<li> "Second")
     (<li> "..."))
    (<p> "Un petit texte... tralala...")
    (<input>
     type: 'text
     id: "'myInput'"
     name: "monInput"
     class: "'c1 c2 c3'"
     value: "'une valeur'")
    (<p> "Un autre omg!"))
   (<div>
    id: "setInnerHTML"
    (<p> id: "setInnerString1")
    (<p> id: "setInnerString2"))
   (<p> "The end!")))

(define (lst->string lst)
  (call-with-output-string
   (lambda (port)
     (map
      (lambda (elem)
        (display elem port))
      lst))))

(define (test-document-obj)
  (begin
    (console.log
     (lst->string
      '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XX documentObj XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")))
    (console.log (document-obj))))

(define (test-null-obj)
  (let ((obj (null-obj)))
    (begin
      (console.log
       (lst->string
        '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
          "XX nullObj XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
          "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")))
      (console.log obj)
      (console.log
       (lst->string
        '("--- null?\n"
          "    expect : true")))
      (console.log (null? obj)))))

(define (test-undefined-obj)
  (begin
    (console.log
     (lst->string
      '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XX undefinedObj XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")))
    (console.log (undefined-obj))))

(define (test-undefined?)
  (begin
    (console.log
     (lst->string
      '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XX isUndefined XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")))
    (console.log
     (lst->string
      '("--- with documentObj\n"
        "    expect : false")))
    (console.log (undefined? (document-obj)))
    (console.log
     (lst->string
      '("--- with nullObj\n"
        "    expect : false")))
    (console.log (undefined? (null-obj)))
    (console.log
     (lst->string
      '("--- with inline-exp with undefined\n"
        "    expect : true")))
    (console.log (undefined? (undefined-obj)))))

(define (test-defined?)
  (begin
    (console.log
     (lst->string
      '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XX isDefined XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "--- with documentObj\n"
        "    expect : true")))
    (console.log (defined? (document-obj)))
    (console.log
     (lst->string
      '("--- with nullObj\n"
        "    expect : true")))
    (console.log (defined? (null-obj)))
    (console.log
     (lst->string
      '("--- with inline-exp with undefined\n"
        "    expect : false")))
    (console.log (defined? (undefined-obj)))))

(define (test-usable-obj?)
  (begin
    (console.log
     (lst->string
      '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XX isUsableObject XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "--- with documentObj\n"
        "    expect : true")))
    (console.log (usable-obj? (document-obj)))
    (console.log
     (lst->string
      '("--- with nullObj\n"
        "    expect : false")))
    (console.log (usable-obj? (null-obj)))
    (console.log
     (lst->string
      '("--- with inline-exp with undefined\n"
        "    expect : false")))
    (console.log (usable-obj? (undefined-obj)))))

(define (test-class-of)
  (begin
    (console.log
     (lst->string
      '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XX classOf XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "--- with scm string\n"
        "    expect : G_ScmString")))
    (console.log (class-of "test string"))
    (console.log
     (lst->string
      '("--- with js string\n"
        "    expect : String")))
    (console.log (class-of (##inline-host-expression "'test string';")))
    (console.log
     (lst->string
      '("--- with void scm value\n"
        "    expect : hasNoClass")))
    (console.log (class-of (void)))
    (console.log
     (lst->string
      '("--- with nullObj\n"
        "    expect : hasNoClass")))
    (console.log (class-of (null-obj)))
    (console.log
     (lst->string
      '("--- with inline-exp with undefined\n"
        "    expect : hasNoClass")))
    (console.log (class-of (undefined-obj)))))

(define (test-scm-obj?)
  (begin
    (console.log
     (lst->string
      '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XX isScmObject XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "--- with scm html object\n"
        "    expect : true")))
    (console.log (scm-obj? page-main))
    (console.log
     (lst->string
      '("--- with scm string\n"
        "    expect : true")))
    (console.log (scm-obj? "test string"))
    (console.log
     (lst->string
      '("--- with js string\n"
        "    expect : false")))
    (console.log (scm-obj? (##inline-host-expression "'test string';")))
    (console.log
     (lst->string
      '("--- with nullObj\n"
        "    expect : false")))
    (console.log (scm-obj? (null-obj)))
    (console.log
     (lst->string
      '("--- with inline-exp with undefined\n"
        "    expect : false")))
    (console.log (scm-obj? (undefined-obj)))))

(define (test-get-slots)
  (begin
    (console.log
     (lst->string
      '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XX getSlots XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "--- with scm html object")))
    (console.log (get-slots page-main))
    (console.log
     (lst->string
      '("--- with scm string\n"
        "    expect : undefined")))
    (console.log (get-slots "test string"))
    (console.log
     (lst->string
      '("--- with js string\n"
        "    expect : undefined")))
    (console.log (get-slots (##inline-host-expression "'test string';")))
    (console.log
     (lst->string
      '("--- with nullObj\n"
        "    expect : undefined")))
    (console.log (get-slots (null-obj)))
    (console.log
     (lst->string
      '("--- with inline-exp with undefined\n"
        "    expect : undefined")))
    (console.log (get-slots (undefined-obj)))))

(define (test-has-slots?)
  (begin
    (console.log
     (lst->string
      '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XX hasSlots XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "--- with scm html object\n"
        "    expect : true")))
    (console.log (has-slots? page-main))
    (console.log
     (lst->string
      '("--- with scm string\n"
        "    expect : false")))
    (console.log (has-slots? "test string"))
    (console.log
     (lst->string
      '("--- with js string\n"
        "    expect : false")))
    (console.log (has-slots? (##inline-host-expression "'test string';")))
    (console.log
     (lst->string
      '("--- with nullObj\n"
        "    expect : false")))
    (console.log (has-slots? (null-obj)))
    (console.log
     (lst->string
      '("--- with inline-exp with undefined\n"
        "    expect : false")))
    (console.log (has-slots? (undefined-obj)))))

(define (test-html-obj?)
  (begin
    (console.log
     (lst->string
      '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XX isHtmlObject XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "--- with scm html object\n"
        "    expect : true")))
    (console.log (html-obj? page-main))
    (console.log
     (lst->string
      '("--- with scm string\n"
        "    expect : false")))
    (console.log (html-obj? "test string"))
    (console.log
     (lst->string
      '("--- with js string\n"
        "    expect : false")))
    (console.log (html-obj? (##inline-host-expression "'test string';")))
    (console.log
     (lst->string
      '("--- with nullObj\n"
        "    expect : false")))
    (console.log (html-obj? (null-obj)))
    (console.log
     (lst->string
      '("--- with inline-exp with undefined\n"
        "    expect : false")))
    (console.log (html-obj? (undefined-obj)))))

(define (test-html-to-string)
  (begin
    (console.log
     (lst->string
      '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XX html->string XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "--- with scm html object\n"
        "    expect : <p>a test text</p>")))
    (console.log (html->string (<p> "a test text")))
    (console.log
     (lst->string
      '("--- with scm string\n"
        "    expect : false")))
    (console.log (html-obj? "test string"))
    (console.log
     (lst->string
      '("--- with js string\n"
        "    expect : false")))
    (console.log (html-obj? (##inline-host-expression "'test string';")))
    (console.log
     (lst->string
      '("--- with nullObj\n"
        "    expect : false")))
    (console.log (html-obj? (null-obj)))
    (console.log
     (lst->string
      '("--- with inline-exp with undefined\n"
        "    expect : false")))
    (console.log (html-obj? (undefined-obj)))))

(define (test-js-to-scm)
  (let ((js-temp (##inline-host-expression "'test string';"))
        (scm-temp "test string"))
    (begin
      (console.log
       (lst->string
        '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
          "XX jsObj->scmObj XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
          "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
          "--- with scm string\n"
          "    expect : class should stay the same : G_ScmString")))
      (console.log (class-of scm-temp))
      (console.log (class-of (js->scm scm-temp)))
      (console.log
       (lst->string
        '("--- with js string\n"
          "    expect : class should change from String to G_ScmString")))
      (console.log (class-of js-temp))
      (console.log (class-of (js->scm js-temp))))))

(define (test-scm-to-js)
  (let ((js-temp (##inline-host-expression "'test string';"))
        (scm-temp "test string"))
    (begin
      (console.log
       (lst->string
        '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
          "XX scmObj->jsObj XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
          "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
          "--- with scm string\n"
          "    expect : class should change from G_ScmString to String")))
      (console.log (class-of scm-temp))
      (console.log (class-of (scm->js scm-temp)))
      (console.log
       (lst->string
        '("--- with js string\n"
          "    expect : class should stay the same : String")))
      (console.log (class-of js-temp))
      (console.log (class-of (scm->js js-temp))))))

(define (test-type-of)
  (begin
    (console.log
     (lst->string
      '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XX typeOf XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "--- with scm string\n"
        "    expect : object")))
    (console.log (type-of "test string"))
    (console.log
     (lst->string
      '("--- with js string\n"
        "    expect : string")))
    (console.log (type-of (##inline-host-expression "'test string';")))
    (console.log
     (lst->string
      '("--- with void scm value\n"
        "    expect : undefined")))
    (console.log (type-of (void)))
    (console.log
     (lst->string
      '("--- with nullObj\n"
        "    expect : object")))
    (console.log (type-of (null-obj)))
    (console.log
     (lst->string
      '("--- with inline-exp with undefined\n"
        "    expect : undefined")))
    (console.log (type-of (undefined-obj)))))

(define (test-document-write)
  (begin
    (console.log
     (lst->string
      '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XX document.write XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "--- with scm html object : 'a test text'\n"
        "    expect : undefined")))
    (console.log (document.write (<p> "a test text")))
    (console.log
     (lst->string
      '("--- with scm string : 'test string 1 '\n"
        "    expect : undefined")))
    (console.log (document.write "test string 1 "))
    (console.log
     (lst->string
      '("--- with js string : 'test string 2'\n"
        "    expect : undefined")))
    (console.log (document.write (##inline-host-expression "'test string 2';")))
    (console.log
     (lst->string
      '("--- with scm number : 33.3\n"
        "    expect : undefined")))
    (console.log (document.write 33.3))
    (console.log
     (lst->string
      '("--- with js number : 44.4\n"
        "    expect : undefined")))
    (console.log (document.write (##inline-host-expression "44.4;")))
    (console.log
     (lst->string
      '("--- with nullObj\n"
        "    expect : false")))
    (console.log (document.write (null-obj)))
    (console.log
     (lst->string
      '("--- with inline-exp with undefined\n"
        "    expect : false")))
    (console.log (document.write (undefined-obj)))
    (##inline-host-statement "document.body.innerHTML = '';")))

(define (test-console-log)
  (begin
    (console.log
     (lst->string
      '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XX console.log XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "--- with scm html object\n"
        "    expect : <p>a test text</p>")))
    (console.log (<p> "a test text"))
    (console.log "    with inline call : should be different")
    (##inline-host-statement "console.log(@1@);" (<p> "a test text"))
    (console.log
     (lst->string
      '("--- with scm string\n"
        "    expect : test string 1\n"
        "test string 1\n"
        "    with inline call : should be different")))
    (##inline-host-statement "console.log(@1@);" "test string 1")
    (console.log
     (lst->string
      '("--- with js string\n"
        "    expect : test string 2")))
    (console.log (##inline-host-expression "'test string 2';"))
    (console.log "    with inline call : should be the same")
    (##inline-host-statement "console.log('test string 2');")
    (console.log
     (lst->string
      '("--- with scm number\n"
        "    expect : 33.3")))
    (console.log 33.3)
    (console.log "    with inline call : should be different")
    (##inline-host-statement "console.log(@1@);" 33.3)
    (console.log
     (lst->string
      '("--- with js number\n"
        "    expect : 44.4")))
    (console.log (##inline-host-expression "44.4;"))
    (console.log "    with inline call : should be the same")
    (##inline-host-statement "console.log(44.4);")
    (console.log
     (lst->string
      '("--- with nullObj\n"
        "    expect : null")))
    (console.log (null-obj))
    (console.log "    with inline call : should be the same")
    (##inline-host-statement "console.log(@1@);" (null-obj))
    (console.log
     (lst->string
      '("--- with inline-exp with undefined\n"
        "    expect : undefined")))
    (console.log (undefined-obj))
    (console.log "    with inline call : should be the same")
    (##inline-host-statement "console.log(@1@);" (undefined-obj))))

(define (test-dom-obj?)
  (begin
    (console.log
     (lst->string
      '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XX isDOMObj XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "--- with scm string\n"
        "    expect : false")))
    (console.log (dom-obj? "test string 1"))
    (console.log
     (lst->string
      '("--- with js string\n"
        "    expect : false")))
    (console.log (dom-obj? (##inline-host-expression "'test string 2';")))
    (console.log
     (lst->string
      '("--- with nullObj\n"
        "    expect : false")))
    (console.log (dom-obj? (null-obj)))
    (console.log
     (lst->string
      '("--- with inline-exp with undefined\n"
        "    expect : false")))
    (console.log (dom-obj? (undefined-obj)))
    (console.log
     (lst->string
      '("--- with documentObj\n"
        "    expect : true")))
    (console.log (dom-obj? (document-obj)))
    (console.log
     (lst->string
      '("--- with freshly created node\n"
        "    expect : true")))
    (console.log (dom-obj? (##inline-host-expression "document.createElement('P');")))))

(define (test-document-obj?)
  (begin
    (console.log
     (lst->string
      '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XX isDocumentObj XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "--- with scm string\n"
        "    expect : false")))
    (console.log (document-obj? "test string 1"))
    (console.log
     (lst->string
      '("--- with js string\n"
        "    expect : false")))
    (console.log (document-obj? (##inline-host-expression "'test string 2';")))
    (console.log
     (lst->string
      '("--- with nullObj\n"
        "    expect : false")))
    (console.log (document-obj? (null-obj)))
    (console.log
     (lst->string
      '("--- with inline-exp with undefined\n"
        "    expect : false")))
    (console.log (document-obj? (undefined-obj)))
    (console.log
     (lst->string
      '("--- with freshly created node\n"
        "    expect : false")))
    (console.log (document-obj? (##inline-host-expression "document.createElement('P');")))
    (console.log
     (lst->string
      '("--- with documentObj\n"
        "    expect : true")))
    (console.log (document-obj? (document-obj)))))

(define (test-get-element-by-id)
  (let ((my-id "myP"))
    (let ((my-new-p
           (<p>
            id: my-id
            "this is my p")))
      (begin
        (console.log
         (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX getElementById XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- write <p id='myP'>this is my p</p>")))
        (document.write my-new-p)
        (console.log "--- with valid scm string")
        (console.log (get-element-by-id my-id))
        (console.log
         (lst->string
          '("--- with invalid scm string\n"
            "    expect : null")))
        (console.log (get-element-by-id (string-append "invalid" my-id)))
        (console.log
         (lst->string
          '("--- with scm object (not a string)\n"
            "    expect : null")))
        (console.log (get-element-by-id my-new-p))
        (console.log
         (lst->string
          '("--- with js string\n"
            "    expect : null")))
        (console.log (get-element-by-id (scm->js my-id)))
        (##inline-host-statement "document.body.innerHTML = '';")))))

(define (test-query-selector)
  (let ((my-id "myDiv"))
    (let ((my-new-p (<p> "outer p"))
          (my-new-div
           (<div>
            id: my-id
            (<p> "inner p"))))
      (begin
        (console.log
         (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX querySelector XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- write <p>outer p</p>\n"
            "          <div>\n"
            "              <p>inner p</p>\n"
            "          </div>")))
        (document.write my-new-p)
        (document.write my-new-div)
        (let ((div-by-id (get-element-by-id my-id))
              (valid-scm-selector "p")
              (invalid-scm-selector ".not a .valid .selector")
              (js-selector (scm->js "p")))
          (begin
            (console.log
             (lst->string
              '("--- with valid scm string\n"
                "    expect : <p>outer p</p>")))
            (console.log (query-selector valid-scm-selector))
            (console.log
             (lst->string
              '("--- with valid scm string on frame\n"
                "    expect : <p>inner p</p>")))
            (console.log (query-selector valid-scm-selector div-by-id))
            (console.log
             (lst->string
              '("--- with invalid scm string\n"
                "    expect : null")))
            (console.log (query-selector invalid-scm-selector))
            (console.log
             (lst->string
              '("--- with invalid scm string on frame\n"
                "    expect : null")))
            (console.log (query-selector invalid-scm-selector div-by-id))
            (console.log
             (lst->string
              '("--- with scm object other than a string\n"
                "    expect : null")))
            (console.log (query-selector my-new-p))
            (console.log
             (lst->string
              '("--- with js string\n"
                "    expect : null")))
            (console.log (query-selector js-selector))
            (console.log
             (lst->string
              '("--- with valid scm string on scm objet\n"
                "    expect : null")))
            (console.log (query-selector valid-scm-selector valid-scm-selector))
            (console.log
             (lst->string
              '("--- with valid scm string on js objet\n"
                "    expect : null")))
            (console.log (query-selector valid-scm-selector js-selector))
            (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test-query-selector-all)
  (let ((my-id "myDiv"))
    (let ((my-new-p (<p> "outer p"))
          (my-new-div
           (<div>
            id: my-id
            (<p> "inner p"))))
      (begin
        (console.log
         (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX querySelectorAll XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- write <p>outer p</p>\n"
            "          <div>\n"
            "              <p>inner p</p>\n"
            "          </div>")))
        (document.write my-new-p)
        (document.write my-new-div)
        (let ((div-by-id (get-element-by-id my-id))
              (valid-scm-selector "p")
              (invalid-scm-selector ".not a .valid .selector")
              (js-selector (scm->js "p")))
          (begin
            (console.log
             (lst->string
              '("--- with valid scm string\n"
                "    expect : list with : <p>outer p</p> AND <p>inner p</p>")))
            (console.log (query-selector-all valid-scm-selector))
            (console.log
             (lst->string
              '("--- with valid scm string on frame\n"
                "    expect : list with : <p>inner p</p>")))
            (console.log (query-selector-all valid-scm-selector div-by-id))
            (console.log
             (lst->string
              '("--- with invalid scm string\n"
                "    expect : empty list")))
            (console.log (query-selector-all invalid-scm-selector))
            (console.log
             (lst->string
              '("--- with invalid scm string on frame\n"
                "    expect : empty list")))
            (console.log (query-selector-all invalid-scm-selector div-by-id))
            (console.log
             (lst->string
              '("--- with scm object other than a string\n"
                "    expect : null")))
            (console.log (query-selector-all my-new-p))
            (console.log
             (lst->string
              '("--- with js string\n"
                "    expect : null")))
            (console.log (query-selector-all js-selector))
            (console.log
             (lst->string
              '("--- with valid scm string on scm objet\n"
                "    expect : null")))
            (console.log (query-selector-all valid-scm-selector valid-scm-selector))
            (console.log
             (lst->string
              '("--- with valid scm string on js not objet (not DOM nor document)\n"
                "    expect : null")))
            (console.log (query-selector-all valid-scm-selector js-selector))
            (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test-get-attribute)
  (let ((my-id "myInput")
        (my-classes "'c1 c2 c3'")
        (my-value "'a value'")
        (my-custom-att "custom-att")
        (my-custom-val 11.1))
    (let ((my-new-input
           (<input>
            type: 'text
            id: my-id
            name: my-id
            class: my-classes
            value: my-value)))
      (begin
        (console.log
         (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX getAttribute XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- write <input type='text' id='myInput' name='myInput' class='c1 c2 c3' value='a value' custom-att='11.1'>")))
        (document.write my-new-input)
        (let ((input-by-id (get-element-by-id my-id))
              (my-custom-att-js (scm->js my-custom-att)))
          (begin
            (set-attribute input-by-id my-custom-att my-custom-val)
            (console.log
             (lst->string
              `("--- select class on inserted input\n"
                "    expect : " ,my-classes)))
            (console.log (get-attribute input-by-id "class"))
            (console.log
             (lst->string
              `("--- select value on inserted input\n"
                "    expect : " ,my-value)))
            (console.log (get-attribute input-by-id "value"))
            (console.log
             (lst->string
              `("--- select custom-att (initialised with 11.1) on inserted input\n"
                "    expect : '" ,(number->string my-custom-val) "'")))
            (console.log (get-attribute input-by-id my-custom-att))
            (console.log
             (lst->string
              '("--- select invalid attribute on inserted input\n"
                "    expect : null")))
            (console.log (get-attribute input-by-id "not-an-active-att"))
            (console.log
             (lst->string
              '("--- select with scm object (not a string) on inserted input\n"
                "    expect : false")))
            (console.log (get-attribute input-by-id my-new-input))
            (console.log
             (lst->string
              '("--- select with js object on inserted input\n"
                "    expect : false")))
            (console.log (get-attribute input-by-id my-custom-att-js))
            (console.log
             (lst->string
              '("--- select on js object (not a DOM object)\n"
                "    expect : false")))
            (console.log (get-attribute my-custom-att-js my-custom-att))
            (console.log
             (lst->string
              '("--- select on scm object\n"
                "    expect : false")))
            (console.log (get-attribute my-new-input my-custom-att))
            (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test-has-attribute)
  (let ((my-id "myInput")
        (my-classes "'c1 c2 c3'")
        (my-value "'a value'")
        (my-custom-att "custom-att")
        (my-custom-val 11.1))
    (let ((my-new-input
           (<input>
            type: 'text
            id: my-id
            name: my-id
            class: my-classes
            value: my-value)))
      (begin
        (console.log
         (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX hasAttribute XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- write <input type='text' id='myInput' name='myInput' class='c1 c2 c3' value='a value' custom-att='11.1'>")))
        (document.write my-new-input)
        (let ((input-by-id (get-element-by-id my-id))
              (my-custom-att-js (scm->js my-custom-att)))
          (begin
            (set-attribute input-by-id my-custom-att my-custom-val)
            (console.log
             (lst->string
              '("--- select class on inserted input\n"
                "    expect : true")))
            (console.log (has-attribute input-by-id "class"))
            (console.log
             (lst->string
              '("--- select value on inserted input\n"
                "    expect : true")))
            (console.log (has-attribute input-by-id "value"))
            (console.log
             (lst->string
              '("--- select custom-att (initialised with 11.1) on inserted input\n"
                "    expect : true")))
            (console.log (has-attribute input-by-id my-custom-att))
            (console.log
             (lst->string
              '("--- select invalid attribute on inserted input\n"
                "    expect : false")))
            (console.log (has-attribute input-by-id "not-an-active-att"))
            (console.log
             (lst->string
              '("--- select with scm object (not a string) on inserted input\n"
                "    expect : false")))
            (console.log (has-attribute input-by-id my-new-input))
            (console.log
             (lst->string
              '("--- select with js object on inserted input\n"
                "    expect : false")))
            (console.log (has-attribute input-by-id my-custom-att-js))
            (console.log
             (lst->string
              '("--- select on js object (not a DOM object)\n"
                "    expect : false")))
            (console.log (has-attribute my-custom-att-js my-custom-att))
            (console.log
             (lst->string
              '("--- select on scm object\n"
                "    expect : false")))
            (console.log (has-attribute my-new-input my-custom-att))
            (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test-set-attribute)
  (let ((my-id "myInput")
        (my-classes "'c1 c2 c3'")
        (my-value "'a value'")
        (my-new-value "'a brand new value'")
        (my-custom-att "custom-att")
        (my-custom-val 11.1)
        (my-new-custom-val "'new custom val'"))
    (let ((my-new-input
           (<input>
            type: 'text
            id: my-id
            name: my-id
            value: my-value)))
      (begin
        (console.log
         (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX setAttribute XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- write <input type='text' id='myInput' name='myInput' value='a value' custom-att='11.1'>")))
        (document.write my-new-input)
        (let ((input-by-id (get-element-by-id my-id))
              (my-custom-att-js (scm->js my-custom-att)))
          (begin
            (set-attribute input-by-id my-custom-att my-custom-val)
            (console.log
             (lst->string
              '("---------------------------------------------------\n"
                "-- 1 : before modif -------------------------------\n"
                "---------------------------------------------------\n"
                "--- class\n"
                "    expect : null")))
            (console.log (get-attribute input-by-id "class"))
            (console.log
             (lst->string
              `("--- value\n"
                "    expect : " ,my-value)))
            (console.log (get-attribute input-by-id "value"))
            (console.log
             (lst->string
              `("--- custom-att (initialised with 11.1)\n"
                "    expect : '" ,(number->string my-custom-val) "'")))
            (console.log (get-attribute input-by-id my-custom-att))
            (console.log
             (lst->string
              '("---------------------------------------------------\n"
                "-- 2 : setAttribute -------------------------------\n"
                "---------------------------------------------------\n"
                "--- with scm att (not a string) on inserted input\n"
                "    expect : false")))
            (console.log (set-attribute input-by-id my-new-input "val 2"))
            (console.log
             (lst->string
              '("--- with js att on inserted input\n"
                "    expect : false")))
            (console.log (set-attribute input-by-id my-custom-att-js "val 3"))
            (console.log
             (lst->string
              '("--- on js object (not a DOM object)\n"
                "    expect : false")))
            (console.log (set-attribute my-custom-att my-custom-att "val 4"))
            (console.log
             (lst->string
              '("--- on scm object\n"
                "    expect : false")))
            (console.log (set-attribute my-new-input my-custom-att "val 5"))
            (console.log
             (lst->string
              '("--- to js val\n"
                "    expect : false")))
            (console.log (set-attribute input-by-id my-custom-att my-custom-att-js))
            (console.log
             (lst->string
              '("--- set class\n"
                "    expect : undefined")))
            (console.log (set-attribute input-by-id "class" my-classes))
            (console.log
             (lst->string
              '("--- set value\n"
                "    expect : undefined")))
            (console.log (set-attribute input-by-id "value" my-new-value))
            (console.log
             (lst->string
              '("--- set custom-att\n"
                "    expect : undefined")))
            (console.log (set-attribute input-by-id my-custom-att my-new-custom-val))
            (console.log
             (lst->string
              `("---------------------------------------------------\n"
                "-- 3 : after modif --------------------------------\n"
                "---------------------------------------------------\n"
                "--- class\n"
                "    expect : " ,my-classes)))
            (console.log (get-attribute input-by-id "class"))
            (console.log
             (lst->string
              `("--- value\n"
                "    expect : " ,my-new-value)))
            (console.log (get-attribute input-by-id "value"))
            (console.log
             (lst->string
              `("--- custom-att (initialised with 11.1)\n"
                "    expect : " ,my-new-custom-val)))
            (console.log (get-attribute input-by-id my-custom-att))
            (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test-remove-attribute)
  (let ((my-id "myInput")
        (my-value "'a value'")
        (my-custom-att "custom-att")
        (my-custom-val 11.1))
    (let ((my-new-input
           (<input>
            type: 'text
            id: my-id
            name: my-id
            value: my-value)))
      (begin
        (console.log
         (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX removeAttribute XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- write <input type='text' id='myInput' name='myInput' value='a value' custom-att='11.1'>")))
        (document.write my-new-input)
        (let ((input-by-id (get-element-by-id my-id))
              (my-custom-att-js (scm->js my-custom-att)))
          (begin
            (set-attribute input-by-id my-custom-att my-custom-val)
            (console.log
             (lst->string
              '("---------------------------------------------------\n"
                "-- 1 : before modif -------------------------------\n"
                "---------------------------------------------------\n"
                "--- class\n"
                "    expect : null")))
            (console.log (get-attribute input-by-id "class"))
            (console.log
             (lst->string
              `("--- value\n"
                "    expect : " ,my-value)))
            (console.log (get-attribute input-by-id "value"))
            (console.log
             (lst->string
              `("--- custom-att (initialised with 11.1)\n"
                "    expect : '" ,(number->string my-custom-val) "'")))
            (console.log (get-attribute input-by-id my-custom-att))
            (console.log
             (lst->string
              '("---------------------------------------------------\n"
                "-- 2 : removeAttribute ----------------------------\n"
                "---------------------------------------------------\n"
                "--- with scm att (not a string) on inserted input\n"
                "    expect : false")))
            (console.log (remove-attribute input-by-id my-new-input))
            (console.log
             (lst->string
              '("--- with js att on inserted input\n"
                "    expect : false")))
            (console.log (remove-attribute input-by-id my-custom-att-js))
            (console.log
             (lst->string
              '("--- on js object (not a DOM object)\n"
                "    expect : false")))
            (console.log (remove-attribute my-custom-att my-custom-att))
            (console.log
             (lst->string
              '("--- on scm object\n"
                "    expect : false")))
            (console.log (remove-attribute my-new-input my-custom-att))
            (console.log
             (lst->string
              '("--- remove class\n"
                "    expect : undefined")))
            (console.log (remove-attribute input-by-id "class"))
            (console.log
             (lst->string
              '("--- remove value\n"
                "    expect : undefined")))
            (console.log (remove-attribute input-by-id "value"))
            (console.log
             (lst->string
              '("--- remove custom-att\n"
                "    expect : undefined")))
            (console.log (remove-attribute input-by-id my-custom-att))
            (console.log
             (lst->string
              '("---------------------------------------------------\n"
                "-- 3 : after modif --------------------------------\n"
                "---------------------------------------------------\n"
                "--- inserted input after removals")))
            (console.log (get-element-by-id my-id))
            (console.log
             (lst->string
              '("--- class\n"
                "    expect : null")))
            (console.log (get-attribute input-by-id "class"))
            (console.log
             (lst->string
              '("--- value\n"
                "    expect : null")))
            (console.log (get-attribute input-by-id "value"))
            (console.log
             (lst->string
              '("--- custom-att (initialised with 12.345)\n"
                "    expect : null")))
            (console.log (get-attribute input-by-id my-custom-att))
            (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test-get-element-type)
  (let ((my-id "myDiv"))
    (let ((my-new-div (<div> id: my-id)))
      (begin
        (console.log
         (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX getElementType XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- write <div id='myDiv'></div>")))
        (document.write my-new-div)
        (console.log
         (lst->string
          '("--- inserted div\n"
            "    expect : DIV")))
        (console.log (get-element-type (get-element-by-id my-id)))
        (console.log
         (lst->string
          '("--- on js object not DOM\n"
            "    expect : undefined")))
        (console.log (get-element-type (scm->js my-id)))
        (console.log
         (lst->string
          '("--- on scm object\n"
            "    expect : false")))
        (console.log (get-element-type my-new-div))
        (##inline-host-statement "document.body.innerHTML = '';")))))

(define (test-element-of-type?)
  (let ((my-id "myDiv"))
    (let ((my-new-div (<div> id: my-id)))
      (begin
        (console.log
         (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX isElementOfType XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- write <div id='myDiv'></div>")))
        (document.write my-new-div)
        (let ((my-div (get-element-by-id my-id))
              (js-id (scm->js my-id)))
          (begin
            (console.log
             (lst->string
              '("--- inserted div is of type 'DIV'?\n"
                "    expect : true")))
            (console.log (element-of-type? my-div "DIV"))
            (console.log
             (lst->string
              '("--- inserted div is of type 'P'?\n"
                "    expect : false")))
            (console.log (element-of-type? my-div "P"))
            (console.log
             (lst->string
              '("--- js string is of type 'STRING'?\n"
                "    expect : false")))
            (console.log (element-of-type? js-id "STRING"))
            (console.log
             (lst->string
              '("--- js string is of type 'DIV'?\n"
                "    expect : false")))
            (console.log (element-of-type? js-id "DIV"))
            (console.log
             (lst->string
              '("--- scm object is of type 'DIV'?\n"
                "    expect : false")))
            (console.log (element-of-type? my-new-div "DIV"))
            (console.log
             (lst->string
              '("--- scm object is of type 'P'?\n"
                "    expect : false")))
            (console.log (element-of-type? my-new-div "P"))
            (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test-first-child)
  (let ((my-id "myDiv"))
    (let ((my-new-div
           (<div>
            id: my-id
            (<div>
             (<p> "inner p")))))
      (begin
        (console.log
         (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX firstChild XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- write <div id='myDiv'><div><p>inner p</p></div></div>")))
        (document.write my-new-div)
        (let ((div-by-id (get-element-by-id my-id)))
          (begin
            (console.log
             (lst->string
              '("--- inserted div\n"
                "    expect : <div><p>inner p</p></div>")))
            (console.log (first-child div-by-id))
            (console.log
             (lst->string
              '("--- on js object not DOM\n"
                "    expect : false")))
            (console.log (first-child (scm->js my-id)))
            (console.log
             (lst->string
              '("--- on scm object\n"
                "    expect : false")))
            (console.log (first-child my-new-div))
            (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test-parent-node)
  (let ((my-id "myDiv"))
    (let ((my-new-div
           (<div>
            (<p>
             id: my-id
             "inner p"))))
      (begin
        (console.log
         (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX parentNode XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- write <div><p id='myDiv'>inner p</p></div>")))
        (document.write my-new-div)
        (let ((p-by-id (get-element-by-id my-id)))
          (begin
            (console.log
             (lst->string
              '("--- p inside inserted div\n"
                "    expect : <div><p>inner p</p></div>")))
            (console.log (parent-node p-by-id))
            (console.log
             (lst->string
              '("--- on js object not DOM\n"
                "    expect : false")))
            (console.log (parent-node (scm->js my-id)))
            (console.log
             (lst->string
              '("--- on scm object\n"
                "    expect : false")))
            (console.log (parent-node my-new-div))
            (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test-get-new-node)
  (begin
    (console.log
     (lst->string
      '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XX getNewNode XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")))
    (console.log (get-new-node))))

(define (test-remove-node)
  (let ((my-id-1 "myDiv1"))
    (let ((my-new-div-1 (<div> id: my-id-1))
          (my-new-div-2 (get-new-node)))
      (begin
        (console.log
         (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX removeNode XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- write <div id='myDiv1'></div>")))
        (document.write my-new-div-1)
        (let ((div-by-id-1 (get-element-by-id my-id-1)))
          (begin
            (console.log
             (lst->string
              '("---------------------------------------------------\n"
                "-- 1 : using removeNode ---------------------------\n"
                "---------------------------------------------------\n"
                "--- remove inserted div\n"
                "    expect : undefined")))
            (console.log (remove-node div-by-id-1))
            (console.log
             (lst->string
              '("--- remove uninserted div\n"
                "    expect : false")))
            (console.log (remove-node my-new-div-2))
            (console.log
             (lst->string
              '("--- remove js string\n"
                "    expect : false")))
            (console.log (remove-node (scm->js my-id-1)))
            (console.log
             (lst->string
              '("--- remove scm string\n"
                "    expect : false")))
            (console.log (remove-node my-id-1))
            (console.log
             (lst->string
              '("---------------------------------------------------\n"
                "-- 2 : after modif --------------------------------\n"
                "---------------------------------------------------\n"
                "--- body\n"
                "    expect : <body></body>")))
            (console.log (query-selector "body"))
            (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test-set-inner-html)
  (let ((my-id-1 "myDiv1")
        (my-id-2 "myDiv2")
        (my-id-3 "myDiv3"))
    (let ((my-new-div-1
           (<div>
            id: my-id-1
            (<p> "p no. 1")))
          (my-new-div-2
           (<div>
            id: my-id-2
            (<p> "p no. 2")))
          (my-new-div-3
           (<div>
            id: my-id-3
            (<p> "p no. 3")))
          (new-inner-1
           (<ul>
            (<li> "First")
            (<li> "Second")
            (<li> "...")))
          (new-inner-2 "<span>new inner content no. 2</span>")
          (new-inner-3 (scm->js "<span>new inner content no. 3</span>")))
      (begin
        (console.log
         (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX setInnerHTML XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- write <div id='myDiv1'><p>p no. 1</p></div>\n"
            "          <div id='myDiv2'><p>p no. 2</p></div>\n"
            "          <div id='myDiv3'><p>p no. 3</p></div>")))
        (document.write my-new-div-1)
        (document.write my-new-div-2)
        (document.write my-new-div-3)
        (let ((div-by-id-1 (get-element-by-id my-id-1))
              (div-by-id-2 (get-element-by-id my-id-2))
              (div-by-id-3 (get-element-by-id my-id-3)))
          (begin
            (console.log
             (lst->string
              '("---------------------------------------------------\n"
                "-- 1 : using setInnerHTML -------------------------\n"
                "---------------------------------------------------\n"
                "--- replace div 1's content with html object\n"
                "    expect : undefined")))
            (console.log (set-inner-html div-by-id-1 new-inner-1))
            (console.log
             (lst->string
              '("--- replace div 2's content with scm string\n"
                "    expect : undefined")))
            (console.log (set-inner-html div-by-id-2 new-inner-2))
            (console.log
             (lst->string
              '("--- replace div 3's content with js string\n"
                "    expect : undefined")))
            (console.log (set-inner-html div-by-id-3 new-inner-3))
            (console.log
             (lst->string
              '("--- replace js object's content (not DOM)\n"
                "    expect : false")))
            (console.log (set-inner-html new-inner-3 new-inner-2))
            (console.log
             (lst->string
              '("--- replace scm object's content\n"
                "    expect : false")))
            (console.log (set-inner-html my-new-div-1 new-inner-2))
            (console.log
             (lst->string
              '("---------------------------------------------------\n"
                "-- 2 : after modif --------------------------------\n"
                "---------------------------------------------------\n"
                "--- div 1\n"
                "    expect : <div id='myDiv1'><ul><li>First</li><li>Seconde</li><li>...</li></ul></div>")))
            (console.log (get-element-by-id my-id-1))
            (console.log
             (lst->string
              '("--- div 2\n"
                "    expect : <div id='myDiv2'><span>new inner content no. 2</span></div>")))
            (console.log (get-element-by-id my-id-2))
            (console.log
             (lst->string
              '("--- div 3\n"
                "    expect : <div id='myDiv3'><span>new inner content no. 3</span></div>")))
            (console.log (get-element-by-id my-id-3))
            (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test-append-node)
  (let ((my-id-1 "myDiv1")
        (my-id-2 "myDiv2"))
    (let ((my-new-div-1 (<div> id: my-id-1))
          (my-new-div-2 (<div> id: my-id-2)))
      (begin
        (console.log
         (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX appendNode XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- write <div id='myDiv1'></div>\n"
            "          <div id='myDiv2'></div>")))
        (document.write my-new-div-1)
        (document.write my-new-div-2)
        (let ((div-by-id-1 (get-element-by-id my-id-1))
              (div-by-id-2 (get-element-by-id my-id-2))
              (my-id-js (scm->js my-id-1)))
          (begin
            (console.log
             (lst->string
              '("---------------------------------------------------\n"
                "-- 1 : using appendNode ---------------------------\n"
                "---------------------------------------------------\n"
                "--- append div 2 to div 1\n"
                "    expect : undefined")))
            (console.log (append-node div-by-id-1 div-by-id-2))
            (console.log
             (lst->string
              '("--- append js string to div 1\n"
                "    expect : false")))
            (console.log (append-node div-by-id-1 my-id-js))
            (console.log
             (lst->string
              '("--- append scm string to div 1\n"
                "    expect : false")))
            (console.log (append-node div-by-id-1 my-id-1))
            (console.log
             (lst->string
              '("--- append div 2 to js object (not DOM)\n"
                "    expect : false")))
            (console.log (append-node my-id-js div-by-id-2))
            (console.log
             (lst->string
              '("--- append js string to js object (not DOM)\n"
                "    expect : false")))
            (console.log (append-node my-id-js my-id-js))
            (console.log
             (lst->string
              '("--- append scm string to js object (not DOM)\n"
                "    expect : false")))
            (console.log (append-node my-id-js my-id-1))
            (console.log
             (lst->string
              '("--- append div 2 to scm object\n"
                "    expect : false")))
            (console.log (append-node my-new-div-1 div-by-id-2))
            (console.log
             (lst->string
              '("--- append js string to scm object\n"
                "    expect : false")))
            (console.log (append-node my-new-div-1 my-id-js))
            (console.log
             (lst->string
              '("--- append scm string to scm object\n"
                "    expect : false")))
            (console.log (append-node my-new-div-1 my-id-1))
            (console.log
             (lst->string
              '("---------------------------------------------------\n"
                "-- 2 : after modif --------------------------------\n"
                "---------------------------------------------------\n"
                "--- div 1\n"
                "    expect : <div id='myDiv1'><div id='myDiv2'></div></div>")))
            (console.log (get-element-by-id my-id-1))
            (console.log
             (lst->string
              '("--- div 2\n"
                "    expect : <div id='myDiv2'></div>")))
            (console.log (get-element-by-id my-id-2))
            (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test-append-html)
  (let ((my-id-1 "myDiv1")
        (my-id-2 "myDiv2")
        (my-id-3 "myDiv3"))
    (let ((my-new-div-1 (<div> id: my-id-1))
          (my-new-div-2 (<div> id: my-id-2))
          (my-new-div-3 (<div> id: my-id-3))
          (new-inner-1
           (<ul>
            (<li> "First")
            (<li> "Second")
            (<li> "...")))
          (new-inner-2 "<span>new inner content no. 2</span>")
          (new-inner-3 (scm->js "<span>new inner content no. 3</span>")))
      (begin
        (console.log
         (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX appendHTML XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- write <div id='myDiv1'></div>\n"
            "          <div id='myDiv2'></div>\n"
            "          <div id='myDiv3'></div>")))
        (document.write my-new-div-1)
        (document.write my-new-div-2)
        (document.write my-new-div-3)
        (let ((div-by-id-1 (get-element-by-id my-id-1))
              (div-by-id-2 (get-element-by-id my-id-2))
              (div-by-id-3 (get-element-by-id my-id-3)))
          (begin
            (console.log
             (lst->string
              '("---------------------------------------------------\n"
                "-- 1 : using appendHTML ---------------------------\n"
                "---------------------------------------------------\n"
                "--- append html object div 1\n"
                "    expect : undefined")))
            (console.log (set-inner-html div-by-id-1 new-inner-1))
            (console.log
             (lst->string
              '("--- append scm string div 2\n"
                "    expect : undefined")))
            (console.log (append-html div-by-id-2 new-inner-2))
            (console.log
             (lst->string
              '("--- append js string div 3\n"
                "    expect : undefined")))
            (console.log (append-html div-by-id-3 new-inner-3))
            (console.log
             (lst->string
              '("--- on js object (not DOM)\n"
                "    expect : false")))
            (console.log (append-html new-inner-3 new-inner-2))
            (console.log
             (lst->string
              '("--- on scm object\n"
                "    expect : false")))
            (console.log (append-html my-new-div-1 new-inner-2))
            (console.log
             (lst->string
              '("---------------------------------------------------\n"
                "-- 2 : after modif --------------------------------\n"
                "---------------------------------------------------\n"
                "--- div 1\n"
                "    expect : <div id='myDiv1'><ul><li>First</li><li>Seconde</li><li>...</li></ul></div>")))
            (console.log (get-element-by-id my-id-1))
            (console.log
             (lst->string
              '("--- div 2\n"
                "    expect : <div id='myDiv2'><span>new inner content no. 2</span></div>")))
            (console.log (get-element-by-id my-id-2))
            (console.log
             (lst->string
              '("--- div 3\n"
                "    expect : <div id='myDiv3'><span>new inner content no. 3</span></div>")))
            (console.log (get-element-by-id my-id-3))
            (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test-insert-node-before)
  (let ((my-id-1 "myDiv1")
        (my-id-11 "myP")
        (my-id-2 "myDiv2")
        (my-id-3 "myDiv3"))
    (let ((my-new-div-1
           (<div>
            id: my-id-1
            (<p> id: my-id-11)))
          (my-new-div-2 (<div> id: my-id-2))
          (my-new-div-3 (<div> id: my-id-3)))
      (begin
        (console.log
         (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX insertNodeBefore XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- write <div id='myDiv1'><p id='myP'></p></div>\n"
            "          <div id='myDiv2'></div>\n"
            "          <div id='myDiv3'></div>")))
        (document.write my-new-div-1)
        (document.write my-new-div-2)
        (document.write my-new-div-3)
        (let ((div-by-id-1 (get-element-by-id my-id-1))
              (p-by-id (get-element-by-id my-id-11))
              (div-by-id-2 (get-element-by-id my-id-2))
              (div-by-id-3 (get-element-by-id my-id-3))
              (my-id-js (scm->js my-id-1)))
          (begin
            (console.log
             (lst->string
              '("---------------------------------------------------\n"
                "-- 1 : using insertNodeBefore ---------------------\n"
                "---------------------------------------------------\n"
                "--- insert div 2 before div 1's p\n"
                "    expect : undefined")))
            (console.log (insert-node-before div-by-id-1 div-by-id-2 p-by-id))
            (console.log
             (lst->string
              '("--- insert div 1's p before div 3 in div 1\n"
                "    expect : false")))
            (console.log (insert-node-before div-by-id-1 p-by-id div-by-id-3))
            (console.log
             (lst->string
              '("--- insert div 3 in div 1 before js string\n"
                "    expect : false")))
            (console.log (insert-node-before div-by-id-1 div-by-id-3 my-id-js))
            (console.log
             (lst->string
              '("--- insert div 3 in div 1 before scm string\n"
                "    expect : false")))
            (console.log (insert-node-before div-by-id-1 div-by-id-3 my-id-1))
            (console.log
             (lst->string
              '("--- insert div 3 in div 1 before scm object\n"
                "    expect : false")))
            (console.log (insert-node-before div-by-id-1 div-by-id-3 my-new-div-2))
            (console.log
             (lst->string
              '("--- insert js string in div 1 before div 2\n"
                "    expect : false")))
            (console.log (insert-node-before div-by-id-1 my-id-js div-by-id-2))
            (console.log
             (lst->string
              '("--- insert scm string in div 1 before div 2\n"
                "    expect : false")))
            (console.log (insert-node-before div-by-id-1 my-id-1 div-by-id-2))
            (console.log
             (lst->string
              '("--- insert scm object in div 1 before div 2\n"
                "    expect : false")))
            (console.log (insert-node-before div-by-id-1 my-new-div-3 div-by-id-2))
            (console.log
             (lst->string
              '("--- insert div 3 in js string before div 2\n"
                "    expect : false")))
            (console.log (insert-node-before my-id-js div-by-id-3 div-by-id-2))
            (console.log
             (lst->string
              '("--- insert div 3 in scm string before div 2\n"
                "    expect : false")))
            (console.log (insert-node-before my-id-1 div-by-id-3 div-by-id-2))
            (console.log
             (lst->string
              '("--- insert div 3 in scm object before div 2\n"
                "    expect : false")))
            (console.log (insert-node-before my-new-div-1 div-by-id-3 div-by-id-2))
            (console.log
             (lst->string
              '("---------------------------------------------------\n"
                "-- 2 : after modif --------------------------------\n"
                "---------------------------------------------------\n"
                "--- div 1\n"
                "    expect : <div id='myDiv1'><div id='myDiv2'></div><p id='myP'></p></div>")))
            (console.log (get-element-by-id my-id-1))
            (console.log
             (lst->string
              '("--- div 3\n"
                "    expect : <div id='myDiv3'></div>")))
            (console.log (get-element-by-id my-id-3))
            (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test-insert-html-before)
  (let ((my-id-1 "myDiv1")
        (my-id-11 "myP")
        (my-id-2 "myDiv2")
        (my-id-3 "myDiv3"))
    (let ((my-new-div-1
           (<div>
            id: my-id-1
            (<p> id: my-id-11)))
          (my-new-div-2 (<div> id: my-id-2))
          (my-new-div-3 (<div> id: my-id-3))
          (my-new-div-4 (<div>)))
      (begin
        (console.log
         (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX insertHTMLBefore XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- write <div id='myDiv1'><p id='myP'></p></div>\n"
            "          <div id='myDiv2'></div>\n"
            "          <div id='myDiv3'></div>")))
        (document.write my-new-div-1)
        (document.write my-new-div-2)
        (document.write my-new-div-3)
        (let ((div-by-id-1 (get-element-by-id my-id-1))
              (p-by-id (get-element-by-id my-id-11))
              (div-by-id-2 (get-element-by-id my-id-2))
              (div-by-id-3 (get-element-by-id my-id-3))
              (my-id-js (scm->js my-id-1)))
          (begin
            (console.log
             (lst->string
              '("---------------------------------------------------\n"
                "-- 1 : using insertHTMLBefore ---------------------\n"
                "---------------------------------------------------\n"
                "--- insert div 2 before div 1's p\n"
                "    expect : undefined")))
            (console.log (insert-html-before div-by-id-1 div-by-id-2 p-by-id))
            (set! div-by-id-2 (get-element-by-id my-id-2))
            (console.log
             (lst->string
              '("--- insert div 1's p before div 3 in div 1\n"
                "    expect : false")))
            (console.log (insert-html-before div-by-id-1 p-by-id div-by-id-3))
            (console.log
             (lst->string
              '("--- insert div 3 in div 1 before js string\n"
                "    expect : false")))
            (console.log (insert-html-before div-by-id-1 div-by-id-3 my-id-js))
            (console.log
             (lst->string
              '("--- insert div 3 in div 1 before scm string\n"
                "    expect : false")))
            (console.log (insert-html-before div-by-id-1 div-by-id-3 my-id-1))
            (console.log
             (lst->string
              '("--- insert div 3 in div 1 before scm object\n"
                "    expect : false")))
            (console.log (insert-html-before div-by-id-1 div-by-id-3 my-new-div-2))
            (console.log
             (lst->string
              '("--- insert js string in div 1 before div 2\n"
                "    expect : undefined")))
            (console.log (insert-html-before div-by-id-1 my-id-js div-by-id-2))
            (console.log
             (lst->string
              '("--- insert scm string in div 1 before div 2\n"
                "    expect : undefined")))
            (console.log (insert-html-before div-by-id-1 my-id-1 div-by-id-2))
            (console.log
             (lst->string
              '("--- insert scm object in div 1 before div 2\n"
                "    expect : undefined")))
            (console.log (insert-html-before div-by-id-1 my-new-div-4 div-by-id-2))
            (console.log
             (lst->string
              '("--- insert div 3 in js string before div 2\n"
                "    expect : false")))
            (console.log (insert-html-before my-id-js div-by-id-3 div-by-id-2))
            (console.log
             (lst->string
              '("--- insert div 3 in scm string before div 2\n"
                "    expect : false")))
            (console.log (insert-html-before my-id-1 div-by-id-3 div-by-id-2))
            (console.log
             (lst->string
              '("--- insert div 3 in scm object before div 2\n"
                "    expect : false")))
            (console.log (insert-html-before my-new-div-1 div-by-id-3 div-by-id-2))
            (console.log
             (lst->string
              '("---------------------------------------------------\n"
                "-- 2 : after modif --------------------------------\n"
                "---------------------------------------------------\n"
                "--- div 1\n"
                "    expect : <div id='myDiv1'>myDiv1myDiv1<div></div><div id='myDiv2'></div><p id='myP'></p></div>")))
            (console.log (get-element-by-id my-id-1))
            (console.log
             (lst->string
              '("--- div 3\n"
                "    expect : <div id='myDiv3'></div>")))
            (console.log (get-element-by-id my-id-3))
            (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test-element-present-by-query?)
  (let ((my-id "myDiv"))
    (let ((my-newP (<p> "outer p"))
          (my-new-div
           (<div>
            id: my-id
            (<p> "inner p"))))
      (begin
        (console.log
         (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX isElementPresentByQuery XXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- write <p>outer p</p>\n"
            "          <div>\n"
            "              <p>inner p</p>\n"
            "          </div>")))
        (document.write my-new-p)
        (document.write my-new-div)
        (let ((div-by-id (get-element-by-id my-id))
              (valid-scm-selector "p")
              (invalid-scm-selector ".not a .valid .selector")
              (js-selector (scm->js "p")))
          (begin
            (console.log
             (lst->string
              '("--- with valid scm string\n"
                "    expect : true>")))
            (console.log (element-present-by-query? valid-scm-selector))
            (console.log
             (lst->string
              '("--- with valid scm string on frame\n"
                "    expect : true")))
            (console.log (element-present-by-query? valid-scm-selector div-by-id))
            (console.log
             (lst->string
              '("--- with invalid scm string\n"
                "    expect : false")))
            (console.log (element-present-by-query? invalid-scm-selector))
            (console.log
             (lst->string
              '("--- with invalid scm string on frame\n"
                "    expect : false")))
            (console.log (element-present-by-query? invalid-scm-selector div-by-id))
            (console.log
             (lst->string
              '("--- with scm object other than a string\n"
                "    expect : false")))
            (console.log (element-present-by-query? my-new-p))
            (console.log
             (lst->string
              '("--- with js string\n"
                "    expect : false")))
            (console.log (element-present-by-query? js-selector))
            (console.log
             (lst->string
              '("--- with valid scm string on scm objet\n"
                "    expect : false")))
            (console.log (element-present-by-query? valid-scm-selecto valid-scm-selector))
            (console.log
             (lst->string
              '("--- with valid scm string on js objet\n"
                "    expect : false")))
            (console.log (element-present-by-query? valid-scm-selector js-selector))
            (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test-replace-node)
  (let ((my-id-1 "myDiv1")
        (my-id-11 "myP")
        (my-id-2 "myDiv2")
        (my-id-3 "myDiv3"))
    (let ((my-new-div-1
           (<div>
            id: my-id-1
            (<p> id: my-id-11)))
          (my-new-div-2 (<div> id: my-id-2))
          (my-new-div-3 (get-new-node)))
      (begin
        (console.log
         (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX replaceNode XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- write <div id='myDiv1'><p id='myP'></p></div>\n"
            "          <div id='myDiv2'></div>")))
        (document.write my-new-div-1)
        (document.write my-new-div-2)
        (let ((div-by-id-1 (get-element-by-id my-id-1))
              (p-by-id (get-element-by-id my-id-11))
              (div-by-id-2 (get-element-by-id my-id-2))
              (my-id-js (scm->js my-id-1)))
          (begin
            (console.log
             (lst->string
              '("---------------------------------------------------\n"
                "-- 1 : using replaceNode --------------------------\n"
                "---------------------------------------------------\n"
                "--- replace div 1's p with div 2\n"
                "    expect : undefined")))
            (console.log (replace-node p-by-id div-by-id-2))
            (console.log
             (lst->string
              '("--- replace uninserted div 3 with div 2\n"
                "    expect : false")))
            (console.log (replace-node div-by-id-3 div-by-id-2))
            (console.log
             (lst->string
              '("--- replace js string with div 3\n"
                "    expect : false")))
            (console.log (replace-node my-id-js div-by-id-3))
            (console.log
             (lst->string
              '("--- replace scm string with div 3\n"
                "    expect : false")))
            (console.log (replace-node my-id-1 div-by-id-3))
            (console.log
             (lst->string
              '("--- replace scm object with div 3\n"
                "    expect : false")))
            (console.log (replace-node my-new-div-2 div-by-id-3))
            (console.log
             (lst->string
              '("--- replace div 2 with js string\n"
                "    expect : false")))
            (console.log (replace-node div-by-id-2 my-id-js))
            (console.log
             (lst->string
              '("--- replace div 2 with scm string\n"
                "    expect : false")))
            (console.log (replace-node div-by-id-2 my-id-1))
            (console.log
             (lst->string
              '("--- replace div 2 with scm object\n"
                "    expect : false")))
            (console.log (replace-node div-by-id-2 my-new-div-1))
            (console.log
             (lst->string
              '("---------------------------------------------------\n"
                "-- 2 : after modif --------------------------------\n"
                "---------------------------------------------------\n"
                "--- body's children\n"
                "    expect : <div id='myDiv1'><div id='myDiv2'></div></div>\n"
                "             <div id='myDiv2'></div>")))
            (console.log (query-selector-all "*" (query-selector "body")))
            (##inline-host-statement "document.body.innerHTML = '';")))))))

; -------------------------------------------------------------------------
; RUN TESTS FUNCTION
; -------------------------------------------------------------------------
(define (run-tests)
  (test-document-obj)
  (test-null-obj)
  (test-undefined-obj)
  (test-undefined?)
  (test-defined?)
  (test-usable-obj?)
  (test-class-of)
  (test-scm-obj?)
  (test-get-slots)
  (test-has-slots?)
  (test-html-obj?)
  (test-html-to-string)
  (test-js-to-scm)
  (test-scm-to-js)
  (test-type-of)
  (test-document-write)
  (test-console-log)
  (test-dom-obj?)
  (test-document-obj?)
  (test-get-element-by-id)
  (test-query-selector)
  (test-query-selector-all)
  (test-get-attribute)
  (test-has-attribute)
  (test-set-attribute)
  (test-remove-attribute)
  (test-get-element-type)
  (test-element-of-type?)
  (test-first-child)
  (test-parent-node)
  (test-get-new-node)
  (test-remove-node)
  (test-set-inner-html)
  (test-append-node)
  (test-append-html)
  (test-insert-node-before)
  (test-insert-html-before)
  (test-element-present-by-query?)
  (test-replace-node))
