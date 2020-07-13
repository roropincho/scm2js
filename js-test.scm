(import (gambit))
(##include "html/html.scm")
(##include "js/js.scm")

(declare
 (extended-bindings))

(define (lst->string lst)
  (call-with-output-string
    (lambda (port)
      (map
	(lambda (elem) (display elem port))
	lst))))

(define (string-handler exc)
  "type error (there's still a bug with ##foreign-address when expecting a string)")

(define (message-handler exc)
  (error-exception-message exc))

(define (display-handler exc)
  (call-with-output-string
    (lambda (port)
      (display-exception exc port))))

(define (convert-and-log obj)
  (console.log
    (if (##foreign? obj)
        (foreign->js obj)
        (scm->js obj))))

(define (reset-body)
  (##inline-host-statement "document.body.innerHTML = '';"))

; -------------------------------------------------------------------------
; TESTS FUNCTIONS
; -------------------------------------------------------------------------
(define (test-lst-2-string)
  (let ((lst '("1" "2" "3"))
        (not-lst "123"))
    (begin
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX lst->string XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- list before converting to string\n"
            "    expect : [1, 2, 3] in scheme format")))
      (console.log lst)
      (convert-and-log
        (lst->string
          '("--- list after conversion\n"
            "    expect : '123' in scheme format\n"
            "    expect : '123' in js format")))
      (console.log (lst->string lst))
      (convert-and-log (lst->string lst))
      (convert-and-log
        (lst->string
          '("--- not a list before converting to string\n"
            "    expect : '123' in scheme format\n"
            "    expect : '123' in js format")))
      (console.log not-lst)
      (convert-and-log not-lst)
      (convert-and-log
        (lst->string
          '("--- not a list after conversion\n"
            "    expect : '' in scheme format")))
      (console.log (lst->string not-lst))
      (convert-and-log
        "    expect : '' in js format")
      (convert-and-log (lst->string not-lst)))))

(define (test-html-2-string)
  (let ((html-obj
          (<div> class: "my-class"
                 id: "my-id"))
        (not-html "not an html element"))
    (begin
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX html->string XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- html before converting to string\n"
            "    expect : <div class='my-class' id='my-id'></div> in scheme format")))
      (console.log html-obj)
      (convert-and-log
        (lst->string
          '("--- html after conversion\n"
            "    expect : <div class='my-class' id='my-id'></div> in scheme format\n"
            "    expect : <div class='my-class' id='my-id'></div> in js format")))
      (console.log (html->string html-obj))
      (convert-and-log (html->string html-obj))
      (convert-and-log
        (lst->string
          '("--- not html before converting to string\n"
            "    expect : 'not an html element' in scheme format\n"
            "    expect : 'not an html element' in js format")))
      (console.log not-html)
      (convert-and-log not-html)
      (convert-and-log
        (lst->string
          '("--- not html after conversion\n"
            "    expect : error")))
      (convert-and-log
        (with-exception-catcher
          message-handler
          (lambda () (html->string not-html)))))))
; -------------------------------------------------------------------------
(define (test-foreign-2-js)
  (let ((my-foreign (js->foreign (scm->js "foreign"))))
    (begin
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX foreign->js XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- without any operation on the foreign object")))
      (console.log my-foreign)
      (convert-and-log
        (lst->string
          '("--- js content inside\n"
            "    expect : 'foreign' in js format")))
      (console.log (foreign->js my-foreign)))))

(define (test-js-2-foreign)
  (let ((my-js (scm->js "js")))
    (begin
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX js->foreign XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- without any operation on the js object\n"
            "    expect : 'js' in js format")))
      (console.log my-js)
      (convert-and-log
        (lst->string
          '("--- js object wrapped in foreign object")))
      (console.log (js->foreign my-js)))))

(define (test-scm-2-js)
  (let ((my-scm "scm")
        (my-js (scm->js "js")))
    (begin
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX scm->js XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- scm string before conversion\n"
            "    expect : 'scm' in scheme format")))
      (console.log my-scm)
      (convert-and-log
        (lst->string
          '("--- after conversion\n"
            "    expected : 'scm' in js format")))
      (console.log (scm->js my-scm))
      (convert-and-log
        (lst->string
          '("--- js string before conversion\n"
            "    expect : 'js' in js format")))
      (console.log my-js)
      (convert-and-log
        (lst->string
          '("--- after conversion\n"
            "    expect : null")))
      (convert-and-log
        (with-exception-catcher
          message-handler
          (lambda () (scm->js my-js)))))))

(define (test-js-2-scm)
  (let ((my-js (scm->js "js")))
    (begin
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX js->scm XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- js string before conversion\n"
            "    expect : 'js' in js format")))
      (console.log my-js)
      (convert-and-log
        (lst->string
          '("--- after conversion\n"
            "    expect : 'js' in scm format")))
      (console.log (js->scm my-js)))))
; -------------------------------------------------------------------------
(define (test-document-obj)
  (let ((my-doc (document-obj)))
    (begin
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX document-obj XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- result inside the foreign object returned")))
      (console.log (foreign->js my-doc)))))

(define (test-null-obj)
  (let ((my-null (null-obj)))
    (begin
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX null-obj XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- result inside the foreign object returned")))
      (console.log (foreign->js my-null)))))

(define (test-unedfined-obj)
  (let ((my-undef (undefined-obj)))
    (begin
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX undefined-obj XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- result inside the foreign object returned")))
      (console.log (foreign->js my-undef)))))

(define (test-instanceof)
  (let ((my-str "scm")
        (my-doc (foreign->js (document-obj)))
        (my-err (##inline-host-expression "Error('err')")))
    (begin
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX instanceof XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- scheme string instanceof Node\n"
            "    expect : false")))
      (convert-and-log (instanceof my-str "Node"))
      (convert-and-log
        (lst->string
          '("--- document instanceof Node\n"
            "    expect : true")))
      (convert-and-log (instanceof my-doc "Node"))
      (convert-and-log
        (lst->string
          '("--- error object instanceof Error\n"
            "    expect : true")))
      (convert-and-log (js-error? my-err))
      (convert-and-log
        (lst->string
          '("--- scheme string instanceof not a callabale name\n"
            "    expect : error")))
      (##inline-host-statement "str = 'str'")
      (convert-and-log
        (with-exception-catcher
          display-handler
          (lambda () (instanceof scm-str "str")))))))
; -------------------------------------------------------------------------
(define (test-document.write)
  (let ((my-content "Hello world!"))
    (begin
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX document.write XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- with scheme string 'Hello world!'\n"
            "    expect : document with 'Hello world!'")))
      (document.write my-content)
      (convert-and-log (document-obj))
      (reset-body))))
; -------------------------------------------------------------------------
(define (test-document.getElementById)
  (let ((my-id "myId")
        (not-id "notId"))
    (let ((my-new-div (<div> id: my-id)))
      (begin
        (document.write (html->string my-new-div))
        (convert-and-log
          (lst->string
            '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
              "XX document.getElementById XXXXXXXXXXXXXXXXXXXXXXXX\n"
              "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
              "--- with valid id\n"
              "    expect : <div id='myId'></div>")))
        (convert-and-log (document.getElementById my-id))
        (convert-and-log
          (lst->string
            '("--- with invalid id\n"
              "    expect : null")))
        (convert-and-log (document.getElementById not-id))
        (reset-body)))))

(define (test-.querySelector)
  (let ((my-new-div (<div>))
        (my-foreign (js->foreign (scm->js "foreign"))))
    (begin
      (document.write (html->string my-new-div))
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX .querySelector XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- with valid selector on document element\n"
            "    expect : <div></div>")))
      (convert-and-log (.querySelector (document-obj) "div"))
      (convert-and-log
        (lst->string
          '("--- with invalid selector on document element\n"
            "    expect : null")))
      (convert-and-log (.querySelector (document-obj) ".this #should:not(.work)"))
      (convert-and-log
        (lst->string
          '("--- on foreign containing something other than a dom object\n"
            "    expect : error")))
      (convert-and-log
        (with-exception-catcher
          display-handler
          (lambda () (.querySelector my-foreign "*"))))
      (reset-body))))

(define (test-.querySelectorAll)
  (let ((my-id "myId")
        (my-foreign (js->foreign (scm->js "foreign"))))
    (let ((my-new-div
            (<div> id: my-id
                   (<p>)))
          (my-new-p (<p>)))
      (begin
        (document.write (html->string my-new-div))
        (document.write (html->string my-new-p))
        (let ((div-by-id (document.getElementById my-id))
              (my-doc (document-obj)))
          (convert-and-log
            (lst->string
              '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
                "XX .querySelectorAll XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
                "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
                "--- all <p> in the document\n"
                "    expect : list with 2 <p>")))
          (convert-and-log (.querySelectorAll my-doc "p"))
          (convert-and-log
            (lst->string
              '("--- all <p> in the div\n"
                "    expect : list with 1 <p>")))
          (convert-and-log (.querySelectorAll div-by-id "p"))
          (convert-and-log
            (lst->string
              '("--- invalid selector on document\n"
                "    expect : empty list")))
          (convert-and-log (.querySelectorAll my-doc ".this #should:not(.work)"))
          (convert-and-log
            (lst->string
              '("--- on foreign containing something other than a dom object\n"
                "    expect : error")))
          (convert-and-log
            (with-exception-catcher
              display-handler
              (lambda () (.querySelectorAll my-foreign "*"))))
          (reset-body))))))
; -------------------------------------------------------------------------
(define (test-element.firstChild)
  (let ((my-str "str")
        (my-id "myId")
        (my-inner-div (get-new-div-node))
        (my-foreign (js->foreign (scm->js "foreign"))))
    (let ((my-new-div
            (<div> id: my-id
                   my-str)))
      (begin
        (document.write (html->string my-new-div))
        (let ((div-by-id (document.getElementById my-id)))
          (begin
            (element.appendChild div-by-id my-inner-div)
            (convert-and-log
              (lst->string
                '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
                  "XX element.firstChild XXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
                  "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
                  "--- on inserted div\n"
                  "    expect : 'str' text node")))
            (convert-and-log (element.firstChild div-by-id))
            (convert-and-log
              (lst->string
                '("--- on inner div\n"
                  "    expect : null")))
            (convert-and-log (element.firstChild (.querySelector div-by-id "div")))
            (convert-and-log
              (lst->string
                '("--- on foreign containing something other than a dom object\n"
                  "    expect : unedfined")))
            (convert-and-log (element.firstChild my-foreign))
            (reset-body)))))))

(define (test-element.firstElementChild)
  (let ((my-str "str")
        (my-id "myId")
        (my-inner-div (get-new-div-node))
        (my-foreign (js->foreign (scm->js "foreign"))))
    (let ((my-new-div
            (<div> id: my-id
                   my-str)))
      (begin
        (document.write (html->string my-new-div))
        (let ((div-by-id (document.getElementById my-id)))
          (begin
            (element.appendChild div-by-id my-inner-div)
            (convert-and-log
              (lst->string
                '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
                  "XX element.firstElementChild XXXXXXXXXXXXXXXXXXXXXX\n"
                  "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
                  "--- on inserted div\n"
                  "    expect : <div></div>")))
            (convert-and-log (element.firstElementChild div-by-id))
            (convert-and-log
              (lst->string
                '("--- on inner div\n"
                  "    expect : null")))
            (convert-and-log (element.firstElementChild (.querySelector div-by-id "div")))
            (convert-and-log
              (lst->string
                '("--- on foreign containint something other than a dom object\n"
                  "    expect : undefined")))
            (convert-and-log (element.firstElementChild my-foreign))
            (reset-body)))))))

(define (test-element.parentNode)
  (let ((my-inner-div (get-new-div-node))
        (my-new-div (get-new-div-node))
        (my-foreign (js->foreign (scm->js "foreign"))))
    (begin
      (element.appendChild my-new-div my-inner-div)
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX element.parentNode XXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- on inner div\n"
            "    expect : <div><div></div></div>")))
      (convert-and-log (element.parentNode my-inner-div))
      (convert-and-log
        (lst->string
          '("--- on outer div\n"
            "    expect : null")))
      (convert-and-log (element.parentNode my-new-div))
      (convert-and-log
        (lst->string
          '("--- on foreign containing something other than a dom object\n"
            "    expect : undefined")))
      (convert-and-log (element.parentNode my-foreign)))))

(define (test-document.createElement)
  (begin
    (convert-and-log
      (lst->string
        '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
          "XX document.createElement XXXXXXXXXXXXXXXXXXXXXXXXX\n"
          "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
          "--- create a valid tag\n"
          "    expect : <p></p>")))
    (convert-and-log (document.createElement "p"))
    (convert-and-log
      (lst->string
        '("--- create an invalid tag\n"
          "    expect : <invalid></invalid>")))
    (convert-and-log (document.createElement "invalid"))))
; -------------------------------------------------------------------------
(define (test-element.getAttribute)
  (let ((my-new-div (get-new-div-node))
        (my-foreign (js->foreign (scm->js "foreign"))))
    (begin
      (element.setAttribute my-new-div "id" "myId")
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX element.getAttribute XXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- get 'id' on created div\n"
            "    expect : 'myId'")))
      (convert-and-log (element.getAttribute my-new-div "id"))
      (convert-and-log
        (lst->string
          '("--- get 'invalid' on create div\n"
            "    expect : null")))
      (convert-and-log (element.getAttribute my-new-div "invalid"))
      (convert-and-log
        (lst->string
          '("--- on foreign containing something other than a dom object\n"
            "    expect : error")))
      (convert-and-log
        (with-exception-catcher
          display-handler
          (lambda () (element.getAttribute my-foreign "id")))))))

(define (test-element.hasAttribute)
  (let ((my-new-div (get-new-div-node))
        (my-foreign (js->foreign (scm->js "foreign"))))
    (begin
      (element.setAttribute my-new-div "id" "myId")
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX element.hasAttribute XXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- created div has 'id'\n"
            "    expect : true")))
      (convert-and-log (element.hasAttribute my-new-div "id"))
      (convert-and-log
        (lst->string
          '("--- created div has 'invalid'\n"
            "    expect : false")))
      (convert-and-log (element.hasAttribute my-new-div "invalid"))
      (convert-and-log
        (lst->string
          '("--- on foreign containing something other than a dom object\n"
            "    expect : error")))
      (convert-and-log
        (with-exception-catcher
          display-handler
          (lambda () (element.hasAttribute my-foreign "id")))))))

(define (test-element.setAttribute)
  (let ((my-new-div (get-new-div-node))
        (my-foreign (js->foreign (scm->js "foreign"))))
    (begin
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX element.setAttribute XXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- 'id' before setting value\n"
            "    expect : undefined")))
      (convert-and-log (element.setAttribute my-new-div "id" "myId"))
      (convert-and-log
        (lst->string
          '("--- 'id' after setting value\n"
            "    expect : 'myId'")))
      (convert-and-log (element.getAttribute my-new-div "id"))
      (convert-and-log
        (lst->string
          '("--- on foreign containing something other than a dom object\n"
            "    expect : error")))
      (convert-and-log
        (with-exception-catcher
          display-handler
          (lambda () (element.getAttribute my-foreign "*")))))))

(define (test-element.removeAttribute)
  (let ((my-new-div (get-new-div-node))
        (my-foreign (js->foreign (scm->js "foreign"))))
    (begin
      (element.setAttribute my-new-div "id" "myId")
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX element.removeAttribute XXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- 'id' before removing\n"
            "    expect : 'myId'")))
      (convert-and-log (element.getAttribute my-new-div "id"))
      (element.removeAttribute my-new-div "id")
      (convert-and-log
        (lst->string
          '("--- 'id' after removing\n"
            "    expect : null")))
      (convert-and-log (element.getAttribute my-new-div "id"))
      (convert-and-log
        (lst->string
          '("--- on foreign containing something other than a dom object\n"
            "    expect : error")))
      (convert-and-log
        (with-exception-catcher
          display-handler
          (lambda () (element.removeAttribute my-foreign "*")))))))
; -------------------------------------------------------------------------
(define (test-element.innerHTML-get)
  (let ((my-new-div (get-new-div-node))
        (my-str "str")
        (my-foreign (js->foreign (scm->js "foreign"))))
    (begin
      (element.innerHTML my-new-div my-str)
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX element.innerHTML - get XXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- innerHTML in created div\n"
            "    expect : 'str'")))
      (convert-and-log (element.innerHTML my-new-div))
      (convert-and-log
        (lst->string
          '("--- on foreign containing something other than a dom object\n"
            "    expect : undefined")))
      (convert-and-log (element.innerHTML my-foreign)))))

(define (test-element.innerHTML-set)
  (let ((my-new-div (get-new-div-node))
        (my-str "str")
        (my-foreign (js->foreign (scm->js "foreign"))))
    (begin
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX element.innerHTML - set XXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- innerHTML in created div before set\n"
            "    expect : null")))
      (convert-and-log (element.innerHTML my-new-div))
      (element.innerHTML my-new-div my-str)
      (convert-and-log
        (lst->string
          '("--- innerHTML in created div after set\n"
            "    expect : 'str'")))
      (convert-and-log (element.innerHTML my-new-div))
      (element.innerHTML my-foreign my-str)
      (convert-and-log
        (lst->string
          '("--- on foreign containing something other than a dom object\n"
            "    expect : undefined")))
      (convert-and-log (element.innerHTML my-foreign)))))

(define (test-element.outerHTML-get)
  (let ((my-new-div (get-new-div-node))
        (my-foreign (js->foreign (scm->js "foreign"))))
    (begin
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX element.outerHTML - get XXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- outerHTML in created div\n"
            "    expect : <div></div>")))
      (convert-and-log (element.outerHTML my-new-div))
      (convert-and-log
        (lst->string
          '("--- on foreign containing something other than a dom object\n"
            "    expect : undefined")))
      (convert-and-log (element.outerHTML my-foreign)))))

(define (test-element.outerHTML-set)
  (let ((my-new-div (get-new-div-node))
        (my-inner-div (get-new-div-node))
        (my-str "str")
        (my-foreign (js->foreign (scm->js "foreign"))))
    (begin
      (element.appendChild my-new-div my-inner-div)
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX element.outerHTML - set XXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- innerHTML in created div before set\n"
            "    expect : <div></div>")))
      (convert-and-log (element.innerHTML my-new-div))
      (element.outerHTML my-inner-div my-str)
      (convert-and-log
        (lst->string
          '("--- innerHTML in created div after set\n"
            "    expect : 'str'")))
      (convert-and-log (element.innerHTML my-new-div))
      (convert-and-log
        (lst->string
          '("-- set outerHTML on dom object without a parentNode\n"
            "   expect : error")))
      (convert-and-log
        (with-exception-catcher
          display-handler
          (lambda () (element.outerHTML my-new-div my-str))))
      (element.outerHTML my-foreign my-str)
      (convert-and-log
        (lst->string
          '("--- on foreign containing something other than a dom object\n"
            "    expect : undefined")))
      (convert-and-log (element.outerHTML my-foreign)))))
; -------------------------------------------------------------------------
(define (test-element.removeChild)
  (let ((my-new-div (get-new-div-node))
        (my-inner-div (get-new-div-node))
        (my-foreign (js->foreign (scm->js "foreign"))))
    (begin
      (element.appendChild my-new-div my-inner-div)
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX element.removeChild XXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- created div before removal\n"
            "    expect : <div><div></div></div>")))
      (convert-and-log (element.outerHTML my-new-div))
      (element.removeChild my-new-div my-inner-div)
      (convert-and-log
        (lst->string
          '("--- created div after removal\n"
            "    expect : <div></div>")))
      (convert-and-log (element.outerHTML my-new-div))
      (convert-and-log
        (lst->string
          '("--- on foreign containing something other than a dom object\n"
            "    expect : error")))
      (convert-and-log
        (with-exception-catcher
          display-handler
          (lambda () (element.removeChild my-foreign my-inner-div))))
      (convert-and-log
        (lst->string
          '("--- removing foreign containing something other than a dom object\n"
            "    expect : error")))
      (convert-and-log
        (with-exception-catcher
          display-handler
          (lambda () (element.removeChild my-new-div my-foreign)))))))

(define (test-element.appendChild)
  (let ((my-new-div (get-new-div-node))
        (my-inner-div (get-new-div-node))
        (my-foreign (js->foreign (scm->js "foreign"))))
    (begin
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX element.appendChild XXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- created div before append\n"
            "    expect : <div></div>")))
      (convert-and-log (element.outerHTML my-new-div))
      (element.appendChild my-new-div my-inner-div)
      (convert-and-log
        (lst->string
          '("--- created div after append\n"
            "    expect : <div><div></div></div>")))
      (convert-and-log (element.outerHTML my-new-div))
      (convert-and-log
        (lst->string
          '("--- on foreign containing something other than a dom object\n"
            "    expect : error")))
      (convert-and-log
        (with-exception-catcher
          display-handler
          (lambda () (element.appendChild my-foreign my-inner-div))))
      (convert-and-log
        (lst->string
          '("--- appending foreign containing something other than a dom object\n"
            "    expect : error")))
      (convert-and-log
        (with-exception-catcher
          display-handler
          (lambda () (element.appendChild my-new-div my-foreign)))))))

(define (test-element.insertBefore)
  (let ((my-new-div (get-new-div-node))
        (my-span (document.createElement "span"))
        (my-p (document.createElement "p"))
        (my-foreign (js->foreign (scm->js "foreign"))))
    (begin
      (element.appendChild my-new-div my-p)
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX element.insertBefore XXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- created div before insert\n"
            "    expect : <div><p></p></div>")))
      (convert-and-log (element.outerHTML my-new-div))
      (element.insertBefore my-new-div my-span my-p)
      (convert-and-log
        (lst->string
          '("--- created div after insert\n"
            "    expect : <div><span></span><p></p></div>")))
      (convert-and-log (element.outerHTML my-new-div))
      (convert-and-log
        (lst->string
          '("--- on foreign containing something other than a dom object\n"
            "    expect : error")))
      (convert-and-log
        (with-exception-catcher
          display-handler
          (lambda () (element.insertBefore my-foreign my-p my-span))))
      (convert-and-log
        (lst->string
          '("--- insert foreign containing something other than a dom object\n"
            "    expect : error")))
      (convert-and-log
        (with-exception-catcher
          display-handler
          (lambda () (element.insertBefore my-new-div my-foreign my-span))))
      (convert-and-log
        (lst->string
          '("--- insert before foreign containing something other than a dom object\n"
            "    expect : error")))
      (convert-and-log
        (with-exception-catcher
          display-handler
          (lambda () (element.insertBefore my-new-div my-p my-foreign)))))))

(define (test-element.replaceChild)
  (let ((my-new-div (get-new-div-node))
        (my-p-1 (document.createElement "p"))
        (my-p-2 (document.createElement "p"))
        (my-span (document.createElement "span"))
        (my-foreign (js->foreign (scm->js "foreign"))))
    (begin
      (element.appendChild my-new-div my-p-1)
      (element.appendChild my-new-div my-p-2)
      (convert-and-log
        (lst->string
          '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XX element.replaceChild XXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
            "--- created div before replace\n"
            "    expect : <div><p></p><p></p></div>")))
      (convert-and-log (element.outerHTML my-new-div))
      (element.replaceChild my-new-div my-span my-p-1)
      (convert-and-log
        (lst->string
          '("--- created div after replace\n"
            "    expect : <div><span></span><p></p></div>")))
      (convert-and-log (element.outerHTML my-new-div))
      (convert-and-log
        (lst->string
          '("--- on foreign containing something other than a dom object\n"
            "    expect : error")))
      (convert-and-log
        (with-exception-catcher
          display-handler
          (lambda () (element.replaceChild my-foreign my-span my-p-2))))
      (convert-and-log
        (lst->string
          '("--- replace with foreign containing something other than a dom object\n"
            "    expect : error")))
      (convert-and-log
        (with-exception-catcher
          display-handler
          (lambda () (element.replaceChild my-new-div my-foreign my-p-2))))
      (convert-and-log
        (lst->string
          '("--- replace foreign containing something other than a dom object\n"
            "    expect : error")))
      (convert-and-log
        (with-exception-catcher
          display-handler
          (lambda () (element.replaceChild my-new-div my-span my-foreign)))))))

; -------------------------------------------------------------------------
; THE RUN TESTS FUNCTION
; -------------------------------------------------------------------------
(define (run-tests)
  (test-lst-2-string)
  (test-html-2-string)
  (test-foreign-2-js)
  (test-js-2-foreign)
  (test-scm-2-js)
  (test-js-2-scm)
  (test-document-obj)
  (test-null-obj)
  (test-unedfined-obj)
  (test-instanceof)
  (test-document.write)
  (test-document.getElementById)
  (test-.querySelector)
  (test-.querySelectorAll)
  (test-element.firstChild)
  (test-element.firstElementChild)
  (test-element.parentNode)
  (test-document.createElement)
  (test-element.getAttribute)
  (test-element.hasAttribute)
  (test-element.setAttribute)
  (test-element.removeAttribute)
  (test-element.innerHTML-get)
  (test-element.innerHTML-set)
  (test-element.outerHTML-get)
  (test-element.outerHTML-set)
  (test-element.removeChild)
  (test-element.appendChild)
  (test-element.insertBefore)
  (test-element.replaceChild))

