(import (gambit))
(##include "userlib/github.com/roropincho/scm2js/@/html/html.scm")
(##include "userlib/github.com/roropincho/scm2js/@/js/js.scm")
;(##include "js-test.scm")

(declare
 (extended-bindings))

;(run-tests)

(define ex-id "examples")

(define dev-id "in-dev")

(define bullet-class "bullet")

(define gr-class "ex-gr")

(define plus-class "plus")

(define examples
  '(("." "scm2js")
    ("Game of Life" "game-of-life")))

(define dev
  '(("Card game" "card-game")
    ("Chat room" "chat-room")
    ("Form" "form")
    ("Login system" "login-system")))

(define (page-links page)
  (let ((part-of-url (cadr page)))
    (html->string
     (<li> (<span> class: bullet-class)
           (<a> target: "_blank"
                href: (string-append
                       "https://roropincho.github.io/"
                       part-of-url)
                (<span>)
                (car page))
           " : "
           (<a> target: "_blank"
                href: (string-append
                       "https://github.com/roropincho/"
                       part-of-url)
                (<span>)
                "git repo")))))

(define (insert-list-links id lst)
  (element.innerHTML (document.getElementById id) (append-strings (map page-links lst))))

(define (insert-links)
  (insert-list-links ex-id examples)
  (insert-list-links dev-id dev))

(define new-link
  (let ((temp (document.createElement "link")))
    (begin
      (console.log (foreign->js temp))
      (element.setAttribute temp "rel" "stylesheet")
      (element.setAttribute temp "href" "style.css")
      temp)))

(define new-title
  (let ((temp (document.createElement "title")))
    (begin
      (console.log (foreign->js temp))
      (element.innerHTML temp "Index of Scheme to Javascript projects")
      temp)))

(begin
  (document.write (html->string (<h1> "Scheme to Javascript examples")))

  (document.write
   (html->string
     (<div> class: gr-class
            (<h2> "Finished examples")
            (<input>
            type: 'checkbox
            checked:)
            (<div> class: plus-class
                   (<div>)
                   (<div>))
            (<div> (<ul> id: ex-id)))))

  (document.write
   (html->string
     (<div> class: gr-class
            (<h2> "Examples under construction")
            (<input> type: 'checkbox)
            (<div> class: plus-class
                   (<div>)
                   (<div>))
            (<div> (<ul> id: dev-id)))))

  (insert-links)

  (element.appendChild (.querySelector (document-obj) "head") new-title)

  (element.appendChild (.querySelector (document-obj) "head") new-link))

