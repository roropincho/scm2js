(##include "~/gambit/lib/gambit#.scm")
(##include "~/gambit/examples/web-server/html.scm")
(##include "js.scm")

(declare
 (extended-bindings))

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
  (set-inner-html
   (get-element-by-id id)
   (append-strings
    (map page-links lst))))

(define (insert-links)
  (insert-list-links ex-id examples)
  (insert-list-links dev-id dev))

(begin
  (##inline-host-statement "document.title = 'Index of Scheme to Javascript projects';")

  (append-html
   (query-selector "head")
   (<link>
    rel: "stylesheet"
    href: "style.css"))

  (document.write (<h1> "Scheme to Javascript examples"))

  (document.write
   (<div> class: gr-class
          (<h2> "Finished examples")
          (<input>
           type: 'checkbox
           checked:)
          (<div> class: plus-class
                 (<div>)
                 (<div>))
          (<div> (<ul> id: ex-id))))

  (document.write
   (<div> class: gr-class
          (<h2> "Examples under construction")
          (<input> type: 'checkbox)
          (<div> class: plus-class
                 (<div>)
                 (<div>))
          (<div> (<ul> id: dev-id))))

  (insert-links))
