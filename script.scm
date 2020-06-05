(##include "~/gambit/lib/gambit#.scm")
(##include "~/gambit/examples/web-server/html.scm")
(##include "js.scm")

(declare
 (extended-bindings))

(define ex-id "examples")

(define dev-id "in-dev")

(define gr-class "ex-gr")

(define plus-class "plus")

(define (insert-links)
  (define examples
    '(("." "scm2js")
      ("Game of Life" "game-of-life")))

  (define dev
    '(("Card game" "card-game")
      ("Chat room" "chat-room")
      ("Form" "form")
      ("Login system" "login-system")))

  (define (get-inserted-content lst base)
    (if (pair? lst)
        (let ((page (car lst)))
          (begin
            (console.log "ITER")
            (get-inserted-content
             (cdr lst)
             (append-strings
              `(,base
                "<li><span class='bullet'></span><a href='https://roropincho.github.io/"
                ,(cadr page)
                "' target='_blank'><span></span>"
                ,(car page)
                "</a> : <a href='https://github.com/roropincho/"
                ,(cadr page)
                "' target='_blank'><span></span>git repo</a></li>")))))
        base))

  (set-inner-html
   (get-element-by-id  ex-id)
   (get-inserted-content examples ""))
  (set-inner-html
   (get-element-by-id  dev-id)
   (get-inserted-content dev "")))

(##inline-host-statement "document.title = 'Index of Scheme to Javascript projects';")

(append-html
 (query-selector "head")
 (<style>
  type: "text/css"
  (append-strings
   `("* {"
       "border: 0;"
       "border-collapse: collapse;"
       "margin: 0;"
       "padding: 0;"
       "transition: 0.25s background-color, 0.25s color, 0.25s height, 0.25s opacity, 0.25s right, 0.25s transform;"
     "}"
     "body {"
       "color: darkslategrey;"
       "font-family: sans-serif;"
       "font-size: 15px;"
       "margin: 25px;"
     "}"
     "h1 {"
       "background-color: darkslategrey;"
       "color: white;"
       "font-size: 30px;"
       "margin: -25px;"
       "margin-bottom: 0;"
       "padding: 25px;"
     "}"
     "." ,gr-class " {"
       "position: relative;"
     "}"
     "input, ." ,plus-class " {"
       "position: absolute;"
       "right: 0;"
       "top: 10px;"
     "}"
     "input {"
       "height: 25px;"
       "opacity: 0;"
       "width: 25px;"
       "z-index: 3;"
     "}"
     "input:hover {"
       "cursor: pointer;"
     "}"
     "input:hover + ." ,plus-class " {"
       "background-color: darkslategrey;"
       "cursor: pointer;"
     "}"
     "input + div + div {"
       "height: 0;"
       "overflow: hidden;"
     "}"
     "input:checked + div + div {"
       "height: 100%;"
     "}"
     "." ,plus-class " {"
       "border: 2px solid white;"
       "border-radius: 50%;"
       "height: 21px;"
       "width: 21px;"
       "z-index: 2;"
     "}"
     "." ,plus-class " div {"
       "background-color: white;"
       "height: 2px;"
       "left: 50%;"
       "position: absolute;"
       "top: 50%;"
       "transform: translate(-50%, -50%);"
       "width: 11px;"
     "}"
     "input:not(:checked) + ." ,plus-class " div:nth-child(even) {"
       "transform: translate(-50%, -50%) rotate(90deg);"
     "}"
     "h2 {"
       "background-color: darksalmon;"
       "border-bottom: 2px solid white;"
       "color: white;"
       "font-size: 25px;"
       "font-weight: normal;"
       "letter-spacing: 1px;"
       "margin: 0 -25px;"
       "padding: 10px 25px;"
     "}"
     "ul {"
       "list-style-type: none;"
       "margin: 0 0 50px 0;"
     "}"
     "li {"
       "padding: 20px 0;"
     "}"
     "li:not(:first-of-type) {"
       "border-top: 1px solid darksalmon;"
     "}"
     ".bullet {"
       "border: 2px solid darksalmon;"
       "border-radius: 50%;"
       "display: inline-block;"
       "height: 5px;"
       "margin: 0 7.5px 0 5px;"
       "width: 5px;"
     "}"
     "a, a:visited {"
       "color: lightslategrey;"
       "text-decoration: none;"
     "}"
     "a {"
       "border: 1px solid lightslategrey;"
       "border-radius: 5px;"
       "padding: 5px 10px 6px 10px;"
       "position: relative;"
     "}"
     "a:hover {"
       "color: white;"
     "}"
     "a span {"
       "bottom: 0;"
       "color: lightslategrey;"
       "left: 0;"
       "position: absolute;"
       "right: 100%;"
       "top: 0;"
       "z-index: -1;"
     "}"
     "a:hover span {"
       "background-color: lightslategrey;"
       "right: 0;"
     "}"))))

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

(insert-links)
