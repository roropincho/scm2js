(##include "~/gambit/lib/gambit#.scm")
(##include "~/gambit/examples/web-server/html.scm")
(##include "js.scm")

(declare
 (extended-bindings))

(define insertLinks
  (lambda ()
    (define examples
      (list (list "." "scm2js")
	    (list "Game of Life" "game-of-life")))
    (define (getExampleContent lst base)
      (if (pair? lst)
	  (let ((page (car lst)))
	    (begin
	      (console.log "ITER")
	      (getExampleContent (cdr lst)
				 (string-append base
						"<li><span class='bullet'></span><a href='https://roropincho.github.io/"
						(cadr page)
						"' target='_blank'><span></span>"
						(car page)
						"</a> : <a href='https://github.com/roropincho/"
						(cadr page)
						"' target='_blank'><span></span>git repo</a></li>"))))
	  base))
    (setInnerHTML (querySelector "ul")
		  (getExampleContent examples ""))))

(##inline-host-statement "document.title = 'Index of Scheme to Javascript projects';")
(appendHTML (querySelector "head")
	    (<style> type: "text/css"
		     (string-append "* {"
				    "border: 0;"
				    "border-collapse: collapse;"
				    "margin: 0;"
				    "padding: 0;"
				    "transition: 0.25s background-color, 0.25s color, 0.25s opacity, 0.25s right;"
				    "}"
				    "body {"
				    "color: darkslategrey;"
				    "font-family: sans-serif;"
				    "font-size: 15px;"
				    "margin: 25px;"
				    "}"
				    "h1 {"
				    "margin-bottom: 15px;"
				    "}"
				    "ul {"
				    "border-bottom: 3px solid darkslategrey;"
				    "border-top: 3px solid darkslategrey;"
				    "list-style-type: none;"
				    "}"
				    "li {"
				    "padding: 20px 0;"
				    "}"
				    "li:not(:first-of-type) {"
				    "border-top: 1px solid palevioletred;"
				    "}"
				    ".bullet {"
				    "border: 2px solid palevioletred;"
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
				    "}")))
(document.write (<h1> "Scheme to Javascript examples"))
(document.write (<ul>))
(insertLinks)
