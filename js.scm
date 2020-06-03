(##include "~/gambit/lib/gambit#.scm")
(##include "~/gambit/examples/web-server/html.scm")

(declare
(extended-bindings))

; -------------------------------------------------------------------------
; FUNCTIONS
; -------------------------------------------------------------------------
(define (documentObj)
  (##inline-host-expression "document;"))

(define (nullObj)
  (##inline-host-expression "null;"))

(define (undefinedObj)
  (##inline-host-expression "undefined;"))

(define (isUndefined obj)
  (##inline-host-expression "g_host2scm(@1@ === @2@);"
			    obj
			    (undefinedObj)))

(define (isDefined obj)
  (not (isUndefined obj)))

(define (isUsableObject obj)
  (and (not (null? obj))
       (isDefined obj)))

(define (classOf obj)
  (if (isUsableObject obj)
      (##inline-host-expression "g_host2scm((@1@).constructor.name);"
				obj)
      "hasNoClass"))

(define (isScmObject obj)
  (string=? "G_"
	    (substring (classOf obj)
		       0
		       2)))

(define (getSlots obj)
  (if (isUsableObject obj)
      (##inline-host-expression "(@1@).slots;" obj)
      (undefinedObj)))

(define (hasSlots obj)
  (isDefined (getSlots obj)))

(define (isHtmlObject obj)
  (if (and (isScmObject obj) (hasSlots obj))
      (let ((firstSlot (##inline-host-expression "(@1@)[0];" (getSlots obj))))
	(if (hasSlots firstSlot)
	    (let ((secLevelSlots (getSlots firstSlot)))
	      (if (>= (##inline-host-expression "g_host2scm((@1@).length);"
						secLevelSlots)
		      3)
		  (let ((objSymbol (##inline-host-expression "(@1@)[2];"
							     secLevelSlots)))
		    (let ((objSymbolName (##inline-host-expression "(@1@).name;"
								   objSymbol)))
		      (if (isDefined objSymbolName)
			  (string=? "html"
				    (##inline-host-expression "g_host2scm(@1@);"
							      objSymbolName))
			  #f)))
		  #f))
	    #f))
      #f))

(define (html->string html)
  (if (isHtmlObject html)
      (call-with-output-string (lambda (port)
				 (write-html html
					     port)))
      #f))

(define (jsObj->scmObj obj)
  (if (isScmObject obj)
      obj
      (##inline-host-expression "g_host2scm(@1@);"
				obj)))

(define (scmObj->jsObj obj)
  (if (isScmObject obj)
      (##inline-host-expression "g_scm2host(@1@);"
				obj)
      obj))

(define (typeOf obj)
  (jsObj->scmObj (##inline-host-expression "typeof (@1@);"
					   obj)))

(define (document.write html)
  (if (isUsableObject html)
      (##inline-host-statement "(@1@).write(@2@);"
			       (documentObj)
			       (if (isScmObject html)
				   (scmObj->jsObj (if (isHtmlObject html)
						      (html->string html)
						      html))
				   html))
      #f))

(define (console.log obj)
  (##inline-host-statement "console.log(@1@);"
			   (if (isScmObject obj)
			       (scmObj->jsObj (if (isHtmlObject obj)
						  (html->string obj)
						  obj))
			       obj)))

(define (isDOMObj obj)
  (if (not (isScmObject obj))
      (jsObj->scmObj (##inline-host-expression "(@1@) instanceof Node;"
					       obj))
      #f))

(define (isDocumentObj obj)
  (##inline-host-expression "(@1@) === document;"
			    obj))

; getElementById / querySelector / querySelectorAll

(define (getElementById id)
  (if (and (isScmObject id)
	   (string? id))
      (##inline-host-expression "(@1@).getElementById(@2@);"
				(documentObj)
				(scmObj->jsObj id))
      (nullObj)))

(define (querySelector selector
		       #!optional
		       (frame (documentObj)))
  (if (and (isScmObject selector)
	   (string? selector)
	   (or (isDOMObj frame)
	       (isDocumentObj frame)))
      (##inline-host-expression "(@1@).querySelector(@2@);"
				frame
				(scmObj->jsObj selector))
      (nullObj)))

(define (querySelectorAll selector
			  #!optional
			  (frame (documentObj)))
  (if (and (isScmObject selector)
	   (string? selector)
	   (or (isDOMObj frame)
	       (isDocumentObj frame)))
      (##inline-host-expression "(@1@).querySelectorAll(@2@);"
				frame
				(scmObj->jsObj selector))
      (nullObj)))

; add / get / modify / remove / test attribute

(define (getAttribute obj att)
  (if (and (isDOMObj obj)
	   (not (isDocumentObj obj))
	   (string? att))
      (jsObj->scmObj (##inline-host-expression "(@1@).getAttribute(@2@);"
					       obj
					       (scmObj->jsObj att)))
      #f))

(define (hasAttribute obj att)
  (if (string? att)
      (let ((attVal (getAttribute obj
				  att)))
	(and attVal
	     (not (null? attVal))))
      #f))

(define (setAttribute obj att val)
  (if (and (isDOMObj obj)
	   (not (isDocumentObj obj))
	   (string? att)
	   (isScmObject val))
      (##inline-host-statement "(@1@).setAttribute(@2@, @3@);"
			       obj
			       (scmObj->jsObj att)
			       (scmObj->jsObj val))
      #f))

(define (removeAttribute obj att)
  (if (and (isDOMObj obj)
	   (not (isDocumentObj obj))
	   (string? att))
      (##inline-host-statement "(@1@).removeAttribute(@2@);"
			       obj
			       (scmObj->jsObj att))
      #f))

; insert (before / after) / modify / remove / test (presence / type) html element

(define (getElementType node)
  (if (not (isScmObject node))
      (jsObj->scmObj (##inline-host-expression "(@1@).nodeName;"
					       node))
      #f))

(define (isElementOfType node type)
  (if (and (not (isScmObject node))
	   (string? type))
      (let ((nodeType (getElementType node)))
	(and (isDefined nodeType)
	     (string=? type
		       (getElementType node))))
      #f))

(define (firstChild obj)
  (if (isDOMObj obj)
      (let ((firstGiven (##inline-host-expression "(@1@).firstChild;"
						  obj)))
	(if (or (null? firstGiven)
		(isDOMObj firstGiven)
		(and (isElementOfType firstGiven "#text")
		     (or (> (jsObj->scmObj (##inline-host-expression "(@1@).length;"
								     firstGiven))
			    1)
			 (not (string=? "\n"
					(jsObj->scmObj (##inline-host-expression "(@1@).wholeText;"
										 firstGiven)))))))
	    firstGiven
	    (querySelector "*" obj)))
      #f))

(define (parentNode obj)
  (if (isDOMObj obj)
      (##inline-host-expression "(@1@).parentNode;"
				obj)
      #f))

(define (getNewNode)
  (##inline-host-expression "(@1@).createElement('DIV');"
			    (documentObj)))

(define (removeNode node)
  (let ((parentTemp (parentNode node)))
    (if (and (isDOMObj node)
	     (isUsableObject parentTemp))
	(##inline-host-statement "(@1@).removeChild(@2@);"
				 parentTemp
				 node)
	#f)))

(define (setInnerHTML obj htmlObj)
  (if (isDOMObj obj)
      (##inline-host-statement "(@1@).innerHTML = @2@;"
			       obj
			       (if (isScmObject htmlObj)
				   (scmObj->jsObj (if (isHtmlObject htmlObj)
						      (html->string htmlObj)
						      htmlObj))
				   htmlObj))
      #f))

(define (appendNode parent child)
  (if (and (isDOMObj parent)
	   (isDOMObj child))
      (##inline-host-statement "(@1@).appendChild(@2@);"
			       parent
			       child)
      #f))

(define (appendHTML parent childHTML)
  (let ((parentTemp (getNewNode))
	(childIsDOM (isDOMObj childHTML)))
    (if (setInnerHTML parentTemp
		      (if childIsDOM
			  (##inline-host-expression "(@1@).outerHTML;"
						    childHTML)
			  childHTML))
	(let ((nodeTemp (firstChild parentTemp)))
	  (if nodeTemp
	      (let ((appendReturned (appendNode parent
						nodeTemp)))
		(begin
		  (if (isUndefined appendReturned)
		      (removeNode childHTML))
		  appendReturned))
	      #f))
	#f)))

(define (insertNodeBefore parent child refChild)
  (if (and (isDOMObj parent)
	   (isDOMObj child)
	   (isDOMObj refChild)
	   (eq? parent
		(parentNode refChild)))
      (##inline-host-statement "(@1@).insertBefore(@2@, @3@);"
			       parent
			       child
			       refChild)
      #f))

(define (insertHTMLBefore parent childHTML refChild)
  (let ((parentTemp (getNewNode))
	(childIsDOM (isDOMObj childHTML)))
    (if (setInnerHTML parentTemp
		      (if childIsDOM
			  (##inline-host-expression "(@1@).outerHTML;"
						    childHTML)
			  childHTML))
	(let ((nodeTemp (firstChild parentTemp)))
	  (if nodeTemp
	      (let ((insertReturned (insertNodeBefore parent
						      nodeTemp
						      refChild)))
		(begin
		  (if (isUndefined insertReturned)
		      (removeNode childHTML))
		  insertReturned))
	      #f))
	#f)))

(define (isElementPresentByQuery selector
				 #!optional
				 (frame (documentObj)))
  (not (null? (querySelector selector
			     frame))))

(define (replaceNode old new)
  (let ((parentTemp (parentNode old)))
    (if (and (isDOMObj old)
	     (isUsableObject parentTemp)
	     (isDOMObj new))
	(##inline-host-statement "(@1@).replaceChild(@2@, @3@);"
				 parentTemp
				 new
				 old)
	#f)))

(define (replaceHTML old newHTML)
  (let ((parentTemp (getNewNode))
	(newIsDOM (isDOMObj newHTML)))
    (if (setInnerHTML parentTemp
		      (if newIsDOM
			  (##inline-host-expression "(@1@).outerHTML;"
						    newHTML)
			  newHTML))
	(let ((nodeTemp (firstChild parentTemp)))
	  (if nodeTemp
	      (let ((replaceReturned (replaceNode old
						  nodeTemp)))
		(begin
		  (if (isUndefined replaceReturned)
		      (removeNode newHTML))
		  replaceReturned))
	      #f))
	#f)))

; -------------------------------------------------------------------------
; TESTS
; -------------------------------------------------------------------------
(define page-main
  (<div> (<div> id: "wrapper"
		class: "'wrapper'"
		(<h1> "HELLO WORLD!")
		(<h2> style: (string-append "'"
					    "color: red;"
					    "font-family: sans-serif;"
					    "font-size: 50px;"
					    "font-weight: normal;"
					    "text-transform: uppercase;"
					    "'")
		      "Gros h2")
		(<ul> (<li> class: "'uneClasse deuxClasse'"
			    id: "'firstLI'"
			    onclick: "'javascript: void(0);'"
			    "First")
		      (<li> "Second")
		      (<li> "..."))
		(<p> "Un petit texte... tralala...")
		(<input> type: 'text
			 id: "'myInput'"
			 name: "monInput"
			 class: "'c1 c2 c3'"
			 value: "'une valeur'")
		(<p> "Un autre omg!"))
	 (<div> id: "setInnerHTML"
		(<p> id: "setInnerString1")
		(<p> id: "setInnerString2"))
	 (<p> "The end!")))

(define (test_documentObj)
  (begin
    (console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XX documentObj XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
    (console.log (documentObj))))

(define (test_nullObj)
  (let ((obj (nullObj)))
    (begin
      (console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				  "XX nullObj XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				  "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
      (console.log obj)
      (console.log (string-append "--- null?\n"
				  "    expect : true"))
      (console.log (null? obj)))))

(define (test_undefinedObj)
  (begin
    (console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XX undefinedObj XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
    (console.log (undefinedObj))))

(define (test_isUndefined)
  (begin
    (console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XX isUndefined XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
    (console.log (string-append "--- with documentObj\n"
				"    expect : false"))
    (console.log (isUndefined (documentObj)))
    (console.log (string-append "--- with nullObj\n"
				"    expect : false"))
    (console.log (isUndefined (nullObj)))
    (console.log (string-append "--- with inline-exp with undefined\n"
				"    expect : true"))
    (console.log (isUndefined (undefinedObj)))))

(define (test_isDefined)
  (begin
    (console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XX isDefined XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
    (console.log (string-append "--- with documentObj\n"
				"    expect : true"))
    (console.log (isDefined (documentObj)))
    (console.log (string-append "--- with nullObj\n"
				"    expect : true"))
    (console.log (isDefined (nullObj)))
    (console.log (string-append "--- with inline-exp with undefined\n"
				"    expect : false"))
     (console.log (isDefined (undefinedObj)))))

(define (test_isUsableObject)
  (begin
    (console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XX isUsableObject XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
    (console.log (string-append "--- with documentObj\n"
				"    expect : true"))
    (console.log (isUsableObject (documentObj)))
    (console.log (string-append "--- with nullObj\n"
				"    expect : false"))
    (console.log (isUsableObject (nullObj)))
    (console.log (string-append "--- with inline-exp with undefined\n"
				"    expect : false"))
    (console.log (isUsableObject (undefinedObj)))))

(define (test_classOf)
  (begin
    (console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XX classOf XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
    (console.log (string-append "--- with scm string\n"
				"    expect : G_ScmString"))
    (console.log (classOf "test string"))
    (console.log (string-append "--- with js string\n"
				"    expect : String"))
    (console.log (classOf (##inline-host-expression "'test string';")))
    (console.log (string-append "--- with void scm value\n"
				"    expect : hasNoClass"))
    (console.log (classOf (void)))
    (console.log (string-append "--- with nullObj\n"
				"    expect : hasNoClass"))
    (console.log (classOf (nullObj)))
    (console.log (string-append "--- with inline-exp with undefined\n"
				"    expect : hasNoClass"))
    (console.log (classOf (undefinedObj)))))

(define (test_isScmObject)
  (begin
    (console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XX isScmObject XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
    (console.log (string-append "--- with scm html object\n"
				"    expect : true"))
    (console.log (isScmObject page-main))
    (console.log (string-append "--- with scm string\n"
				"    expect : true"))
    (console.log (isScmObject "test string"))
    (console.log (string-append "--- with js string\n"
				"    expect : false"))
    (console.log (isScmObject (##inline-host-expression "'test string';")))
    (console.log (string-append "--- with nullObj\n"
				"    expect : false"))
    (console.log (isScmObject (nullObj)))
    (console.log (string-append "--- with inline-exp with undefined\n"
				"    expect : false"))
    (console.log (isScmObject (undefinedObj)))))

(define (test_getSlots)
  (begin
    (console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XX getSlots XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
    (console.log "--- with scm html object")
    (console.log (getSlots page-main))
    (console.log (string-append "--- with scm string\n"
				"    expect : undefined"))
    (console.log (getSlots "test string"))
    (console.log (string-append "--- with js string\n"
				"    expect : undefined"))
    (console.log (getSlots (##inline-host-expression "'test string';")))
    (console.log (string-append "--- with nullObj\n"
				"    expect : undefined"))
    (console.log (getSlots (nullObj)))
    (console.log (string-append "--- with inline-exp with undefined\n"
				"    expect : undefined"))
    (console.log (getSlots (undefinedObj)))))

(define (test_hasSlots)
  (begin
    (console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XX hasSlots XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
    (console.log (string-append "--- with scm html object\n"
				"    expect : true"))
    (console.log (hasSlots page-main))
    (console.log (string-append "--- with scm string\n"
				"    expect : false"))
    (console.log (hasSlots "test string"))
    (console.log (string-append "--- with js string\n"
				"    expect : false"))
    (console.log (hasSlots (##inline-host-expression "'test string';")))
    (console.log (string-append "--- with nullObj\n"
				"    expect : false"))
    (console.log (hasSlots (nullObj)))
    (console.log (string-append "--- with inline-exp with undefined\n"
				"    expect : false"))
    (console.log (hasSlots (undefinedObj)))))

(define (test_isHtmlObject)
  (begin
    (console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XX isHtmlObject XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
    (console.log (string-append "--- with scm html object\n"
				"    expect : true"))
    (console.log (isHtmlObject page-main))
    (console.log (string-append "--- with scm string\n"
				"    expect : false"))
    (console.log (isHtmlObject "test string"))
    (console.log (string-append "--- with js string\n"
				"    expect : false"))
    (console.log (isHtmlObject (##inline-host-expression "'test string';")))
    (console.log (string-append "--- with nullObj\n"
				"    expect : false"))
    (console.log (isHtmlObject (nullObj)))
    (console.log (string-append "--- with inline-exp with undefined\n"
				"    expect : false"))
    (console.log (isHtmlObject (undefinedObj)))))

(define (test_htmlToString)
  (begin
    (console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XX html->string XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
    (console.log (string-append "--- with scm html object\n"
				"    expect : <p>a test text</p>"))
    (console.log (html->string (<p> "a test text")))
    (console.log (string-append "--- with scm string\n"
				"    expect : false"))
    (console.log (isHtmlObject "test string"))
    (console.log (string-append "--- with js string\n"
				"    expect : false"))
    (console.log (isHtmlObject (##inline-host-expression "'test string';")))
    (console.log (string-append "--- with nullObj\n"
				"    expect : false"))
    (console.log (isHtmlObject (nullObj)))
    (console.log (string-append "--- with inline-exp with undefined\n"
				"    expect : false"))
    (console.log (isHtmlObject (undefinedObj)))))

(define (test_jsToScm)
  (let ((jsTemp (##inline-host-expression "'test string';"))
	(scmTemp "test string"))
    (begin
      (console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				  "XX jsObj->scmObj XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				  "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
      (console.log (string-append "--- with scm string\n"
				  "    expect : class should stay the same : G_ScmString"))
      (console.log (classOf scmTemp))
      (console.log (classOf (jsObj->scmObj scmTemp)))
      (console.log (string-append "--- with js string\n"
				  "    expect : class should change from String to G_ScmString"))
      (console.log (classOf jsTemp))
      (console.log (classOf (jsObj->scmObj jsTemp))))))

(define (test_scmToJs)
  (let ((jsTemp (##inline-host-expression "'test string';"))
	(scmTemp "test string"))
    (begin
      (console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				  "XX scmObj->jsObj XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				  "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
      (console.log (string-append "--- with scm string\n"
				  "    expect : class should change from G_ScmString to String"))
      (console.log (classOf scmTemp))
      (console.log (classOf (scmObj->jsObj scmTemp)))
      (console.log (string-append "--- with js string\n"
				  "    expect : class should stay the same : String"))
      (console.log (classOf jsTemp))
      (console.log (classOf (scmObj->jsObj jsTemp))))))

(define (test_typeOf)
  (begin
    (console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XX typeOf XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
    (console.log (string-append "--- with scm string\n"
				"    expect : object"))
    (console.log (typeOf "test string"))
    (console.log (string-append "--- with js string\n"
				"    expect : string"))
    (console.log (typeOf (##inline-host-expression "'test string';")))
    (console.log (string-append "--- with void scm value\n"
				"    expect : undefined"))
    (console.log (typeOf (void)))
    (console.log (string-append "--- with nullObj\n"
				"    expect : object"))
    (console.log (typeOf (nullObj)))
    (console.log (string-append "--- with inline-exp with undefined\n"
				"    expect : undefined"))
    (console.log (typeOf (undefinedObj)))))

(define (test_documentWrite)
  (begin
    (console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XX document.write XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
    (console.log (string-append "--- with scm html object : 'a test text'\n"
				"    expect : undefined"))
    (console.log (document.write (<p> "a test text")))
    (console.log (string-append "--- with scm string : 'test string 1 '\n"
				"    expect : undefined"))
    (console.log (document.write "test string 1 "))
    (console.log (string-append "--- with js string : 'test string 2'\n"
				"    expect : undefined"))
    (console.log (document.write (##inline-host-expression "'test string 2';")))
    (console.log (string-append "--- with scm number : 33.3\n"
				"    expect : undefined"))
    (console.log (document.write 33.3))
    (console.log (string-append "--- with js number : 44.4\n"
				"    expect : undefined"))
    (console.log (document.write (##inline-host-expression "44.4;")))
    (console.log (string-append "--- with nullObj\n"
				"    expect : false"))
    (console.log (document.write (nullObj)))
    (console.log (string-append "--- with inline-exp with undefined\n"
				"    expect : false"))
    (console.log (document.write (undefinedObj)))
    (##inline-host-statement "document.body.innerHTML = '';")))

(define (test_consoleLog)
  (begin
    (console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XX console.log XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
    (console.log (string-append "--- with scm html object\n"
				"    expect : <p>a test text</p>"))
    (console.log (<p> "a test text"))
    (console.log "    with inline call : should be different")
    (##inline-host-statement "console.log(@1@);"
			     (<p> "a test text"))
    (console.log (string-append "--- with scm string\n"
				"    expect : test string 1"))
    (console.log "test string 1")
    (console.log "    with inline call : should be different")
    (##inline-host-statement "console.log(@1@);"
			     "test string 1")
    (console.log (string-append "--- with js string\n"
				"    expect : test string 2"))
    (console.log (##inline-host-expression "'test string 2';"))
    (console.log "    with inline call : should be the same")
    (##inline-host-statement "console.log('test string 2');")
    (console.log (string-append "--- with scm number\n"
				"    expect : 33.3"))
    (console.log 33.3)
    (console.log "    with inline call : should be different")
    (##inline-host-statement "console.log(@1@);"
			     33.3)
    (console.log (string-append "--- with js number\n"
				"    expect : 44.4"))
    (console.log (##inline-host-expression "44.4;"))
    (console.log "    with inline call : should be the same")
    (##inline-host-statement "console.log(44.4);")
    (console.log (string-append "--- with nullObj\n"
				"    expect : null"))
    (console.log (nullObj))
    (console.log "    with inline call : should be the same")
    (##inline-host-statement "console.log(@1@);"
			     (nullObj))
    (console.log (string-append "--- with inline-exp with undefined\n"
				"    expect : undefined"))
    (console.log (undefinedObj))
    (console.log "    with inline call : should be the same")
    (##inline-host-statement "console.log(@1@);"
			     (undefinedObj))))

(define (test_isDOMObj)
  (begin
    (console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XX isDOMObj XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
    (console.log (string-append "--- with scm string\n"
				"    expect : false"))
    (console.log (isDOMObj "test string 1"))
    (console.log (string-append "--- with js string\n"
				"    expect : false"))
    (console.log (isDOMObj (##inline-host-expression "'test string 2';")))
    (console.log (string-append "--- with nullObj\n"
				"    expect : false"))
    (console.log (isDOMObj (nullObj)))
    (console.log (string-append "--- with inline-exp with undefined\n"
				"    expect : false"))
    (console.log (isDOMObj (undefinedObj)))
    (console.log (string-append "--- with documentObj\n"
				"    expect : true"))
    (console.log (isDOMObj (documentObj)))
    (console.log (string-append "--- with freshly created node\n"
				"    expect : true"))
    (console.log (isDOMObj (##inline-host-expression "document.createElement('P');")))))

(define (test_isDocumentObj)
  (begin
    (console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XX isDocumentObj XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
    (console.log (string-append "--- with scm string\n"
				"    expect : false"))
    (console.log (isDocumentObj "test string 1"))
    (console.log (string-append "--- with js string\n"
				"    expect : false"))
    (console.log (isDocumentObj (##inline-host-expression "'test string 2';")))
    (console.log (string-append "--- with nullObj\n"
				"    expect : false"))
    (console.log (isDocumentObj (nullObj)))
    (console.log (string-append "--- with inline-exp with undefined\n"
				"    expect : false"))
    (console.log (isDocumentObj (undefinedObj)))
    (console.log (string-append "--- with freshly created node\n"
				"    expect : false"))
    (console.log (isDocumentObj (##inline-host-expression "document.createElement('P');")))
    (console.log (string-append "--- with documentObj\n"
				"    expect : true"))
    (console.log (isDocumentObj (documentObj)))))

(define (test_getElementById)
  (let ((myId "myP"))
    (let ((myNewP (<p> id: myId
		       "this is my p")))
      (begin
	(console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XX getElementById XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	(console.log "--- write <p id='myP'>this is my p</p>")
	(document.write myNewP)
	(console.log "--- with valid scm string")
	(console.log (getElementById myId))
	(console.log (string-append "--- with invalid scm string\n"
				    "    expect : null"))
	(console.log (getElementById (string-append "invalid"
						    myId)))
	(console.log (string-append "--- with scm object (not a string)\n"
				    "    expect : null"))
	(console.log (getElementById myNewP))
	(console.log (string-append "--- with js string\n"
				    "    expect : null"))
	(console.log (getElementById (scmObj->jsObj myId)))
	(##inline-host-statement "document.body.innerHTML = '';")))))

(define (test_querySelector)
  (let ((myId "myDiv"))
    (let ((myNewP (<p> "outer p"))
	  (myNewDiv (<div> id: myId
			   (<p> "inner p"))))
      (begin
	(console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XX querySelector XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	(console.log (string-append "--- write <p>outer p</p>\n"
				    "          <div>\n"
				    "              <p>inner p</p>\n"
				    "          </div>"))
	(document.write myNewP)
	(document.write myNewDiv)
	(let ((divById (getElementById myId))
	      (validScmSelector "p")
	      (invalidScmSelector ".not a .valid .selector")
	      (jsSelector (scmObj->jsObj "p")))
	  (begin
	    (console.log (string-append "--- with valid scm string\n"
					"    expect : <p>outer p</p>"))
	    (console.log (querySelector validScmSelector))
	    (console.log (string-append "--- with valid scm string on frame\n"
					"    expect : <p>inner p</p>"))
	    (console.log (querySelector validScmSelector
					divById))
	    (console.log (string-append "--- with invalid scm string\n"
					"    expect : null"))
	    (console.log (querySelector invalidScmSelector))
	    (console.log (string-append "--- with invalid scm string on frame\n"
					"    expect : null"))
	    (console.log (querySelector invalidScmSelector
					divById))
	    (console.log (string-append "--- with scm object other than a string\n"
					"    expect : null"))
	    (console.log (querySelector myNewP))
	    (console.log (string-append "--- with js string\n"
					"    expect : null"))
	    (console.log (querySelector jsSelector))
	    (console.log (string-append "--- with valid scm string on scm objet\n"
					"    expect : null"))
	    (console.log (querySelector validScmSelector
					validScmSelector))
	    (console.log (string-append "--- with valid scm string on js objet\n"
					"    expect : null"))
	    (console.log (querySelector validScmSelector
					jsSelector))
	    (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test_querySelectorAll)
  (let ((myId "myDiv"))
    (let ((myNewP (<p> "outer p"))
	  (myNewDiv (<div> id: myId
			   (<p> "inner p"))))
      (begin
	(console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XX querySelectorAll XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	(console.log (string-append "--- write <p>outer p</p>\n"
				    "          <div>\n"
				    "              <p>inner p</p>\n"
				    "          </div>"))
	(document.write myNewP)
	(document.write myNewDiv)
	(let ((divById (getElementById myId))
	      (validScmSelector "p")
	      (invalidScmSelector ".not a .valid .selector")
	      (jsSelector (scmObj->jsObj "p")))
	  (begin
	    (console.log (string-append "--- with valid scm string\n"
					"    expect : list with : <p>outer p</p> AND <p>inner p</p>"))
	    (console.log (querySelectorAll validScmSelector))
	    (console.log (string-append "--- with valid scm string on frame\n"
					"    expect : list with : <p>inner p</p>"))
	    (console.log (querySelectorAll validScmSelector
					   divById))
	    (console.log (string-append "--- with invalid scm string\n"
					"    expect : empty list"))
	    (console.log (querySelectorAll invalidScmSelector))
	    (console.log (string-append "--- with invalid scm string on frame\n"
					"    expect : empty list"))
	    (console.log (querySelectorAll invalidScmSelector
					   divById))
	    (console.log (string-append "--- with scm object other than a string\n"
					"    expect : null"))
	    (console.log (querySelectorAll myNewP))
	    (console.log (string-append "--- with js string\n"
					"    expect : null"))
	    (console.log (querySelectorAll jsSelector))
	    (console.log (string-append "--- with valid scm string on scm objet\n"
					"    expect : null"))
	    (console.log (querySelectorAll validScmSelector
					   validScmSelector))
	    (console.log (string-append "--- with valid scm string on js not objet (not DOM nor document)\n"
					"    expect : null"))
	    (console.log (querySelectorAll validScmSelector
					   jsSelector))
	    (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test_getAttribute)
  (let ((myId "myInput")
	(myClasses "'c1 c2 c3'")
	(myValue "'a value'")
	(myCustomAtt "custom-att")
	(myCustomVal 11.1))
    (let ((myNewInput (<input> type: 'text
			       id: myId
			       name: myId
			       class: myClasses
			       value: myValue)))
      (begin
	(console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XX getAttribute XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	(console.log "--- write <input type='text' id='myInput' name='myInput' class='c1 c2 c3' value='a value' custom-att='11.1'>")
	(document.write myNewInput)
	(let ((inputById (getElementById myId))
	      (myCustomAttJs (scmObj->jsObj myCustomAtt)))
	  (begin
	    (setAttribute inputById myCustomAtt myCustomVal)
	    (console.log (string-append "--- select class on inserted input\n"
					"    expect : "
					myClasses))
	    (console.log (getAttribute inputById "class"))
	    (console.log (string-append "--- select value on inserted input\n"
					"    expect : "
					myValue))
	    (console.log (getAttribute inputById "value"))
	    (console.log (string-append "--- select custom-att (initialised with 11.1) on inserted input\n"
					"    expect : '"
					(number->string myCustomVal)
					"'"))
	    (console.log (getAttribute inputById myCustomAtt))
	    (console.log (string-append "--- select invalid attribute on inserted input\n"
					"    expect : null"))
	    (console.log (getAttribute inputById "not-an-active-att"))
	    (console.log (string-append "--- select with scm object (not a string) on inserted input\n"
					"    expect : false"))
	    (console.log (getAttribute inputById myNewInput))
	    (console.log (string-append "--- select with js object on inserted input\n"
					"    expect : false"))
	    (console.log (getAttribute inputById myCustomAttJs))
	    (console.log (string-append "--- select on js object (not a DOM object)\n"
					"    expect : false"))
	    (console.log (getAttribute myCustomAttJs myCustomAtt))
	    (console.log (string-append "--- select on scm object\n"
					"    expect : false"))
	    (console.log (getAttribute myNewInput myCustomAtt))
	    (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test_hasAttribute)
  (let ((myId "myInput")
	(myClasses "'c1 c2 c3'")
	(myValue "'a value'")
	(myCustomAtt "custom-att")
	(myCustomVal 11.1))
    (let ((myNewInput (<input> type: 'text
			       id: myId
			       name: myId
			       class: myClasses
			       value: myValue)))
      (begin
	(console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XX hasAttribute XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	(console.log "--- write <input type='text' id='myInput' name='myInput' class='c1 c2 c3' value='a value' custom-att='11.1'>")
	(document.write myNewInput)
	(let ((inputById (getElementById myId))
	      (myCustomAttJs (scmObj->jsObj myCustomAtt)))
	  (begin
	    (setAttribute inputById myCustomAtt myCustomVal)
	    (console.log (string-append "--- select class on inserted input\n"
					"    expect : true"))
	    (console.log (hasAttribute inputById "class"))
	    (console.log (string-append "--- select value on inserted input\n"
					"    expect : true"))
	    (console.log (hasAttribute inputById "value"))
	    (console.log (string-append "--- select custom-att (initialised with 11.1) on inserted input\n"
					"    expect : true"))
	    (console.log (hasAttribute inputById myCustomAtt))
	    (console.log (string-append "--- select invalid attribute on inserted input\n"
					"    expect : false"))
	    (console.log (hasAttribute inputById "not-an-active-att"))
	    (console.log (string-append "--- select with scm object (not a string) on inserted input\n"
					"    expect : false"))
	    (console.log (hasAttribute inputById myNewInput))
	    (console.log (string-append "--- select with js object on inserted input\n"
					"    expect : false"))
	    (console.log (hasAttribute inputById myCustomAttJs))
	    (console.log (string-append "--- select on js object (not a DOM object)\n"
					"    expect : false"))
	    (console.log (hasAttribute myCustomAttJs myCustomAtt))
	    (console.log (string-append "--- select on scm object\n"
					"    expect : false"))
	    (console.log (hasAttribute myNewInput myCustomAtt))
	    (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test_setAttribute)
  (let ((myId "myInput")
	(myClasses "'c1 c2 c3'")
	(myValue "'a value'")
	(myNewValue "'a brand new value'")
	(myCustomAtt "custom-att")
	(myCustomVal 11.1)
	(myNewCustomVal "'new custom val'"))
    (let ((myNewInput (<input> type: 'text
			       id: myId
			       name: myId
			       value: myValue)))
      (begin
	(console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XX setAttribute XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	(console.log "--- write <input type='text' id='myInput' name='myInput' value='a value' custom-att='11.1'>")
	(document.write myNewInput)
	(let ((inputById (getElementById myId))
	      (myCustomAttJs (scmObj->jsObj myCustomAtt)))
	  (begin
	    (setAttribute inputById myCustomAtt myCustomVal)
	    (console.log (string-append "---------------------------------------------------\n"
					"-- 1 : before modif -------------------------------\n"
					"---------------------------------------------------"))
	    (console.log (string-append "--- class\n"
					"    expect : null"))
	    (console.log (getAttribute inputById "class"))
	    (console.log (string-append "--- value\n"
					"    expect : "
					myValue))
	    (console.log (getAttribute inputById "value"))
	    (console.log (string-append "--- custom-att (initialised with 11.1)\n"
					"    expect : '"
					(number->string myCustomVal)
					"'"))
	    (console.log (getAttribute inputById myCustomAtt))
	    (console.log (string-append "---------------------------------------------------\n"
					"-- 2 : setAttribute -------------------------------\n"
					"---------------------------------------------------"))
	    (console.log (string-append "--- with scm att (not a string) on inserted input\n"
					"    expect : false"))
	    (console.log (setAttribute inputById myNewInput "val 2"))
	    (console.log (string-append "--- with js att on inserted input\n"
					"    expect : false"))
	    (console.log (setAttribute inputById myCustomAttJs "val 3"))
	    (console.log (string-append "--- on js object (not a DOM object)\n"
					"    expect : false"))
	    (console.log (setAttribute myCustomAtt myCustomAtt "val 4"))
	    (console.log (string-append "--- on scm object\n"
					"    expect : false"))
	    (console.log (setAttribute myNewInput myCustomAtt "val 5"))
	    (console.log (string-append "--- to js val\n"
					"    expect : false"))
	    (console.log (setAttribute inputById myCustomAtt myCustomAttJs))
	    (console.log (string-append "--- set class\n"
					"    expect : undefined"))
	    (console.log (setAttribute inputById "class" myClasses))
	    (console.log (string-append "--- set value\n"
					"    expect : undefined"))
	    (console.log (setAttribute inputById "value" myNewValue))
	    (console.log (string-append "--- set custom-att\n"
					"    expect : undefined"))
	    (console.log (setAttribute inputById myCustomAtt myNewCustomVal))
	    (console.log (string-append "---------------------------------------------------\n"
					"-- 3 : after modif --------------------------------\n"
					"---------------------------------------------------"))
	    (console.log (string-append "--- class\n"
					"    expect : "
					myClasses))
	    (console.log (getAttribute inputById "class"))
	    (console.log (string-append "--- value\n"
					"    expect : "
					myNewValue))
	    (console.log (getAttribute inputById "value"))
	    (console.log (string-append "--- custom-att (initialised with 11.1)\n"
					"    expect : "
					myNewCustomVal))
	    (console.log (getAttribute inputById myCustomAtt))
	    (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test_removeAttribute)
  (let ((myId "myInput")
	(myValue "'a value'")
	(myCustomAtt "custom-att")
	(myCustomVal 11.1))
    (let ((myNewInput (<input> type: 'text
			       id: myId
			       name: myId
			       value: myValue)))
      (begin
	(console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XX removeAttribute XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	(console.log "--- write <input type='text' id='myInput' name='myInput' value='a value' custom-att='11.1'>")
	(document.write myNewInput)
	(let ((inputById (getElementById myId))
	      (myCustomAttJs (scmObj->jsObj myCustomAtt)))
	  (begin
	    (setAttribute inputById myCustomAtt myCustomVal)
	    (console.log (string-append "---------------------------------------------------\n"
					"-- 1 : before modif -------------------------------\n"
					"---------------------------------------------------"))
	    (console.log (string-append "--- class\n"
					"    expect : null"))
	    (console.log (getAttribute inputById "class"))
	    (console.log (string-append "--- value\n"
					"    expect : "
					myValue))
	    (console.log (getAttribute inputById "value"))
	    (console.log (string-append "--- custom-att (initialised with 11.1)\n"
					"    expect : '"
					(number->string myCustomVal)
					"'"))
	    (console.log (getAttribute inputById myCustomAtt))
	    (console.log (string-append "---------------------------------------------------\n"
					"-- 2 : removeAttribute ----------------------------\n"
					"---------------------------------------------------"))
	    (console.log (string-append "--- with scm att (not a string) on inserted input\n"
					"    expect : false"))
	    (console.log (removeAttribute inputById myNewInput))
	    (console.log (string-append "--- with js att on inserted input\n"
					"    expect : false"))
	    (console.log (removeAttribute inputById myCustomAttJs))
	    (console.log (string-append "--- on js object (not a DOM object)\n"
					"    expect : false"))
	    (console.log (removeAttribute myCustomAtt myCustomAtt))
	    (console.log (string-append "--- on scm object\n"
					"    expect : false"))
	    (console.log (removeAttribute myNewInput myCustomAtt))
	    (console.log (string-append "--- remove class\n"
					"    expect : undefined"))
	    (console.log (removeAttribute inputById "class"))
	    (console.log (string-append "--- remove value\n"
					"    expect : undefined"))
	    (console.log (removeAttribute inputById "value"))
	    (console.log (string-append "--- remove custom-att\n"
					"    expect : undefined"))
	    (console.log (removeAttribute inputById myCustomAtt))
	    (console.log (string-append "---------------------------------------------------\n"
					"-- 3 : after modif --------------------------------\n"
					"---------------------------------------------------"))
	    (console.log (string-append "--- inserted input after removals"))
	    (console.log (getElementById myId))
	    (console.log (string-append "--- class\n"
					"    expect : null"))
	    (console.log (getAttribute inputById "class"))
	    (console.log (string-append "--- value\n"
					"    expect : null"))
	    (console.log (getAttribute inputById "value"))
	    (console.log (string-append "--- custom-att (initialised with 12.345)\n"
					"    expect : null"))
	    (console.log (getAttribute inputById myCustomAtt))
	    (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test_getElementType)
  (let ((myId "myDiv"))
    (let ((myNewDiv (<div> id: myId)))
      (begin
	(console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XX getElementType XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	(console.log "--- write <div id='myDiv'></div>")
	(document.write myNewDiv)
	(console.log (string-append "--- inserted div\n"
				    "    expect : DIV"))
	(console.log (getElementType (getElementById myId)))
	(console.log (string-append "--- on js object not DOM\n"
				    "    expect : undefined"))
	(console.log (getElementType (scmObj->jsObj myId)))
	(console.log (string-append "--- on scm object\n"
				    "    expect : false"))
	(console.log (getElementType myNewDiv))
	(##inline-host-statement "document.body.innerHTML = '';")))))

(define (test_isElementOfType)
  (let ((myId "myDiv"))
    (let ((myNewDiv (<div> id: myId)))
      (begin
	(console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XX isElementOfType XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	(console.log "--- write <div id='myDiv'></div>")
	(document.write myNewDiv)
	(let ((myDIv (getElementById myId))
	      (jsId (scmObj->jsObj myId)))
	  (begin
	    (console.log (string-append "--- inserted div is of type 'DIV'?\n"
					"    expect : true"))
	    (console.log (isElementOfType myDIv "DIV"))
	    (console.log (string-append "--- inserted div is of type 'P'?\n"
					"    expect : false"))
	    (console.log (isElementOfType myDIv "P"))
	    (console.log (string-append "--- js string is of type 'STRING'?\n"
					"    expect : false"))
	    (console.log (isElementOfType jsId "STRING"))
	    (console.log (string-append "--- js string is of type 'DIV'?\n"
					"    expect : false"))
	    (console.log (isElementOfType jsId "DIV"))
	    (console.log (string-append "--- scm object is of type 'DIV'?\n"
					"    expect : false"))
	    (console.log (isElementOfType myNewDiv "DIV"))
	    (console.log (string-append "--- scm object is of type 'P'?\n"
					"    expect : false"))
	    (console.log (isElementOfType myNewDiv "P"))
	    (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test_firstChild)
  (let ((myId "myDiv"))
    (let ((myNewDiv (<div> id: myId
			   (<div> (<p> "inner p")))))
      (begin
	(console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XX firstChild XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	(console.log "--- write <div id='myDiv'><div><p>inner p</p></div></div>")
	(document.write myNewDiv)
	(let ((divById (getElementById myId)))
	  (begin
	    (console.log (string-append "--- inserted div\n"
					"    expect : <div><p>inner p</p></div>"))
	    (console.log (firstChild divById))
	    (console.log (string-append "--- on js object not DOM\n"
					"    expect : false"))
	    (console.log (firstChild (scmObj->jsObj myId)))
	    (console.log (string-append "--- on scm object\n"
					"    expect : false"))
	    (console.log (firstChild myNewDiv))
	    (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test_parentNode)
  (let ((myId "myDiv"))
    (let ((myNewDiv (<div> (<p> id: myId
				"inner p"))))
      (begin
	(console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XX parentNode XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	(console.log "--- write <div><p id='myDiv'>inner p</p></div>")
	(document.write myNewDiv)
	(let ((pById (getElementById myId)))
	  (begin
	    (console.log (string-append "--- p inside inserted div\n"
					"    expect : <div><p>inner p</p></div>"))
	    (console.log (parentNode pById))
	    (console.log (string-append "--- on js object not DOM\n"
					"    expect : false"))
	    (console.log (parentNode (scmObj->jsObj myId)))
	    (console.log (string-append "--- on scm object\n"
					"    expect : false"))
	    (console.log (parentNode myNewDiv))
	    (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test_getNewNode)
  (begin
    (console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XX getNewNode XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
    (console.log (getNewNode))))

(define (test_removeNode)
  (let ((myId_1 "myDiv1"))
    (let ((myNewDiv_1 (<div> id: myId_1))
	  (myNewDiv_2 (getNewNode)))
      (begin
	(console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XX removeNode XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	(console.log "--- write <div id='myDiv1'></div>")
	(document.write myNewDiv_1)
	(let ((divById_1 (getElementById myId_1)))
	  (begin
	    (console.log (string-append "---------------------------------------------------\n"
					"-- 1 : using removeNode ---------------------------\n"
					"---------------------------------------------------"))
	    (console.log (string-append "--- remove inserted div\n"
					"    expect : undefined"))
	    (console.log (removeNode divById_1))
	    (console.log (string-append "--- remove uninserted div\n"
					"    expect : false"))
	    (console.log (removeNode myNewDiv_2))
	    (console.log (string-append "--- remove js string\n"
					"    expect : false"))
	    (console.log (removeNode (scmObj->jsObj myId_1)))
	    (console.log (string-append "--- remove scm string\n"
					"    expect : false"))
	    (console.log (removeNode myId_1))
	    (console.log (string-append "---------------------------------------------------\n"
					"-- 2 : after modif --------------------------------\n"
					"---------------------------------------------------"))
	    (console.log (string-append "--- body\n"
					"    expect : <body></body>"))
	    (console.log (querySelector "body"))
	    (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test_setInnerHTML)
  (let ((myId_1 "myDiv1")
	(myId_2 "myDiv2")
	(myId_3 "myDiv3"))
    (let ((myNewDiv_1 (<div> id: myId_1
			     (<p> "p no. 1")))
	  (myNewDiv_2 (<div> id: myId_2
			     (<p> "p no. 2")))
	  (myNewDiv_3 (<div> id: myId_3
			     (<p> "p no. 3")))
	  (newInner_1 (<ul> (<li> "First")
			    (<li> "Second")
			    (<li> "...")))
	  (newInner_2 "<span>new inner content no. 2</span>")
	  (newInner_3 (scmObj->jsObj "<span>new inner content no. 3</span>")))
      (begin
	(console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XX setInnerHTML XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	(console.log (string-append "--- write <div id='myDiv1'><p>p no. 1</p></div>\n"
				    "          <div id='myDiv2'><p>p no. 2</p></div>\n"
				    "          <div id='myDiv3'><p>p no. 3</p></div>"))
	(document.write myNewDiv_1)
	(document.write myNewDiv_2)
	(document.write myNewDiv_3)
	(let ((divById_1 (getElementById myId_1))
	      (divById_2 (getElementById myId_2))
	      (divById_3 (getElementById myId_3)))
	  (begin
	    (console.log (string-append "---------------------------------------------------\n"
					"-- 1 : using setInnerHTML -------------------------\n"
					"---------------------------------------------------"))
	    (console.log (string-append "--- replace div 1's content with html object\n"
					"    expect : undefined"))
	    (console.log (setInnerHTML divById_1 newInner_1))
	    (console.log (string-append "--- replace div 2's content with scm string\n"
					"    expect : undefined"))
	    (console.log (setInnerHTML divById_2 newInner_2))
	    (console.log (string-append "--- replace div 3's content with js string\n"
					"    expect : undefined"))
	    (console.log (setInnerHTML divById_3 newInner_3))
	    (console.log (string-append "--- replace js object's content (not DOM)\n"
					"    expect : false"))
	    (console.log (setInnerHTML newInner_3 newInner_2))
	    (console.log (string-append "--- replace scm object's content\n"
					"    expect : false"))
	    (console.log (setInnerHTML myNewDiv_1 newInner_2))
	    (console.log (string-append "---------------------------------------------------\n"
					"-- 2 : after modif --------------------------------\n"
					"---------------------------------------------------"))
	    (console.log (string-append "--- div 1\n"
					"    expect : <div id='myDiv1'><ul><li>First</li><li>Seconde</li><li>...</li></ul></div>"))
	    (console.log (getElementById myId_1))
	    (console.log (string-append "--- div 2\n"
					"    expect : <div id='myDiv2'><span>new inner content no. 2</span></div>"))
	    (console.log (getElementById myId_2))
	    (console.log (string-append "--- div 3\n"
					"    expect : <div id='myDiv3'><span>new inner content no. 3</span></div>"))
	    (console.log (getElementById myId_3))
	    (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test_appendNode)
  (let ((myId_1 "myDiv1")
	(myId_2 "myDiv2"))
    (let ((myNewDiv_1 (<div> id: myId_1))
	  (myNewDiv_2 (<div> id: myId_2)))
      (begin
	(console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XX appendNode XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	(console.log (string-append "--- write <div id='myDiv1'></div>\n"
				    "          <div id='myDiv2'></div>"))
	(document.write myNewDiv_1)
	(document.write myNewDiv_2)
	(let ((divById_1 (getElementById myId_1))
	      (divById_2 (getElementById myId_2))
	      (myIdJs (scmObj->jsObj myId_1)))
	  (begin
	    (console.log (string-append "---------------------------------------------------\n"
					"-- 1 : using appendNode ---------------------------\n"
					"---------------------------------------------------"))
	    (console.log (string-append "--- append div 2 to div 1\n"
					"    expect : undefined"))
	    (console.log (appendNode divById_1 divById_2))
	    (console.log (string-append "--- append js string to div 1\n"
					"    expect : false"))
	    (console.log (appendNode divById_1 myIdJs))
	    (console.log (string-append "--- append scm string to div 1\n"
					"    expect : false"))
	    (console.log (appendNode divById_1 myId_1))
	    (console.log (string-append "--- append div 2 to js object (not DOM)\n"
					"    expect : false"))
	    (console.log (appendNode myIdJs divById_2))
	    (console.log (string-append "--- append js string to js object (not DOM)\n"
					"    expect : false"))
	    (console.log (appendNode myIdJs myIdJs))
	    (console.log (string-append "--- append scm string to js object (not DOM)\n"
					"    expect : false"))
	    (console.log (appendNode myIdJs myId_1))
	    (console.log (string-append "--- append div 2 to scm object\n"
					"    expect : false"))
	    (console.log (appendNode myNewDiv_1 divById_2))
	    (console.log (string-append "--- append js string to scm object\n"
					"    expect : false"))
	    (console.log (appendNode myNewDiv_1 myIdJs))
	    (console.log (string-append "--- append scm string to scm object\n"
					"    expect : false"))
	    (console.log (appendNode myNewDiv_1 myId_1))
	    (console.log (string-append "---------------------------------------------------\n"
					"-- 2 : after modif --------------------------------\n"
					"---------------------------------------------------"))
	    (console.log (string-append "--- div 1\n"
					"    expect : <div id='myDiv1'><div id='myDiv2'></div></div>"))
	    (console.log (getElementById myId_1))
	    (console.log (string-append "--- div 2\n"
					"    expect : <div id='myDiv2'></div>"))
	    (console.log (getElementById myId_2))
	    (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test_appendHTML)
  (let ((myId_1 "myDiv1")
	(myId_2 "myDiv2")
	(myId_3 "myDiv3"))
    (let ((myNewDiv_1 (<div> id: myId_1))
	  (myNewDiv_2 (<div> id: myId_2))
	  (myNewDiv_3 (<div> id: myId_3))
	  (newInner_1 (<ul> (<li> "First")
			    (<li> "Second")
			    (<li> "...")))
	  (newInner_2 "<span>new inner content no. 2</span>")
	  (newInner_3 (scmObj->jsObj "<span>new inner content no. 3</span>")))
      (begin
	(console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XX appendHTML XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	(console.log (string-append "--- write <div id='myDiv1'></div>\n"
				    "          <div id='myDiv2'></div>\n"
				    "          <div id='myDiv3'></div>"))
	(document.write myNewDiv_1)
	(document.write myNewDiv_2)
	(document.write myNewDiv_3)
	(let ((divById_1 (getElementById myId_1))
	      (divById_2 (getElementById myId_2))
	      (divById_3 (getElementById myId_3)))
	  (begin
	    (console.log (string-append "---------------------------------------------------\n"
					"-- 1 : using appendHTML ---------------------------\n"
					"---------------------------------------------------"))
	    (console.log (string-append "--- append html object div 1\n"
					"    expect : undefined"))
	    (console.log (setInnerHTML divById_1 newInner_1))
	    (console.log (string-append "--- append scm string div 2\n"
					"    expect : undefined"))
	    (console.log (appendHTML divById_2 newInner_2))
	    (console.log (string-append "--- append js string div 3\n"
					"    expect : undefined"))
	    (console.log (appendHTML divById_3 newInner_3))
	    (console.log (string-append "--- on js object (not DOM)\n"
					"    expect : false"))
	    (console.log (appendHTML newInner_3 newInner_2))
	    (console.log (string-append "--- on scm object\n"
					"    expect : false"))
	    (console.log (appendHTML myNewDiv_1 newInner_2))
	    (console.log (string-append "---------------------------------------------------\n"
					"-- 2 : after modif --------------------------------\n"
					"---------------------------------------------------"))
	    (console.log (string-append "--- div 1\n"
					"    expect : <div id='myDiv1'><ul><li>First</li><li>Seconde</li><li>...</li></ul></div>"))
	    (console.log (getElementById myId_1))
	    (console.log (string-append "--- div 2\n"
					"    expect : <div id='myDiv2'><span>new inner content no. 2</span></div>"))
	    (console.log (getElementById myId_2))
	    (console.log (string-append "--- div 3\n"
					"    expect : <div id='myDiv3'><span>new inner content no. 3</span></div>"))
	    (console.log (getElementById myId_3))
	    (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test_insertNodeBefore)
  (let ((myId_1 "myDiv1")
	(myId_11 "myP")
	(myId_2 "myDiv2")
	(myId_3 "myDiv3"))
    (let ((myNewDiv_1 (<div> id: myId_1
			     (<p> id: myId_11)))
	  (myNewDiv_2 (<div> id: myId_2))
	  (myNewDiv_3 (<div> id: myId_3)))
      (begin
	(console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XX insertNodeBefore XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	(console.log (string-append "--- write <div id='myDiv1'><p id='myP'></p></div>\n"
				    "          <div id='myDiv2'></div>\n"
				    "          <div id='myDiv3'></div>"))
	(document.write myNewDiv_1)
	(document.write myNewDiv_2)
	(document.write myNewDiv_3)
	(let ((divById_1 (getElementById myId_1))
	      (pById (getElementById myId_11))
	      (divById_2 (getElementById myId_2))
	      (divById_3 (getElementById myId_3))
	      (myIdJs (scmObj->jsObj myId_1)))
	  (begin
	    (console.log (string-append "---------------------------------------------------\n"
					"-- 1 : using insertNodeBefore ---------------------\n"
					"---------------------------------------------------"))
	    (console.log (string-append "--- insert div 2 before div 1's p\n"
					"    expect : undefined"))
	    (console.log (insertNodeBefore divById_1 divById_2 pById))
	    (console.log (string-append "--- insert div 1's p before div 3 in div 1\n"
					"    expect : false"))
	    (console.log (insertNodeBefore divById_1 pById divById_3))
	    (console.log (string-append "--- insert div 3 in div 1 before js string\n"
					"    expect : false"))
	    (console.log (insertNodeBefore divById_1 divById_3 myIdJs))
	    (console.log (string-append "--- insert div 3 in div 1 before scm string\n"
					"    expect : false"))
	    (console.log (insertNodeBefore divById_1 divById_3 myId_1))
	    (console.log (string-append "--- insert div 3 in div 1 before scm object\n"
					"    expect : false"))
	    (console.log (insertNodeBefore divById_1 divById_3 myNewDiv_2))
	    (console.log (string-append "--- insert js string in div 1 before div 2\n"
					"    expect : false"))
	    (console.log (insertNodeBefore divById_1 myIdJs divById_2))
	    (console.log (string-append "--- insert scm string in div 1 before div 2\n"
					"    expect : false"))
	    (console.log (insertNodeBefore divById_1 myId_1 divById_2))
	    (console.log (string-append "--- insert scm object in div 1 before div 2\n"
					"    expect : false"))
	    (console.log (insertNodeBefore divById_1 myNewDiv_3 divById_2))
	    (console.log (string-append "--- insert div 3 in js string before div 2\n"
					"    expect : false"))
	    (console.log (insertNodeBefore myIdJs divById_3 divById_2))
	    (console.log (string-append "--- insert div 3 in scm string before div 2\n"
					"    expect : false"))
	    (console.log (insertNodeBefore myId_1 divById_3 divById_2))
	    (console.log (string-append "--- insert div 3 in scm object before div 2\n"
					"    expect : false"))
	    (console.log (insertNodeBefore myNewDiv_1 divById_3 divById_2))
	    (console.log (string-append "---------------------------------------------------\n"
					"-- 2 : after modif --------------------------------\n"
					"---------------------------------------------------"))
	    (console.log (string-append "--- div 1\n"
					"    expect : <div id='myDiv1'><div id='myDiv2'></div><p id='myP'></p></div>"))
	    (console.log (getElementById myId_1))
	    (console.log (string-append "--- div 3\n"
					"    expect : <div id='myDiv3'></div>"))
	    (console.log (getElementById myId_3))
	    (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test_insertHTMLBefore)
  (let ((myId_1 "myDiv1")
	(myId_11 "myP")
	(myId_2 "myDiv2")
	(myId_3 "myDiv3"))
    (let ((myNewDiv_1 (<div> id: myId_1
			     (<p> id: myId_11)))
	  (myNewDiv_2 (<div> id: myId_2))
	  (myNewDiv_3 (<div> id: myId_3))
	  (myNewDiv_4 (<div>)))
      (begin
	(console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XX insertHTMLBefore XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	(console.log (string-append "--- write <div id='myDiv1'><p id='myP'></p></div>\n"
				    "          <div id='myDiv2'></div>\n"
				    "          <div id='myDiv3'></div>"))
	(document.write myNewDiv_1)
	(document.write myNewDiv_2)
	(document.write myNewDiv_3)
	(let ((divById_1 (getElementById myId_1))
	      (pById (getElementById myId_11))
	      (divById_2 (getElementById myId_2))
	      (divById_3 (getElementById myId_3))
	      (myIdJs (scmObj->jsObj myId_1)))
	  (begin
	    (console.log (string-append "---------------------------------------------------\n"
					"-- 1 : using insertHTMLBefore ---------------------\n"
					"---------------------------------------------------"))
	    (console.log (string-append "--- insert div 2 before div 1's p\n"
					"    expect : undefined"))
	    (console.log (insertHTMLBefore divById_1 divById_2 pById))
	    (set! divById_2 (getElementById myId_2))
	    (console.log (string-append "--- insert div 1's p before div 3 in div 1\n"
					"    expect : false"))
	    (console.log (insertHTMLBefore divById_1 pById divById_3))
	    (console.log (string-append "--- insert div 3 in div 1 before js string\n"
					"    expect : false"))
	    (console.log (insertHTMLBefore divById_1 divById_3 myIdJs))
	    (console.log (string-append "--- insert div 3 in div 1 before scm string\n"
					"    expect : false"))
	    (console.log (insertHTMLBefore divById_1 divById_3 myId_1))
	    (console.log (string-append "--- insert div 3 in div 1 before scm object\n"
					"    expect : false"))
	    (console.log (insertHTMLBefore divById_1 divById_3 myNewDiv_2))
	    (console.log (string-append "--- insert js string in div 1 before div 2\n"
					"    expect : undefined"))
	    (console.log (insertHTMLBefore divById_1 myIdJs divById_2))
	    (console.log (string-append "--- insert scm string in div 1 before div 2\n"
					"    expect : undefined"))
	    (console.log (insertHTMLBefore divById_1 myId_1 divById_2))
	    (console.log (string-append "--- insert scm object in div 1 before div 2\n"
					"    expect : undefined"))
	    (console.log (insertHTMLBefore divById_1 myNewDiv_4 divById_2))
	    (console.log (string-append "--- insert div 3 in js string before div 2\n"
					"    expect : false"))
	    (console.log (insertHTMLBefore myIdJs divById_3 divById_2))
	    (console.log (string-append "--- insert div 3 in scm string before div 2\n"
					"    expect : false"))
	    (console.log (insertHTMLBefore myId_1 divById_3 divById_2))
	    (console.log (string-append "--- insert div 3 in scm object before div 2\n"
					"    expect : false"))
	    (console.log (insertHTMLBefore myNewDiv_1 divById_3 divById_2))
	    (console.log (string-append "---------------------------------------------------\n"
					"-- 2 : after modif --------------------------------\n"
					"---------------------------------------------------"))
	    (console.log (string-append "--- div 1\n"
					"    expect : <div id='myDiv1'>myDiv1myDiv1<div></div><div id='myDiv2'></div><p id='myP'></p></div>"))
	    (console.log (getElementById myId_1))
	    (console.log (string-append "--- div 3\n"
					"    expect : <div id='myDiv3'></div>"))
	    (console.log (getElementById myId_3))
	    (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test_isElementPresentByQuery)
  (let ((myId "myDiv"))
    (let ((myNewP (<p> "outer p"))
	  (myNewDiv (<div> id: myId
			   (<p> "inner p"))))
      (begin
	(console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XX isElementPresentByQuery XXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	(console.log (string-append "--- write <p>outer p</p>\n"
				    "          <div>\n"
				    "              <p>inner p</p>\n"
				    "          </div>"))
	(document.write myNewP)
	(document.write myNewDiv)
	(let ((divById (getElementById myId))
	      (validScmSelector "p")
	      (invalidScmSelector ".not a .valid .selector")
	      (jsSelector (scmObj->jsObj "p")))
	  (begin
	    (console.log (string-append "--- with valid scm string\n"
					"    expect : true>"))
	    (console.log (isElementPresentByQuery validScmSelector))
	    (console.log (string-append "--- with valid scm string on frame\n"
					"    expect : true"))
	    (console.log (isElementPresentByQuery validScmSelector
						  divById))
	    (console.log (string-append "--- with invalid scm string\n"
					"    expect : false"))
	    (console.log (isElementPresentByQuery invalidScmSelector))
	    (console.log (string-append "--- with invalid scm string on frame\n"
					"    expect : false"))
	    (console.log (isElementPresentByQuery invalidScmSelector
						  divById))
	    (console.log (string-append "--- with scm object other than a string\n"
					"    expect : false"))
	    (console.log (isElementPresentByQuery myNewP))
	    (console.log (string-append "--- with js string\n"
					"    expect : false"))
	    (console.log (isElementPresentByQuery jsSelector))
	    (console.log (string-append "--- with valid scm string on scm objet\n"
					"    expect : false"))
	    (console.log (isElementPresentByQuery validScmSelector
						  validScmSelector))
	    (console.log (string-append "--- with valid scm string on js objet\n"
					"    expect : false"))
	    (console.log (isElementPresentByQuery validScmSelector
						  jsSelector))
	    (##inline-host-statement "document.body.innerHTML = '';")))))))

(define (test_replaceNode)
  (let ((myId_1 "myDiv1")
	(myId_11 "myP")
	(myId_2 "myDiv2")
	(myId_3 "myDiv3"))
    (let ((myNewDiv_1 (<div> id: myId_1
			     (<p> id: myId_11)))
	  (myNewDiv_2 (<div> id: myId_2))
	  (myNewDiv_3 (getNewNode)))
      (begin
	(console.log (string-append "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XX replaceNode XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	(console.log (string-append "--- write <div id='myDiv1'><p id='myP'></p></div>\n"
				    "          <div id='myDiv2'></div>"))
	(document.write myNewDiv_1)
	(document.write myNewDiv_2)
	(let ((divById_1 (getElementById myId_1))
	      (pById (getElementById myId_11))
	      (divById_2 (getElementById myId_2))
	      (myIdJs (scmObj->jsObj myId_1)))
	  (begin
	    (console.log (string-append "---------------------------------------------------\n"
					"-- 1 : using replaceNode --------------------------\n"
					"---------------------------------------------------"))
	    (console.log (string-append "--- replace div 1's p with div 2\n"
					"    expect : undefined"))
	    (console.log (replaceNode pById divById_2))
	    (console.log (string-append "--- replace uninserted div 3 with div 2\n"
					"    expect : false"))
	    (console.log (replaceNode divById_3 divById_2))
	    (console.log (string-append "--- replace js string with div 3\n"
					"    expect : false"))
	    (console.log (replaceNode myIdJs divById_3))
	    (console.log (string-append "--- replace scm string with div 3\n"
					"    expect : false"))
	    (console.log (replaceNode myId_1 divById_3))
	    (console.log (string-append "--- replace scm object with div 3\n"
					"    expect : false"))
	    (console.log (replaceNode myNewDiv_2 divById_3))
	    (console.log (string-append "--- replace div 2 with js string\n"
					"    expect : false"))
	    (console.log (replaceNode divById_2 myIdJs))
	    (console.log (string-append "--- replace div 2 with scm string\n"
					"    expect : false"))
	    (console.log (replaceNode divById_2 myId_1))
	    (console.log (string-append "--- replace div 2 with scm object\n"
					"    expect : false"))
	    (console.log (replaceNode divById_2 myNewDiv_1))
	    (console.log (string-append "---------------------------------------------------\n"
					"-- 2 : after modif --------------------------------\n"
					"---------------------------------------------------"))
	    (console.log (string-append "--- body's children\n"
					"    expect : <div id='myDiv1'><div id='myDiv2'></div></div>\n"
					"             <div id='myDiv2'></div>"))
	    (console.log (querySelectorAll "*" (querySelector "body")))
	    (##inline-host-statement "document.body.innerHTML = '';")))))))

					; -------------------------------------------------------------------------
					; RUN TESTS FUNCTION
					; -------------------------------------------------------------------------
(define (runTests)
  (test_documentObj)
  (test_nullObj)
  (test_undefinedObj)
  (test_isUndefined)
  (test_isDefined)
  (test_isUsableObject)
  (test_classOf)
  (test_isScmObject)
  (test_getSlots)
  (test_hasSlots)
  (test_isHtmlObject)
  (test_htmlToString)
  (test_jsToScm)
  (test_scmToJs)
  (test_typeOf)
  (test_documentWrite)
  (test_consoleLog)
  (test_isDOMObj)
  (test_isDocumentObj)
  (test_getElementById)
  (test_querySelector)
  (test_querySelectorAll)
  (test_getAttribute)
  (test_hasAttribute)
  (test_setAttribute)
  (test_removeAttribute)
  (test_getElementType)
  (test_isElementOfType)
  (test_firstChild)
  (test_parentNode)
  (test_getNewNode)
  (test_removeNode)
  (test_setInnerHTML)
  (test_appendNode)
  (test_appendHTML)
  (test_insertNodeBefore)
  (test_insertHTMLBefore)
  (test_isElementPresentByQuery)
  (test_replaceNode))
