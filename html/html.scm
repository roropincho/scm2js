;==============================================================================

; File: "html.scm", Time-stamp: <2008-12-15 11:53:45 feeley>

; Copyright (c) 2000-2008 by Brad Lucier, All Rights Reserved.
; Copyright (c) 2005-2008 by Marc Feeley, All Rights Reserved.

; This is an attempt at a complete implementation of HTML 4.0 in
; Scheme without any Netscape or Microsoft extensions.

;==============================================================================

;(##import (gambit))
;(##namespace ("html#"))
;(##include "html#.scm")
#|
(declare
  (standard-bindings)
  (extended-bindings)
  (block)
  (not safe)
)
|#
;==============================================================================

;;; All html tags are translated to scheme functions with name
;;; <tagname> that takes keyword arguments for the attributes and an
;;; optional rest parameter if the tag has an end-tag.

(define-type html
  id: a121f3a9-a905-4db5-b2c2-6da0a7e47046
  read-only:
  unprintable:

  pre-tag
  body
  post-tag
)

(define-type unprotected
  id: 33754754-e720-4e7a-b84d-2a205415f1ff
  read-only:
  unprintable:

  content
)

;;; Display the structure we built, which is a tree of Scheme objects.

(define (write-html x #!optional (port (current-output-port)))

  (declare (fixnum))

;;; This table has a non-#f entry for every character that is valid in
;;; a standard HTML document.  The entry is what should be displayed when
;;; this character occurs.

  (define character-entity-table
    '#(#\nul
       #f; #\x01
       #f; #\x02
       #f; #\x03
       #f; #\x04
       #f; #\x05
       #f; #\x06
       #f; #\alarm
       #f; #\backspace
       #\tab
       #\newline
       #f; #\vtab
       #f; #\page
       #\return
       #f; #\x0E
       #f; #\x0F
       #f; #\x10
       #f; #\x11
       #f; #\x12
       #f; #\x13
       #f; #\x14
       #f; #\x15
       #f; #\x16
       #f; #\x17
       #f; #\x18
       #f; #\x19
       #f; #\x1A
       #f; #\x1B
       #f; #\x1C
       #f; #\x1D
       #f; #\x1E
       #f; #\x1F
       #\space
       #\!
       "&quot;"
       #\#
       #\$
       #\%
       "&amp;"
       #\'
       #\(
       #\)
       #\*
       #\+
       #\,
       #\-
       #\.
       #\/
       #\0
       #\1
       #\2
       #\3
       #\4
       #\5
       #\6
       #\7
       #\8
       #\9
       #\:
       #\;
       "&lt;"
       #\=
       "&gt;"
       #\?
       #\@
       #\A
       #\B
       #\C
       #\D
       #\E
       #\F
       #\G
       #\H
       #\I
       #\J
       #\K
       #\L
       #\M
       #\N
       #\O
       #\P
       #\Q
       #\R
       #\S
       #\T
       #\U
       #\V
       #\W
       #\X
       #\Y
       #\Z
       #\[
       #\\
       #\]
       #\^
       #\_
       #\`
       #\a
       #\b
       #\c
       #\d
       #\e
       #\f
       #\g
       #\h
       #\i
       #\j
       #\k
       #\l
       #\m
       #\n
       #\o
       #\p
       #\q
       #\r
       #\s
       #\t
       #\u
       #\v
       #\w
       #\x
       #\y
       #\z
       #\{
       #\|
       #\}
       #\~
       #f; #\rubout
       #f; "&#128;"
       #f; "&#129;"
       "&#130;"
       "&#131;"
       "&#132;"
       "&#133;"
       "&#134;"
       "&#135;"
       "&#136;"
       "&#137;"
       "&#138;"
       "&#139;"
       "&#140;"
       #f; "&#141;"
       "&#142;"
       #f; "&#143;"
       #f; "&#144;"
       "&#145;"
       "&#146;"
       "&#147;"
       "&#148;"
       "&#149;"
       "&#150;"
       "&#151;"
       "&#152;"
       "&#153;"
       "&#154;"
       "&#155;"
       "&#156;"
       #f; "&#157;"
       "&#158;"
       "&#159;"
       "&#160;"
       "&#161;"
       "&#162;"
       "&#163;"
       "&#164;"
       "&#165;"
       "&#166;"
       "&#167;"
       "&#168;"
       "&#169;"
       "&#170;"
       "&#171;"
       "&#172;"
       "&#173;"
       "&#174;"
       "&#175;"
       "&#176;"
       "&#177;"
       "&#178;"
       "&#179;"
       "&#180;"
       "&#181;"
       "&#182;"
       "&#183;"
       "&#184;"
       "&#185;"
       "&#186;"
       "&#187;"
       "&#188;"
       "&#189;"
       "&#190;"
       "&#191;"
       "&#192;"
       "&#193;"
       "&#194;"
       "&#195;"
       "&#196;"
       "&#197;"
       "&#198;"
       "&#199;"
       "&#200;"
       "&#201;"
       "&#202;"
       "&#203;"
       "&#204;"
       "&#205;"
       "&#206;"
       "&#207;"
       "&#208;"
       "&#209;"
       "&#210;"
       "&#211;"
       "&#212;"
       "&#213;"
       "&#214;"
       "&#215;"
       "&#216;"
       "&#217;"
       "&#218;"
       "&#219;"
       "&#220;"
       "&#221;"
       "&#222;"
       "&#223;"
       "&#224;"
       "&#225;"
       "&#226;"
       "&#227;"
       "&#228;"
       "&#229;"
       "&#230;"
       "&#231;"
       "&#232;"
       "&#233;"
       "&#234;"
       "&#235;"
       "&#236;"
       "&#237;"
       "&#238;"
       "&#239;"
       "&#240;"
       "&#241;"
       "&#242;"
       "&#243;"
       "&#244;"
       "&#245;"
       "&#246;"
       "&#247;"
       "&#248;"
       "&#249;"
       "&#250;"
       "&#251;"
       "&#252;"
       "&#253;"
       "&#254;"
       "&#255;"
       ))

  (define (protect x)
    (cond ((unprotected? x)
           (d (unprotected-content x)))
          ((pair? x)
           (for-each protect x))
          ((html? x)
           (d (html-pre-tag x))
           (protect (html-body x))
           (d (html-post-tag x)))
          ((null? x)
           (void))
          ((string? x)
           (let ((n (string-length x)))
             (let loop ((start 0) (end 0))
               (if (= end n)
                   (write-substring x start end port)
                   (let* ((ch (string-ref x end))
                          (index (char->integer ch)))
                     (cond ((and (< index 256)
                                 (vector-ref character-entity-table index))
                            =>
                            (lambda (character-value)
                              (if (char? character-value)
                                  (loop start (+ end 1))
                                  (begin  ; it's a string
                                    (write-substring
                                     x
                                     start
                                     end
                                     port)
                                    (write-substring
                                     character-value
                                     0
                                     (string-length character-value)
                                     port)
                                    (loop (+ end 1) (+ end 1))))))
                           (else
                            (display
                             (string-append
                              "Warning: Character (integer->char "
                              (number->string index)
                              ") is not a valid HTML 4.0 character entity\n")
                             (current-error-port))
                            (loop start (+ end 1)))))))))
          ((char? x)
           (let ((index (char->integer x)))
             (cond ((and (< index 256)
                         (vector-ref character-entity-table index))
                    =>
                    (lambda (character-value)
                      (if (char? character-value)
                          (write-char character-value port)
                          (write-substring
                           character-value
                           0
                           (string-length character-value)
                           port))))
                   (else
                    (display
                     (string-append
                      "Warning: Character (integer->char "
                      (number->string index)
                      ") is not a valid HTML 4.0 character entity\n")
                     (current-error-port))
                    (write-char x port)))))
          (else
           (display x port))))

  (define (d x)
    (cond ((pair? x)
           (for-each d x))
          ((html? x)
           (error "Shouldn't be a form here" x))
          ((null? x)
           (void))
          ((string? x)
           (write-substring x 0 (string-length x) port))
          (else
           (display x port))))

  (protect  x))

;;; this function parses the args of such a form.

(define (html-parse-args form-name end-tag? attribute-alist single-attribute-alist args)
  (let loop ((args args))
    (cond ((and (not (null? args)) (keyword? (car args)))
           (let ((key (car args))
                 (args (cdr args)))
             (cond ((assq key attribute-alist)
                    => (lambda (entry)
                         (cond ((cdr entry)
                                (error "Keyword used more than once" form-name key))
                               ((null? args)
                                (error "Keyword must take an argument" form-name key))
                               (else
                                (set-cdr! entry (car args))
                                (loop (cdr args))))))
                   ((assq key single-attribute-alist)
                    => (lambda (entry)
                         (cond ((cdr entry)
                                (error "Keyword used more than once" form-name key))
                               (else
                                (set-cdr! entry #t)
                                (loop args)))))
                   (else
                    (error "Unrecognized keyword" form-name key)))))
          ((and (not end-tag?) (not (null? args)))
           (error "Body found in tag without end-tag" form-name args))
          (else
           args)))) ; return

;;; this function builds the form

(define (html-build-form pre-tag-start post-tag attribute-alist attribute-strings single-attribute-alist single-attribute-strings args)
  (let ((pre-tag
         (let loop ((alist attribute-alist)
                    (strings attribute-strings)
                    (out (list ">")))
           (if (not (null? alist))
               (let ((entry (car alist)))
                 (if (cdr entry)
                     (loop (cdr alist)
                           (cdr strings)
                           (cons (list (car strings)
                                       (cdr entry))
                                 out))
                     (loop (cdr alist)
                           (cdr strings)
                           out)))
               (let loop ((alist single-attribute-alist)
                          (strings single-attribute-strings)
                          (out out))
                 (if (not (null? alist))
                     (let ((entry (car alist)))
                       (if (cdr entry)
                           (loop (cdr alist)
                                 (cdr strings)
                                 (cons (car strings)
                                       out))
                           (loop (cdr alist)
                                 (cdr strings)
                                 out)))
                     (cons pre-tag-start out)))))))
    (make-html pre-tag args post-tag)))

;;; tags are defined with this macro.  It takes a required tag-name as
;;; the first argument, and the following key arguments:

;;; allow-event-attributes?: Takes HTML 5.0 event attributes (default #t)
;;; end-tag?: takes an end tag and a body (default #t)
;;; attributes: the attributes of the tag that take values
;;; single-attributes: the attributes of the tag that don't take a value.

;;; There should also be a required: argument giving required
;;; attributes for each tag.

(define-macro (define-tag . args)
  (let ((internal-define-tag
         (lambda (tag-name
                  #!key
                  (allow-event-attributes? #t)
                  (end-tag? #t)
                  (attributes '())
                  (single-attributes '()))
           (let ((core-attributes '(accesskey
                                    autocapitalize
                                    class
                                    contenteditable
; ----------------------------------> also any attribute starting with 'data-'
                                    dir
                                    draggable
                                    id
                                    inputmode
                                    is
                                    itemid
                                    itemprop
                                    itemref
                                    itemscope
                                    itemtype
                                    lang
                                    slot
                                    spellcheck
                                    style
                                    tabindex
                                    title
                                    translate))
                 (core-single-attributes '(hidden))
                 (event-attributes '(onabort
                                     onafterprint
                                     onbeforeprint
                                     onbeforeunload
                                     onblur
                                     oncanplay
                                     oncanplaythrough
                                     onchange
                                     onclick
                                     oncontextmenu
                                     oncopy
                                     oncuechange
                                     oncut
                                     ondblclick
                                     ondrag
                                     ondragend
                                     ondragenter
                                     ondragleave
                                     ondragover
                                     ondragstart
                                     ondrop
                                     ondurationchange
                                     onemptied
                                     onended
                                     onerror
                                     onfocus
                                     onhashchange
                                     oninput
                                     oninvalid
                                     onkeydown
                                     onkeypress
                                     onkeyup
                                     onlanguagechange
                                     onload
                                     onloadeddata
                                     onloadedmetadata
                                     onloadstart
                                     onmessage
                                     onmousedown
                                     onmousemove
                                     onmouseout
                                     onmouseover
                                     onmouseup
                                     onmousewheel
                                     onoffline
                                     ononline
                                     onpageshow
                                     onpaste
                                     onpause
                                     onplay
                                     onplaying
                                     onpopstate
                                     onprogress
                                     onratechange
                                     onredo
                                     onreset
                                     onresize
                                     onscroll
                                     onsearch
                                     onseeked
                                     onseeking
                                     onselect
                                     onstalled
                                     onstorage
                                     onsubmit
                                     onsuspend
                                     ontimeupdate
                                     ontoggle
                                     onundo
                                     onunload
                                     onvolumechange
                                     onwaiting
                                     onwheel)))
             (let* ((attributes
                     (append
                      (if allow-event-attributes?
                          (append attributes event-attributes)
                          attributes)
                      core-attributes))
                    (single-attributes
                     (append single-attributes core-single-attributes))
                    (attribute-keywords
                     (map (lambda (x) (string->keyword (symbol->string x))) attributes))
                    (attribute-strings
                     (map (lambda (x) (string-append " " (symbol->string x) "=")) attributes))
                    (single-attribute-keywords
                     (map (lambda (x) (string->keyword (symbol->string x))) single-attributes))
                    (single-attribute-strings
                     (map (lambda (x) (string-append " " (symbol->string x))) single-attributes))
                    (form-name
                     (string->symbol (string-append "<" (symbol->string tag-name) ">"))))
               `(define (,form-name . args)
                  (let ((attribute-alist (map (lambda (x) (cons x #f)) ,(list 'quote attribute-keywords)))
                        (single-attribute-alist (map (lambda (x) (cons x #f)) ,(list 'quote single-attribute-keywords))))
                    (let ((args (html-parse-args ,(list 'quote form-name) ,end-tag? attribute-alist single-attribute-alist args)))
                      (html-build-form
                       ,(string-append "<"
                                       (symbol->string tag-name))
                       ,(if end-tag?
                            (string-append "</"
                                           (symbol->string tag-name)
                                           ">")
                            ''())
                       attribute-alist
                       ,(list 'quote attribute-strings)
                       single-attribute-alist
                       ,(list 'quote single-attribute-strings)
                       args)))))))))

    (apply internal-define-tag args)))

(define-macro (foo x) 111)
(define-macro (bar x) 222)

(define-tag a
  attributes: (charset
               coords
               download
               href
               hreflang
               media
               name
               ping
               referrerpolicy
               rel
               rev
               shape
               target
               type))

(define-tag abbr)

(define-tag acronym)

(define-tag address)

(define-tag applet
  attributes: (align
               alt
               archive
               code
               codebase
               datafld
               datasrc
               height
               hspace
               mayscript
               name
               object
               src
               vspace
               width))

(define-tag area
  end-tag?: #f
  attributes: (alt
               coords
               download
               href
               hreflang
               media
               name
               nohref
               ping
               referrerpolicy
               rel
               shape
               tabindex
               target
               type))

(define-tag article)

(define-tag aside)

(define-tag audio
  attributes: (crossorigin
               preload
               src)
  single-attributes: (autoplay
                      controls
                      loop
                      muted))

(define-tag b)

(define-tag base
  allow-event-attributes?: #f
  end-tag?: #f
  attributes: (href
               target))

(define-tag basefont
  attributes: (color
                face
                size))

(define-tag bdi)

(define-tag bdo)

(define-tag big)

(define-tag blockquote
  attributes: (cite))

(define-tag body
  attributes: (alink
               background
               bgcolor
               bottommargin
               leftmargin
               link
               rightmargin
               text
               topmargin
               vlink))

(define-tag br
  end-tag?: #f
  attributes: (clear))

(define-tag button
  attributes: (autocomplete
               form
               formaction
               formenctype
               formmethod
               formtarget
               name
               type
               value)
  single-attributes: (autofocus
                      disabled
                      formnovalidate))

(define-tag canvas
  attributes: (height
               width))

(define-tag caption
  attributes: (align))

(define-tag center)

(define-tag cite)

(define-tag code)

(define-tag col
  end-tag?: #f
  attributes: (align
               bgcolor
               char
               charoff
               span
               valign
               width))

(define-tag colgroup
  attributes: (align
               bgcolor
               char
               charoff
               span
               valign
               width))

(define-tag data
  allow-event-attributes?: #f
  attributes: (value))

(define-tag datalist)

(define-tag dd
  attributes: (nowrap))

(define-tag del
  attributes: (cite
               datetime))

(define-tag details
  single-attributes: (open))

(define-tag dfn)

(define-tag dialog
  single-attributes: (open))

(define-tag dir
  single-attributes: (compact))

(define-tag div)

(define-tag dl)

(define-tag dt)

(define-tag em)

(define-tag embed
  attributes: (height
               src
               type
               width))

(define-tag fieldset
  attributes: (form
               name)
  single-attributes: (disabled))

(define-tag figcaption)

(define-tag figure)

(define-tag font
  attributes: (color
               face
               size))

(define-tag footer)

(define-tag form
  attributes: (accept
               accept-charset
               action
               autocomplete
               enctype
               method
               name
               rel
               target)
  single-attributes: (novalidate))

(define-tag frame
  attributes: (frameborder
               marginheight
               marginwidth
               name
               scrolling
               src)
  single-attributes: (noresize))

(define-tag frameset
  attributes: (cols
               rows))

(define-tag h1)

(define-tag h2)

(define-tag h3)

(define-tag h4)

(define-tag h5)

(define-tag h6)

(define-tag head
  allow-event-attributes?: #f
  attributes: (profile))

(define-tag header)

(define-tag hp1)

(define-tag hp2)

(define-tag hp3)

(define-tag hp4)

(define-tag hp5)

(define-tag hp6)

(define-tag hr
  end-tag?: #f
  attributes: (align
               color
               noshade
               size
               width))

(define-tag html
  allow-event-attributes?: #f
  attributes: (manifest
               version
               xmlns))

(define-tag i)

(define-tag iframe
  attributes: (align
               allow
               allowfullscreen
               allowpaymentrequest
               csp
               frameborder
               height
               importance
               loading
               longdesc
               marginheight
               marginwidth
               name
               referrerpolicy
               sandbox
               scrolling
               src
               srcdoc
               width))

(define-tag img
  end-tag?: #f
  attributes: (align
               alt
               border
               crossorigin
               decoding
               height
               hspace
               importance
               intrinsicsize
               loading
               longdesc
               name
               referrerpolicy
               sizes
               src
               srcset
               usemap
               vspace
               width)
  single-attributes: (ismap))

(define-tag input
  end-tag?: #f
  attributes: (accept
               alt
               autocomplete
               capture
               dirname
               file
               form
               formaction
               formenctype
               formmethod
               formtarget
               height
               list
               max
               maxlength
               min
               minlength
               name
               pattern
               placeholder
               size
               src
               step
               tabindex
               type
               value
               width)
  single-attributes: (autofocus
                      checked
                      disabled
                      formnovalidate
                      multiple
                      readonly
                      required))

(define-tag ins
  attributes: (cite
               datetime))

(define-tag isindex
  attributes: (action
               prompt))

(define-tag kbd)

(define-tag label
  attributes: (for
               form))

(define-tag legend)

(define-tag li
  attributes: (type
               value))

(define-tag link
  end-tag?: #f
  attributes: (as
               charset
               crossorigin
               href
               hreflang
               importance
               integrity
               media
               methods
               prefetch
               referrerpolicy
               rel
               rev
               sizes
               target
               type)
  single-attributes: (disabled))

(define-tag listing)

(define-tag main)

(define-tag map
  attributes: (name))

(define-tag mark)

(define-tag menu
  attributes: (label
               type))

(define-tag meta
  allow-event-attributes?: #f
  end-tag?: #f
  attributes: (charset
               content
               http-equiv
               name
               scheme))

(define-tag meter
  attributes: (form
               high
               low
               max
               min
               optimum
               value))

(define-tag nav)

(define-tag nextid
  attributes: (n))

(define-tag noframes)

(define-tag noscript
  allow-event-attributes?: #f)

(define-tag object
  attributes: (archive
               border
               classid
               codebase
               codetype
               data
               form
               height
               name
               standby
               tabindex
               type
               typemustmatch
               usemap
               width)
  single-attributes: (declare))

(define-tag ol
  attributes: (start
               type)
  single-attributes: (compact
                      reversed))

(define-tag optgroup
  attributes: (label)
  single-attributes: (disabled))

(define-tag option
  attributes: (label
               value)
  single-attributes: (disabled
                      selected))

(define-tag output
  attributes: (for
               form
               name))

(define-tag p)

(define-tag param
  end-tag?: #f
  attributes: (name
               value
               valuetype
               type))

(define-tag picture)

(define-tag plaintext)

(define-tag pre
  attributes: (cols
               width)
  single-attributes: (wrap))

(define-tag progress
  attributes: (max
               value))

(define-tag q
  attributes: (cite))

(define-tag rp)

(define-tag rt)

(define-tag rtc)

(define-tag ruby)

(define-tag s)

(define-tag samp)

(define-tag script
  allow-event-attributes?: #f
  attributes: (charset
               crossorigin
               integrity
               language
               referrerpolicy
               src
               type)
  single-attributes: (async
                      defer
                      nomodule))

(define-tag section)

(define-tag select
  attributes: (autocomplete
               form
               name
               size)
  single-attributes: (autofocus
                      disabled
                      multiple
                      required))

(define-tag small)

(define-tag source
  end-tag?: #f
  attributes: (media
               sizes
               src
               srcset
               type))

(define-tag span)

(define-tag strike)

(define-tag strong)

(define-tag style
  attributes: (media
               type)
  single-attributes: (scoped))

(define-tag sub)

(define-tag summary)

(define-tag sup)

(define-tag table
  attributes: (align
               bgcolor
               border
               cellpadding
               cellspacing
               frame
               rules
               summary
               width))

(define-tag tbody
  attributes: (align
               bgcolor
               char
               charoff
               valign))

(define-tag td
  attributes: (abbr
               align
               axis
               bgcolor
               char
               charoff
               colspan
               headers
               rowspan
               scope
               valign
               width))

(define-tag template
  allow-event-attributes?: #f)

(define-tag textarea
  attributes: (autocomplete
               cols
               form
               maxlength
               minlength
               name
               placeholder
               rows
               wrap)
  single-attributes: (autofocus
                      disabled
                      readonly
                      required))

(define-tag tfoot
  attributes: (align
               bgcolor
               char
               charoff
               valign))

(define-tag th
  attributes: (abbr
               align
               axis
               bgcolor
               char
               charoff
               colspan
               headers
               rowspan
               scope
               valign
               width))

(define-tag thead
  attributes: (align
               bgcolor
               char
               charoff
               valign))

(define-tag time
  attributes: (datetime))

(define-tag title
  allow-event-attributes?: #f)

(define-tag tr
  attributes: (align
               bgcolor
               char
               charoff
               valign))

(define-tag track
  end-tag?: #f
  attributes: (kind
               label
               src
               srclang)
  single-attributes: (default))

(define-tag tt)

(define-tag u)

(define-tag ul
  attributes: (type)
  single-attributes: (compact))

(define-tag var)

(define-tag video
  attributes: (crossorigin
               height
               intrinsicsize
               poster
               preload
               src
               width)
  single-attributes: (autoplay
                      buffered
                      controls
                      loop
                      muted
                      played
                      playsinline))

(define-tag wbr)

(define-tag xmp)

;==============================================================================
