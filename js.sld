(define-labrary (js)
  (export foreign->js
          js->foreign
          scm->js
          js->scm
          document-obj
          null-obj
          undefined-obj
          extract-error
          instanceof
          js-error?
          document.write
          console.log
          document.getElementById
          .querySelector
          .querySelectorAll
          element.firstChild
          element.firstElementChild
          element.parentNode
          document.createElement
          get-new-div-node
          element.getAttribute
          element.hasAttribute
          element.setAttribute
          element.removeAttribute
          element.innerHTML
          element.outerHTML
          element.removeChild
          element.appendChild
          element.insertBefore
          element.replaceChild)

  (include "js/js.scm"))
