;;; js2-externs.el -- JavaScript extern definitions for js2-mode

;; Author:  Steve Yegge (steve.yegge@gmail.com)
;; Keywords:  javascript languages

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Code:

(defvar js2-ecma-262-externs
  (mapcar 'symbol-name
          '(Array
            Boolean
            Date
            Error
            EvalError
            Function
            Infinity
            Math
            NaN
            Number
            Object
            RangeError
            ReferenceError
            RegExp
            String
            SyntaxError
            TypeError
            URIError
            arguments
            decodeURI
            decodeURIComponent
            encodeURI
            encodeURIComponent
            escape
            eval
            isFinite
            isNaN
            parseFloat
            parseInt
            undefined
            unescape))
"Ecma-262 externs.  Included in `js2-externs' by default.")

(defvar js2-browser-externs
  (mapcar 'symbol-name
          '(;; DOM level 1
            Attr
            CDATASection
            CharacterData
            Comment
            DOMException
            DOMImplementation
            Document
            DocumentFragment
            DocumentType
            Element
            Entity
            EntityReference
            ExceptionCode
            NamedNodeMap
            Node
            NodeList
            Notation
            ProcessingInstruction
            Text

            ;; DOM level 2
            HTMLAnchorElement
            HTMLAppletElement
            HTMLAreaElement
            HTMLBRElement
            HTMLBaseElement
            HTMLBaseFontElement
            HTMLBodyElement
            HTMLButtonElement
            HTMLCollection
            HTMLDListElement
            HTMLDirectoryElement
            HTMLDivElement
            HTMLDocument
            HTMLElement
            HTMLFieldSetElement
            HTMLFontElement
            HTMLFormElement
            HTMLFrameElement
            HTMLFrameSetElement
            HTMLHRElement
            HTMLHeadElement
            HTMLHeadingElement
            HTMLHtmlElement
            HTMLIFrameElement
            HTMLImageElement
            HTMLInputElement
            HTMLIsIndexElement
            HTMLLIElement
            HTMLLabelElement
            HTMLLegendElement
            HTMLLinkElement
            HTMLMapElement
            HTMLMenuElement
            HTMLMetaElement
            HTMLModElement
            HTMLOListElement
            HTMLObjectElement
            HTMLOptGroupElement
            HTMLOptionElement
            HTMLOptionsCollection
            HTMLParagraphElement
            HTMLParamElement
            HTMLPreElement
            HTMLQuoteElement
            HTMLScriptElement
            HTMLSelectElement
            HTMLStyleElement
            HTMLTableCaptionElement
            HTMLTableCellElement
            HTMLTableColElement
            HTMLTableElement
            HTMLTableRowElement
            HTMLTableSectionElement
            HTMLTextAreaElement
            HTMLTitleElement
            HTMLUListElement

            ;; DOM level 3
            DOMConfiguration
            DOMError
            DOMException
            DOMImplementationList
            DOMImplementationSource
            DOMLocator
            DOMStringList
            NameList
            TypeInfo
            UserDataHandler

            ;; Window
            alert
            confirm
            document
            java
            navigator
            prompt
            screen
            self
            top

            ;; W3C CSS
            CSSCharsetRule
            CSSFontFace
            CSSFontFaceRule
            CSSImportRule
            CSSMediaRule
            CSSPageRule
            CSSPrimitiveValue
            CSSProperties
            CSSRule
            CSSRuleList
            CSSStyleDeclaration
            CSSStyleRule
            CSSStyleSheet
            CSSValue
            CSSValueList
            Counter
            DOMImplementationCSS
            DocumentCSS
            DocumentStyle
            ElementCSSInlineStyle
            LinkStyle
            MediaList
            RGBColor
            Rect
            StyleSheet
            StyleSheetList
            ViewCSS
            
            ;; W3C Event
            EventListener
            EventTarget
            Event
            DocumentEvent
            UIEvent
            MouseEvent
            MutationEvent
            KeyboardEvent

            ;; W3C Range
            DocumentRange
            Range
            RangeException

            ;; W3C XML
            XPathResult
            XMLHttpRequest
            ))
  "Browser externs.
You can cause these to be included or excluded with the custom
variable `js2-include-browser-externs'.")

(defvar js2-rhino-externs
  (mapcar 'symbol-name
          '(Packages
            importClass
            importPackage
            com
            org
            java

            ;; Global object (shell) externs
            defineClass
            deserialize
            doctest
            gc
            help
            load
            loadClass
            print
            quit
            readFile
            readUrl
            runCommand
            seal
            serialize
            spawn
            sync
            toint32
            version))
  "Mozilla Rhino externs.
Set `js2-include-rhino-externs' to t to include them.")

(defvar js2-gears-externs
  (mapcar 'symbol-name
          '(
            ;; finish me!
            ))
  "Google Gears externs.
Set `js2-include-gears-externs' to t to include them.")

(provide 'js2-externs)

;;; js2-externs.el ends here
