;;; js2-parse.el --- JavaScript parser

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

;; Commentary:

;; This is based on Rhino's parser and tries to follow its code
;; structure as closely as practical, so that changes to the Rhino
;; parser can easily be propagated into this code.  However, Rhino
;; does not currently generate a usable AST representation, at least
;; from an IDE perspective, so we build our own more suitable AST.

;; The AST node structures are defined in `js2-ast.el'.
;; Every parser function that creates and returns an AST node has
;; the following responsibilities:

;;   1) set the node start to the absolute buffer start position
;;   2) set the node length to include any closing chars (RC, SEMI)
;;   3) fix up any child-node starts to be relative to this node
;;   4) set any field positions (e.g. keywords) relative to this node
;;   5) report any child nodes with `js2-node-add-children'
;;      (note that this call fixes up start positions by default)

;; The resulting AST has all node start positions relative to the
;; parent nodes; only the root has an absolute start position.

;; Note: fontification is done inline while parsing.  It used to be
;; done in a second pass over the AST, but doing it inline is about
;; twice as fast.  Most of the fontification happens when tokens are
;; scanned, and the parser has a few spots that perform extra
;; fontification.  In addition to speed, a second benefit of inline
;; parsing is that if a long parse is interrupted, everything parsed
;; so far is still fontified.

;; The editing mode that uses this parser, `js2-mode', directs the
;; parser to check periodically for user input.  If user input
;; arrives, the parse is abandoned, except for the highlighting that
;; has occurred so far, and a re-parse is rescheduled for when Emacs
;; becomes idle again.  This works pretty well, but could be better.
;; In particular, when the user input has not resulted in changes to
;; the buffer (for instance, navigation input), the parse tree built
;; so far should not be discarded, and the parse should continue where
;; it left off.  It will be some work to create what amounts to a
;; continuation, but it should not be unreasonably difficult.

;; TODO:
;; - make non-editing input restart parse at previous continuation
;; - in Eclipse, sibling nodes never overlap start/end ranges
;;   - for getters, prop name and function nodes overlap
;;   - should write a debug tree visitor to look for overlaps
;; - mark array and object literals as "destructuring" (node prop?)
;;   so we can syntax-highlight them properly.
;; - figure out a way not to store value in string/name nodes
;;   - needs a solution for synthetic nodes

;;; Code

(eval-and-compile
  (require 'cl))  ; for delete-if

(require 'js2-util)
(require 'js2-ast)       ; node classes
(require 'js2-scan)      ; tokenizer
(require 'js2-highlight) ; syntax coloring
(require 'js2-browse)    ; imenu support

(defconst js2-version "1.7.0"
  "Version of JavaScript supported, plus minor js2 version.")

(defsubst js2-record-comment ()
  (push (make-js2-comment-node :len (- js2-token-end js2-token-beg)
                               :format js2-ts-comment-type)
        js2-scanned-comments)
  (when js2-parse-ide-mode
    (js2-set-face js2-token-beg js2-token-end 'font-lock-comment-face 'record)
    (when (memq js2-ts-comment-type '(html preprocessor))
      ;; Tell cc-engine the bounds of the comment.
      (put-text-property js2-token-beg (1- js2-token-end) 'c-in-sws t))))

(defsubst js2-node-end (n)
  "Computes the absolute end of node N.
Use with caution!  Assumes `js2-node-pos' is -absolute-, which
is only true until the node is added to its parent."
  (+ (js2-node-pos n)
     (js2-node-len n)))

;; This function is called depressingly often, so it should be fast.
;; Most of the time it's looking at the same token it peeked before.
(defsubst js2-peek-token ()
  "Returns the next token without consuming it.
If previous token was consumed, calls scanner to get new token.
If previous token was -not- consumed, returns it (idempotent).

This function will not return a newline (js2-EOL) - instead, it
gobbles newlines until it finds a non-newline token, and flags
that token as appearing just after a newline.

This function will also not return a js2-COMMENT.  Instead, it
records comments found in `js2-scanned-comments'.  If the token
returned by this function immediately follows a jsdoc comment,
the token is flagged as such.

Note that this function always returned the un-flagged token!
The flags, if any, are saved in `js2-current-flagged-token'."
  (if (/= js2-current-flagged-token js2-EOF) ; last token not consumed
      js2-current-token  ; most common case - return already-peeked token
    (let ((tt (js2-get-token))          ; call scanner
          saw-eol
          flags
          face)
      ;; process comments and whitespace
      (while (or (eq tt js2-EOL)
                 (eq tt js2-COMMENT))
        (if (eq tt js2-EOL)
            (setq saw-eol t)
          (setq saw-eol nil)
          (if js2-record-comments
              (js2-record-comment)))
        (setq tt (js2-get-token)))      ; call scanner

      (setq js2-current-token tt
            js2-current-flagged-token (if saw-eol
                                          (set-flag tt js2-ti-after-eol)
                                        tt))
      ;; perform lexical fontification as soon as token is scanned
      (when js2-parse-ide-mode
        (cond
         ((minusp tt)
          (js2-set-face js2-token-beg js2-token-end 'js2-error-face 'record))
         ((setq face (aref js2-kwd-tokens tt))
          (js2-set-face js2-token-beg js2-token-end face 'record))
         ((and (eq tt js2-NAME)
               (equal js2-ts-string "undefined"))
          (js2-set-face js2-token-beg js2-token-end 'font-lock-constant-face 'record))))
      tt)))  ; return unflagged token

(defsubst js2-peek-flagged-token ()
  "Returns the current token along with any flags set for it."
  (js2-peek-token)
  js2-current-flagged-token)

(defsubst js2-consume-token ()
  (setq js2-current-flagged-token js2-EOF))

(defsubst js2-next-token ()
  (prog1
      (js2-peek-token)
    (js2-consume-token)))

(defsubst js2-next-flagged-token ()
  (js2-peek-token)
  (prog1 js2-current-flagged-token
    (js2-consume-token)))

(defsubst js2-match-token (match)
  "Consume and return t if next token matches MATCH, a bytecode.
Returns nil and consumes nothing if MATCH is not the next token."
  (if (neq (js2-peek-token) match)
      nil
    (js2-consume-token)
    t))

(defsubst js2-valid-prop-name-token (tt)
  (or (eq tt js2-NAME)
      (and js2-allow-keywords-as-property-names
           (plusp tt)
           (aref js2-kwd-tokens tt))))

(defsubst js2-match-prop-name ()
  "Consume token and return t if next token is a valid property name.
It's valid if it's a js2-NAME, or `js2-allow-keywords-as-property-names'
is non-nil and it's a keyword token."
  (if (js2-valid-prop-name-token (js2-peek-token))
      (progn
        (js2-consume-token)
        t)
    nil))

(defsubst js2-must-match-prop-name (msg-id &optional pos len)
  (if (js2-match-prop-name)
      t
    (js2-report-error msg-id pos len)
    nil))

(defsubst js2-peek-token-or-eol ()
  "Return js2-EOL if the current token immediately follows a newline.
Else returns the current token.  Used in situations where we don't
consider certain token types valid if they are preceded by a newline.
One example is the postfix ++ or -- operator, which has to be on the
same line as its operand."
  (let ((tt (js2-peek-token)))
    ;; Check for last peeked token flags
    (if (/= 0 (logand js2-current-flagged-token js2-ti-after-eol))
        js2-EOL
      tt)))

(defsubst js2-set-check-for-label ()
  (if (/= (logand js2-current-flagged-token js2-clear-ti-mask) js2-NAME)
      (js2-code-bug))
  (setq js2-current-flagged-token
        (set-flag js2-current-flagged-token js2-ti-check-label)))

(defsubst js2-must-match (token msg-id &optional pos len)
  "Match next token to token code TOKEN, or record a syntax error.
MSG-ID is the error message to report if the match fails.
Returns t on match, nil if no match."
  (if (js2-match-token token)
      t
    (js2-report-error msg-id nil pos len)
    nil))

(defsubst js2-set-requires-activation ()
  (if (js2-function-node-p js2-current-script-or-fn)
      (setf (js2-function-node-needs-activation js2-current-script-or-fn) t)))

(defsubst js2-set-is-generator ()
  (if (js2-function-node-p js2-current-script-or-fn)
      (setf (js2-function-node-is-generator js2-current-script-or-fn) t)))

(defsubst js2-must-have-xml ()
  (unless js2-compiler-xml-available
    (js2-report-error "msg.XML.not.available")))

(defsubst js2-inside-function ()
  (/= 0 js2-nesting-of-function))

(defsubst js2-push-scope (node)
  "Push `js2-scope' associated with NODE, a `js2-node'.
NODE must either be a scope node supported by `js2-node-scope'
or a `js2-scope' itself."
  (unless (js2-scope-p node)
    (setq node (js2-node-scope node)))
  (if (js2-scope-parent node)
      (js2-code-bug))
  (if (eq js2-current-scope node)
      (js2-code-bug))
  (setf (js2-scope-parent node) js2-current-scope
        js2-current-scope node))

(defsubst js2-pop-scope ()
  (setq js2-current-scope
        (js2-scope-parent js2-current-scope)))

(defsubst js2-enter-loop (loop-node)
  (push loop-node js2-loop-set)
  (push loop-node js2-loop-and-switch-set)
  (setf (js2-loop-node-scope loop-node)
        (make-js2-scope :type js2-LOOP
                        :ast-node loop-node))
  (js2-push-scope loop-node))

(defsubst js2-exit-loop ()
  (setq js2-loop-set (cdr js2-loop-set))
  (setq js2-loop-and-switch-set (cdr js2-loop-and-switch-set))
  (js2-pop-scope))

(defsubst js2-enter-switch (switch-node)
  (push switch-node js2-loop-and-switch-set))

(defsubst js2-exit-switch ()
  (pop js2-loop-and-switch-set))

(defun js2-parse (&optional buf cb)
  "Tells the js2 parser to parse a region of JavaScript.

BUF is a buffer or buffer name containing the code to parse.
Call `narrow-to-region' first to parse only part of the buffer.

The returned AST root node is given some additional properties:
  `node-count' - total number of nodes in the AST
  `buffer' - BUF.  The buffer it refers to may change or be killed,
             so the value is not necessarily reliable.

An optional callback CB can be specified to report parsing
progress.  If `(functionp CB)' returns t, it will be called with
the current line number once before parsing begins, then again
each time the lexer reaches a new line number.

CB can also be a list of the form `(symbol cb ...)' to specify
multiple callbacks with different criteria.  Each symbol is a
criterion keyword, and the following element is the callback to
call

  :line  - called whenever the line number changes
  :token - called for each new token consumed

The list of criteria could be extended to include entering or
leaving a statement, an expression, or a function definition."
  (if (and cb (not (functionp cb)))
      (error "criteria callbacks not yet implemented"))
  (let ((inhibit-point-motion-hooks t)
        (js2-compiler-xml-available (>= js2-language-version 160))
        ;; This is a recursive-descent parser, so give it a big stack.
        (max-lisp-eval-depth (max max-lisp-eval-depth 3000))
        (max-specpdl-size (max max-specpdl-size 3000))
        (case-fold-search nil)
        ast)
    (or buf (setq buf (current-buffer)))
    (save-excursion
      (set-buffer buf)
      (setq js2-scanned-comments nil
            js2-parsed-errors nil
            js2-parsed-warnings nil
            js2-imenu-recorder nil
            js2-imenu-function-map nil)
      (js2-init-scanner)
      (setq ast (js2-with-unmodifying-text-property-changes
                  (js2-do-parse)))
      (unless js2-ts-hit-eof
        (js2-report-error "msg.syntax"))
      (setf (js2-ast-root-errors ast) js2-parsed-errors
            (js2-ast-root-warnings ast) js2-parsed-warnings)
      (run-hooks 'js2-parse-finished-hook)
      ast)))

;; Corresponds to Rhino's Parser.parse() method.
(defun js2-do-parse ()
  "Parse current buffer starting from current point.
Scanner should be initialized."
  (let ((pos js2-ts-cursor)
        root
        n
        tt
        msg
        (end js2-ts-cursor)) ; in case file is empty
    ;; initialize buffer-local vars
    (setf root (make-js2-ast-root :buffer (buffer-name)
                                  :pos js2-ts-cursor)
          js2-current-script-or-fn root
          js2-current-scope (js2-node-set-scope
                             root
                             (make-js2-scope :type js2-SCRIPT
                                             :ast-node root))
          js2-current-flagged-token js2-EOF
          js2-nesting-of-function 0
          js2-labeled-stmt nil
          js2-recorded-assignments nil)
    (catch 'break
      (while t
        (setq tt (js2-peek-token))
        (if (= tt js2-EOF)
            (throw 'break nil))
        (if (eq tt js2-FUNCTION)
            (progn
              (js2-consume-token)
              (setq n (js2-parse-function
                       (if js2-called-by-compile-function
                           'FUNCTION_EXPRESSION
                         'FUNCTION_STATEMENT)))
              (js2-record-imenu-functions n))
          ;; not a function - parse a statement
          (setq n (js2-parse-statement)))
        ;; add to script
        (setq end (js2-node-end n))
        (js2-block-node-push root n)))
    (setf (js2-node-pos root) pos
          (js2-node-len root) (- end pos))
    (js2-highlight-undeclared-vars)
    ;; add comments to root in lexical order
    (when js2-scanned-comments
      ;; if we find a comment beyond end of normal kids, use its end
      (setq end (max end (js2-node-end (first js2-scanned-comments))))
      (dolist (comment js2-scanned-comments)
        (push comment (js2-ast-root-comments root))
        (js2-node-add-children root comment))
      (setf (js2-node-len root) (- end pos)))
    root))

(defun js2-parse-function-body ()
  (incf js2-nesting-of-function)
  (let ((pn (make-js2-block-node))  ; starts at LC position
        n
        tt)
    (unwind-protect
        (catch 'break-loop
          (while t
            (setq tt (js2-peek-token))
            (cond
             ((or (eq tt js2-ERROR)
                  (eq tt js2-EOF)
                  (eq tt js2-RC))
               (throw 'break-loop nil))
             ((eq tt js2-FUNCTION)
              (js2-consume-token)
              (setq n (js2-parse-function 'FUNCTION_STATEMENT)))
             (t
              (setq n (js2-parse-statement))))
            (js2-block-node-push pn n)))
      (decf js2-nesting-of-function))
    pn))

(defun js2-function-parser ()
  (js2-consume-token)
  (js2-parse-function 'FUNCTION_EXPRESSION_STATEMENT))

;; The function parser is simpler than Rhino's.  It doesn't define
;; symbols, doesn't support the experimental member-expr syntax,
;; doesn't do tree-rewriting for destructuring assignment, and doesn't
;; keep track of dynamic scope.  That stuff can (and should) be done
;; in a separate codegen pass.  Parsing is already hairy enough.

(defun js2-parse-function (function-type)
  "Function parser.  FUNCTION-TYPE is a symbol."
  (let ((pos js2-token-beg)
        name
        fn-node
        function-index
        saved-script-or-fn
        saved-current-scope
        saved-nesting-of-with
        saved-label-set
        saved-loop-set
        saved-loop-and-switch-set
        saved-end-flags
        body
        n
        n2
        pos2
        len
        tt
        params
        scope
        lp)
    (when (js2-match-token js2-NAME)
      (js2-set-face js2-token-beg js2-token-end 'font-lock-function-name-face 'record)
      (setq name (make-js2-name-node :pos js2-token-beg
                                     :len (- js2-token-end js2-token-beg))))
    (if (js2-must-match js2-LP "msg.no.paren.parms")
        (setq lp js2-token-beg))

    (setf fn-node (make-js2-function-node :pos pos
                                          :name name
                                          :form function-type
                                          :lp (if lp (- lp pos))))

    (if (and name (neq function-type 'FUNCTION_EXPRESSION))
        ;; Function statements define a symbol in the enclosing scope
        (js2-define-symbol js2-FUNCTION (js2-name-node-name name) fn-node))

    (setf scope (make-js2-scope :type js2-FUNCTION
                                :ast-node fn-node)
          (js2-function-node-scope fn-node) scope
          saved-script-or-fn         js2-current-script-or-fn
          js2-current-script-or-fn   fn-node
          saved-current-scope        js2-current-scope
          js2-current-scope          scope
          saved-nesting-of-with      js2-nesting-of-with
          js2-nesting-of-with        0
          saved-label-set            js2-label-set
          js2-label-set              nil
          saved-loop-set             js2-loop-set
          js2-loop-set               nil
          saved-loop-and-switch-set  js2-loop-and-switch-set
          js2-loop-and-switch-set    nil
          saved-end-flags            js2-end-flags
          js2-end-flags              0)
    (unwind-protect
        (progn
          ;; parse function parameter list
          (if (js2-match-token js2-RP)
              (setf (js2-function-node-rp fn-node) (- js2-token-beg pos))
            (loop
             do
             (setq tt (js2-peek-token))
             ;; Rhino does some tree rewriting to support destructuring
             ;; assignment; we'll do that later during codegen, and just
             ;; create array-literal or object-literal nodes for any
             ;; destructuring-assignment parameters we find.
             (cond
              ((eq tt js2-LB)
               (js2-consume-token)
               (push (js2-parse-array-literal) params))
              ((eq tt js2-LC)
               (js2-consume-token)
               (push (js2-parse-object-literal) params))
              (t
               (js2-must-match js2-NAME "msg.no.parm")
               (js2-set-face js2-token-beg js2-token-end
                             'js2-function-param-face 'record)
               (setq pos2 js2-token-beg
                     len (- js2-token-end js2-token-beg)
                     n (make-js2-name-node :pos pos2
                                           :len len)
                     n2 (make-js2-function-arg-node :pos pos2
                                                    :len len
                                                    :value n))
               (js2-define-symbol js2-LP js2-ts-string n2)
               (js2-node-add-children n2 n)
               (push n2 params)))
             while
             (js2-match-token js2-COMMA))
            (if (js2-must-match js2-RP "msg.no.paren.after.parms")
                (setf (js2-function-node-rp fn-node) (- js2-token-beg pos))))
          (dolist (p params)
            (js2-node-add-children fn-node p)
            (push p (js2-function-node-params fn-node)))
          ;; parse function body
          (js2-must-match js2-LC "msg.no.brace.body")
          (setf pos2 js2-token-beg
                body (js2-parse-function-body)
                (js2-node-pos body) pos2
                (js2-node-len body) (- js2-ts-cursor pos2)
                (js2-function-node-body fn-node) body)
          (js2-node-add-children fn-node body)
          (js2-must-match js2-RC "msg.no.brace.after.body" pos2)

          (if (and name
                   (eq function-type 'FUNCTION_EXPRESSION)
                   (null (js2-scope-get-symbol js2-current-scope
                                               (js2-name-node-name name))))
              ;; Function expressions define a name only in the body of the
              ;; function, and only if not hidden by a parameter name
              (js2-define-symbol js2-FUNCTION
                                 (js2-name-node-name name)
                                 fn-node))
          (if (and name
                   (eq function-type 'FUNCTION_EXPRESSION_STATEMENT))
              (js2-record-imenu-functions fn-node)))

      ;; finally
      (setq js2-end-flags            saved-end-flags
            js2-loop-and-switch-set  saved-loop-and-switch-set
            js2-loop-set             saved-loop-set
            js2-label-set            saved-label-set
            js2-nesting-of-with      saved-nesting-of-with
            js2-current-script-or-fn saved-script-or-fn
            js2-current-scope        saved-current-scope))

    (setf (js2-node-len fn-node) (- js2-ts-cursor pos))
    (js2-node-add-children fn-node name)

    ;; Rhino doesn't do this, but we need it for finding undeclared vars.
    ;; We wait until after parsing the function to set its parent scope,
    ;; since `js2-define-symbol' needs the defining-scope check to stop
    ;; at the function boundary when checking for redeclarations.
    (setf (js2-scope-parent scope) js2-current-scope)

    fn-node))

(defun js2-parse-statements (&optional parent)
  "Parse a statement list.  Last token consumed must be js2-LC.

PARENT can be a `js2-block-node', in which case the statements are
appended to PARENT.  Otherwise a new `js2-block-node' is created
and returned.

This function does not match the closing js2-RC: the caller
matches the RC so it can provide a suitable error message if not
matched.  This means it's up to the caller to set the length of
the node to include the closing RC.  The node start pos is set to
the absolute buffer start position, and the caller should fix it
up to be relative to the parent node.  All children of this block
node are given relative start positions and correct lengths."
  (let ((pn (or (and parent
                     (prog1 parent
                       (setf (js2-node-pos parent) js2-token-beg)))
                (make-js2-block-node)))
        stmt
        tt)
      (while (and (> (setq tt (js2-peek-token)) js2-EOF)
                  (neq tt js2-RC))
        (setq stmt (js2-parse-statement))
        (js2-block-node-push pn stmt))
      pn))

(defun js2-parse-condition ()
  "Parse a parenthesized boolean expression, e.g. in an if- or while-stmt.
The parens are discarded and the expression node is returned.
The `pos' field of the return value is set to an absolute position
that must be fixed up by the caller.
Return value is a list (EXPR LP RP), with absolute paren positions."
  (let (pn lp rp)
    (if (js2-must-match js2-LP "msg.no.paren.cond")
        (setq lp js2-token-beg))
    (setq pn (js2-parse-expr))
    (if (js2-must-match js2-RP "msg.no.paren.after.cond")
        (setq rp js2-token-beg))
    ;; Report strict warning on code like "if (a = 7) ..."
    (if (and (boundp 'js2-strict-cond-assign-warning)
             js2-strict-cond-assign-warning
             (js2-assign-node-p pn))
        (js2-add-strict-warning "msg.equal.as.assign" nil
                                (js2-node-pos pn)
                                (+ (js2-node-pos pn)
                                   (js2-node-len pn))))
    (list pn lp rp)))

(defun js2-parse-statement ()
  (let ((js2-labeled-stmt nil)
        tt pn beg end)
    (when js2-parse-interruptable-p
      (if (zerop (% (incf js2-parse-stmt-count)
                    js2-statements-per-pause))
          (if (input-pending-p)
              (throw 'interrupted t))))
    (setq pn (js2-statement-helper))
    (unless (js2-node-has-side-effects pn)
      (setq end (js2-node-end pn))
      (save-excursion
        (goto-char end)
        (setq beg (max (js2-node-pos pn) (point-at-bol))))
      (js2-add-strict-warning "msg.no.side.effects" nil beg end))
    pn))

;; These correspond to the switch cases in Parser.statementHelper
(defconst js2-parsers
  (let ((parsers (make-vector js2-num-tokens
                                #'js2-parse-expr-stmt)))
    (aset parsers js2-BREAK     #'js2-parse-break)
    (aset parsers js2-CONST     #'js2-parse-const-var)
    (aset parsers js2-CONTINUE  #'js2-parse-continue)
    (aset parsers js2-DEBUGGER  #'js2-parse-debugger)
    (aset parsers js2-DEFAULT   #'js2-parse-default-xml-namespace)
    (aset parsers js2-DO        #'js2-parse-do)
    (aset parsers js2-FOR       #'js2-parse-for)
    (aset parsers js2-FUNCTION  #'js2-function-parser)
    (aset parsers js2-IF        #'js2-parse-if)
    (aset parsers js2-LC        #'js2-parse-block)
    (aset parsers js2-LET       #'js2-parse-let)
    (aset parsers js2-NAME      #'js2-parse-name)
    (aset parsers js2-RETURN    #'js2-parse-ret-yield)
    (aset parsers js2-SEMI      #'js2-parse-semi)
    (aset parsers js2-SWITCH    #'js2-parse-switch)
    (aset parsers js2-THROW     #'js2-parse-throw)
    (aset parsers js2-TRY       #'js2-parse-try)
    (aset parsers js2-VAR       #'js2-parse-const-var)
    (aset parsers js2-WHILE     #'js2-parse-while)
    (aset parsers js2-WITH      #'js2-parse-with)
    (aset parsers js2-YIELD     #'js2-parse-ret-yield)
    parsers)
  "A vector mapping token types to parser functions.")

(defsubst js2-parse-warn-missing-semi (beg end)
  (and js2-mode-show-strict-warnings
       js2-strict-missing-semi-warning
       (js2-add-strict-warning
        "msg.missing.semi" nil
        ;; back up to beginning of statement or line
        (max beg (save-excursion
                   (goto-char end)
                   (point-at-bol)))
        end)))

(defconst js2-no-semi-insertion
  (list js2-IF
        js2-SWITCH
        js2-WHILE
        js2-DO
        js2-FOR
        js2-TRY
        js2-WITH
        js2-LC
        js2-ERROR
        js2-SEMI
        js2-FUNCTION)
  "List of tokens that don't do automatic semicolon insertion.")

(defun js2-statement-helper ()
  (let* ((tt (js2-peek-token))
         (first-tt tt)
         (parser (if (eq tt js2-ERROR)
                     #'js2-parse-semi
                   (aref js2-parsers tt)))
         pn
         (beg js2-token-beg)
         tt-flagged)

    (setq pn (funcall parser)
          tt-flagged (js2-peek-flagged-token)
          tt (logand tt-flagged js2-clear-ti-mask))

    ;; Don't do auto semi insertion for certain statement types.
    (unless (or (memq first-tt js2-no-semi-insertion)
                ;; check for labeled statement
                (js2-labeled-stmt-node-p pn))
      (cond
       ((eq tt js2-SEMI)
        ;; Consume ';' as a part of expression
        (js2-consume-token)
        ;; extend the node bounds to include the semicolon.
        (setf (js2-node-len pn) (- js2-token-end beg)))
       ((memq tt (list js2-ERROR js2-EOF js2-RC))
        ;; Autoinsert ;
        (js2-parse-warn-missing-semi beg (js2-node-end pn)))
       (t
        (if (zerop (logand tt-flagged js2-ti-after-eol))
            ;; Report error if no EOL or autoinsert ';' otherwise
            (js2-report-error "msg.no.semi.stmt")
          (js2-parse-warn-missing-semi beg (js2-node-end pn))))))
    pn))

(defun js2-parse-if ()
  "Parser for if-statement.  Last matched token must be js2-IF."
  (let ((pos js2-token-beg)
        cond
        if-true
        if-false
        else-pos
        end
        pn)
    (js2-consume-token)
    (setq cond (js2-parse-condition)
          if-true (js2-parse-statement)
          if-false (if (js2-match-token js2-ELSE)
                       (progn
                         (setq else-pos (- js2-token-beg pos))
                         (js2-parse-statement)))
          end (js2-node-end (or if-false if-true))
          pn (make-js2-if-node :pos pos
                               :len (- end pos)
                               :condition (car cond)
                               :then-part if-true
                               :else-part if-false
                               :else-pos else-pos
                               :lp (js2-relpos (second cond) pos)
                               :rp (js2-relpos (third cond) pos)))
    (js2-node-add-children pn (car cond) if-true if-false)
    pn))

(defun js2-parse-switch ()
  "Parser for if-statement.  Last matched token must be js2-SWITCH."
  (let ((pos js2-token-beg)
        tt
        pn
        discriminant
        has-default
        case-expr
        case-node
        case-pos
        cases
        stmt
        lp
        rp)
    (js2-consume-token)
    (if (js2-must-match js2-LP "msg.no.paren.switch")
        (setq lp js2-token-beg))
    (setq discriminant (js2-parse-expr)
          pn (make-js2-switch-node :discriminant discriminant
                                   :pos pos
                                   :lp (js2-relpos lp pos)))
    (js2-node-add-children pn discriminant)
    (js2-enter-switch pn)
    (unwind-protect
        (progn
          (if (js2-must-match js2-RP "msg.no.paren.after.switch")
              (setf (js2-switch-node-rp pn) (- js2-token-beg pos)))
          (js2-must-match js2-LC "msg.no.brace.switch")
          (catch 'break
            (while t
              (setq tt (js2-next-token)
                    case-pos js2-token-beg)
              (cond
               ((eq tt js2-RC)
                (setf (js2-node-len pn) (- js2-token-end pos))
                (throw 'break nil))  ; done

               ((eq tt js2-CASE)
                (setq case-expr (js2-parse-expr))
                (js2-must-match js2-COLON "msg.no.colon.case"))

               ((eq tt js2-DEFAULT)
                (if has-default
                    (js2-report-error "msg.double.switch.default"))
                (setq has-default t
                      case-expr nil)
                (js2-must-match js2-COLON "msg.no.colon.case"))

               (t
                (js2-report-error "msg.bad.switch")
                (throw 'break nil)))

              (setq case-node (make-js2-case-node :pos case-pos
                                                  :expr case-expr))
              (js2-node-add-children case-node case-expr)
              (setf (js2-node-len case-node) (- js2-token-end case-pos))
              (while (and (neq (setq tt (js2-peek-token)) js2-RC)
                          (neq tt js2-CASE)
                          (neq tt js2-DEFAULT)
                          (neq tt js2-EOF))
                (setf stmt (js2-parse-statement)
                      (js2-node-len case-node) (- (js2-node-end stmt) case-pos))
                (js2-block-node-push case-node stmt))
              (push case-node cases)))
          (dolist (kid cases)
            (js2-node-add-children pn kid)
            (push kid (js2-switch-node-cases pn)))
          pn)  ; return value
      (js2-exit-switch))))

(defun js2-parse-while ()
  "Parser for while-statement.  Last matched token must be js2-WHILE."
  (let ((pos js2-token-beg)
        (pn (make-js2-while-node))
        cond
        body)
    (js2-consume-token)
    (js2-enter-loop pn)
    (unwind-protect
        (progn
          (setf cond (js2-parse-condition)
                (js2-while-node-condition pn) (car cond)
                body (js2-parse-statement)
                (js2-while-node-body pn) body
                (js2-node-len pn) (- (js2-node-end body) pos)
                (js2-while-node-lp pn) (js2-relpos (second cond) pos)
                (js2-while-node-rp pn) (js2-relpos (third cond) pos))
          (js2-node-add-children pn body (car cond)))
      (js2-exit-loop))
    pn))

(defun js2-parse-do ()
  "Parser for do-statement.  Last matched token must be js2-DO."
  (let ((pos js2-token-beg)
        (pn (make-js2-do-node))
        cond
        body
        end)
    (js2-consume-token)
    (js2-enter-loop pn)
    (unwind-protect
        (progn
          (setq body (js2-parse-statement))
          (js2-must-match js2-WHILE "msg.no.while.do")
          (setf (js2-do-node-while-pos pn) (- js2-token-beg pos)
                cond (js2-parse-condition)
                (js2-do-node-condition pn) (car cond)
                (js2-do-node-body pn) body
                end js2-ts-cursor
                (js2-do-node-lp pn) (js2-relpos (second cond) pos)
                (js2-do-node-rp pn) (js2-relpos (third cond) pos))
          (js2-node-add-children pn (car cond) body))
      (js2-exit-loop))
    ;; Always auto-insert semicolon to follow SpiderMonkey:
    ;; It is required by ECMAScript but is ignored by the rest of
    ;; world, see bug 238945
    (if (js2-match-token js2-SEMI)
        (setq end js2-ts-cursor))
    (setf (js2-node-len pn) (- end pos))
    pn))

(defun js2-parse-for ()
  "Parser for for-statement.  Last matched token must be js2-FOR.
Parses for, for-in, and for each-in statements."
  (let ((for-pos js2-token-beg)
        let-pos
        pn
        is-for-each
        is-for-in
        in-pos
        each-pos
        init  ; Node init is also foo in 'foo in object'
        cond  ; Node cond is also object in 'foo in object'
        incr  ; 3rd section of for-loop initializer
        body
        end
        tt
        expr
        lp
        rp)
    (js2-consume-token)
    ;; See if this is a for each () instead of just a for ()
    (when (js2-match-token js2-NAME)
      (if (string= "each" js2-ts-string)
          (progn
            (setq is-for-each t
                  each-pos (- js2-token-beg for-pos)) ; relative
            (js2-set-face js2-token-beg js2-token-end
                          'font-lock-keyword-face 'record))
        (js2-report-error "msg.no.paren.for")))

    (if (js2-must-match js2-LP "msg.no.paren.for")
        (setq lp (- js2-token-beg for-pos)))
    (setq tt (js2-peek-token))

    ;; parse init clause
    (let ((js2-in-for-init t))  ; set as dynamic variable
      (if (eq tt js2-SEMI)
          (setq init (make-js2-empty-expr-node))
        (if (or (eq tt js2-VAR) (eq tt js2-LET))
            (progn
              (js2-consume-token)
              (setq let-pos js2-token-beg
                    expr (js2-parse-variables tt))
              (if (eq tt js2-VAR)
                  (setq init expr)
                (prog1
                    (setq init
                          (make-js2-let-expr-node :pos let-pos
                                                  :len (- (js2-node-end expr)
                                                          let-pos)
                                                  :vars expr))
                  (js2-node-add-children init expr))))
          (setq init (js2-parse-expr)))))

    (if (js2-match-token js2-IN)
        ;; 'cond' is the object over which we're iterating
        (setq is-for-in t
              in-pos (- js2-token-beg for-pos)
              cond (js2-parse-expr))
      ;; else ordinary for loop - parse cond and incr
      (js2-must-match js2-SEMI "msg.no.semi.for")
      (setq cond (if (eq (js2-peek-token) js2-SEMI)
                     (make-js2-empty-expr-node) ; no loop condition
                   (js2-parse-expr)))
      (js2-must-match js2-SEMI "msg.no.semi.for.cond")
      (setq incr (if (eq (js2-peek-token) js2-RP)
                     (make-js2-empty-expr-node)
                   (js2-parse-expr))))

    (if (js2-must-match js2-RP "msg.no.paren.for.ctrl")
        (setq rp (- js2-token-beg for-pos)))
    (if (not is-for-in)
        (setq pn (make-js2-for-node :init init
                                    :condition cond
                                    :update incr
                                    :lp lp
                                    :rp rp))
      ;; cond could be null if 'in obj' got eaten by the init node.
      (when (null cond)
        (setq cond (js2-infix-node-right init)
              init (js2-infix-node-left init)))

      ;; TODO:  fix let-node and then check kids for it here
      (if (and (eq (setq tt (js2-node-type init)) js2-VAR)
               (> (length (js2-var-decl-node-kids init)) 1))
          (js2-report-error "msg.mult.index"))

      (setq pn (make-js2-for-in-node :iterator init
                                     :object cond
                                     :in-pos in-pos
                                     :foreach-p is-for-each
                                     :each-pos each-pos
                                     :lp lp
                                     :rp rp)))
    (unwind-protect
        (progn
          (js2-enter-loop pn)
          ;; We have to parse the body -after- creating the loop node,
          ;; so that the loop node appears in the js2-loop-set, allowing
          ;; break/continue statements to find the enclosing loop.
          (setf body (js2-parse-statement)
                (js2-loop-node-body pn) body
                (js2-node-pos pn) for-pos
                end (js2-node-end body)
                (js2-node-len pn) (- end for-pos))
          (js2-node-add-children pn init cond incr body))
      ;; finally
      (js2-exit-loop))
    pn))

(defun js2-parse-try ()
  "Parser for try-statement.  Last matched token must be js2-TRY."
  (let ((try-pos js2-token-beg)
        try-end
        try-block
        catch-blocks
        finally-block
        saw-default-catch
        peek
        var-name
        catch-cond
        catch-node
        guard-kwd
        catch-pos
        catch-end
        pn
        block
        lp
        rp)
    (js2-consume-token)
    (if (/= (js2-peek-token) js2-LC)
        (js2-report-error "msg.no.brace.try"))
    (setq try-block (js2-parse-statement)
          peek (js2-peek-token))
    (cond
     ((eq peek js2-CATCH)
      (while (js2-match-token js2-CATCH)
        (setq guard-kwd nil
              catch-cond nil
              catch-pos js2-token-beg)
        (if saw-default-catch
            (js2-report-error "msg.catch.unreachable"))
        (if (js2-must-match js2-LP "msg.no.paren.catch")
            (setq lp (- js2-token-beg catch-pos)))
        (js2-must-match js2-NAME "msg.bad.catchcond")
        (setq var-name (make-js2-name-node))
        (if (js2-match-token js2-IF)
            (setq guard-kwd (- js2-token-beg catch-pos)
                  catch-cond (js2-parse-expr))
          (setq saw-default-catch t))
        (if (js2-must-match js2-RP "msg.bad.catchcond")
            (setq rp (- js2-token-beg catch-pos)))
        (js2-must-match js2-LC "msg.no.brace.catchblock")
        (setq block (js2-parse-statements)
              catch-end (js2-node-end block)
              catch-node (make-js2-catch-node :var-name var-name
                                              :guard-expr catch-cond
                                              :guard-kwd guard-kwd
                                              :pos catch-pos
                                              :len (- catch-end catch-pos)
                                              :block block
                                              :lp lp
                                              :rp rp))
        (if (js2-must-match js2-RC "msg.no.brace.after.body")
            (setf catch-end js2-token-beg
                  (js2-node-len block) (- catch-end (js2-node-pos block))
                  (js2-node-len catch-node) (- catch-end catch-pos)))
        (js2-node-add-children catch-node
                               var-name
                               catch-cond
                               block)
        (push catch-node catch-blocks)
        (setf try-end js2-token-end
              (js2-node-len catch-node) (- js2-ts-cursor catch-pos))))
     ((/= peek js2-FINALLY)
      (js2-must-match js2-FINALLY "msg.try.no.catchfinally"
                      (js2-node-pos try-block)
                      (- (setq try-end (js2-node-end try-block))
                         (js2-node-pos try-block)))))

    (when (js2-match-token js2-FINALLY)
      (setq catch-pos js2-token-beg
            block (js2-parse-statement)
            try-end (js2-node-end block)
            finally-block (make-js2-finally-node :pos catch-pos
                                                 :len (- try-end catch-pos)
                                                 :block block))
      (js2-node-add-children finally-block block))
    (setq pn (make-js2-try-node :pos try-pos
                                :len (- try-end try-pos)
                                :try-block try-block
                                :finally-block finally-block))
    (js2-node-add-children pn try-block finally-block)
    (dolist (cb catch-blocks)
      (js2-node-add-children pn cb)
      (push cb (js2-try-node-catch-clauses pn)))
    pn))

(defun js2-parse-throw ()
  "Parser for throw-statement.  Last matched token must be js2-THROW."
  (let ((pos js2-token-beg)
        expr
        node)
    (js2-consume-token)
    (if (eq (js2-peek-token-or-eol) js2-EOL)
        ;; ECMAScript does not allow new lines before throw expression,
        ;; see bug 256617
        (js2-report-error "msg.bad.throw.eol"))
    (setq expr (js2-parse-expr)
          node (make-js2-throw-node :pos pos
                                    :len (- (js2-node-end expr) pos)
                                    :expr expr))
    (js2-node-add-children node expr)
    node))

(defsubst js2-match-jump-label-name ()
  "If we match a js2-NAME, return the labeled statement for that label.
If the name does not match an existing label, reports an error and returns nil.
Returns the `js2-labeled-stmt-node', or nil if the token was not a name.
Side effect:  sets token information for the label identifier."
  (let ((tt (js2-peek-token-or-eol))
        label
        name)
    (when (eq tt js2-NAME)
      (js2-consume-token)
      (setq name js2-ts-string
            label (assoc name js2-label-set))
      (if (null label)
          (progn
            (js2-report-error "msg.undef.label")
            nil)
        label))))

(defun js2-parse-break ()
  "Parser for break-statement.  Last matched token must be js2-BREAK."
  (let ((pos js2-token-beg)
        (end js2-token-end)
        break-target ; statement to break from
        break-label  ; in "break foo", name-node representing the foo
        break-node)
    (js2-consume-token)
    (if (eq (js2-peek-token-or-eol) js2-NAME)
        (setq break-label (make-js2-name-node)
              end (js2-node-end break-label)))
    ;; matchJumpLabelName only matches if there is one
    (unless (setq break-target (js2-match-jump-label-name))
      ;; no break target specified - try for innermost enclosing loop/switch
      (if (null js2-loop-and-switch-set)
          (unless break-label
            (js2-report-error "msg.bad.break" nil pos (length "break")))
        (setq break-target (first js2-loop-and-switch-set))))
    (setq break-node (make-js2-break-node :pos pos
                                          :len (- end pos)
                                          :label break-label
                                          :target break-target))
    (js2-node-add-children break-node break-label)
    break-node))

(defun js2-parse-continue ()
  "Parser for continue-statement.  Last matched token must be js2-CONTINUE."
  (let ((pos js2-token-beg)
        label   ; optional user-specified label, a `js2-name-node'
        target  ; a `js2-loop-node' or `js2-labeled-stmt-node' w/ loop stmt
        node
        (end js2-token-end))
    (js2-consume-token)
    ;; matchJumpLabelName only matches if there is one
    (setq target (js2-match-jump-label-name))  ; a `js2-label-node' or nil
    (cond
     ((null target)
      (if (null js2-loop-set)
          (js2-report-error "msg.continue.outside" nil pos (length "continue"))
        (setq target (first js2-loop-set))))  ; innermost enclosing loop
     (t
      (setq label (make-js2-name-node)
            end (js2-node-end label))
      (unless (or (js2-loop-node-p target)
                  (and (js2-labeled-stmt-node-p target)
                       (js2-loop-node-p (js2-labeled-stmt-node-stmt target))))
        (js2-report-error "msg.continue.nonloop"))))
    (setq node (make-js2-continue-node :pos pos
                                       :len (- end pos)
                                       :label label
                                       :target target))
    (js2-node-add-children node label)
    node))

(defun js2-parse-with ()
  "Parser for with-statement.  Last matched token must be js2-WITH."
  (js2-consume-token)
  (let ((pos js2-token-beg)
        obj
        body
        node
        end
        lp
        rp)
    (if (js2-must-match js2-LP "msg.no.paren.with")
        (setq lp js2-token-beg))
    (setq obj (js2-parse-expr))
    (if (js2-must-match js2-RP "msg.no.paren.after.with")
        (setq rp js2-token-beg))
    (incf js2-nesting-of-with)
    (unwind-protect
        (setq body (js2-parse-statement)
              end (js2-node-end body))
      (decf js2-nesting-of-with))
    (setq node (make-js2-with-node :pos pos
                                   :len (- end pos)
                                   :object obj
                                   :body body
                                   :lp (js2-relpos lp pos)
                                   :rp (js2-relpos rp pos)))
    (js2-node-add-children node obj body)
    node))

(defun js2-parse-const-var ()
  "Parser for var- or const-statement.
Last matched token must be js2-CONST or js2-VAR."
  (let ((tt (js2-peek-token))
        (pos js2-token-beg)
        expr
        pn)
    (js2-consume-token)
    (setq expr (js2-parse-variables tt)
          pn (make-js2-expr-stmt-node :pos pos
                                      :len (- (js2-node-end expr) pos)
                                      :expr expr))
    (js2-node-add-children pn expr)
    pn))

(defun js2-parse-let ()
  "Parser for let-statement.  Last matched token must be js2-LET."
  (js2-consume-token)
  (let ((let-pos js2-token-beg)
        (tt (js2-peek-token))
        expr
        pn)
    (if (eq tt js2-LP)
        (js2-parse-let-stmt t let-pos)  ; return value
      ;; Else we're looking at a statement like let x = 6, y = 7;
      ;; Rhino just returns a variables Node of type Token.LET,
      ;; but we'll wrap it in a let-stmt without a block.
      (setq expr (js2-parse-variables js2-LET)
            pn (make-js2-let-stmt-node :pos let-pos
                                       :len (- (js2-node-end expr) let-pos)
                                       :vars expr))
      (js2-node-add-children pn expr)
      pn)))

(defun js2-parse-ret-yield ()
  (js2-parse-return-or-yield (js2-peek-token) nil))

(defconst js2-parse-return-stmt-enders
  (list js2-SEMI js2-RC js2-EOF js2-EOL js2-ERROR js2-RB js2-RP js2-YIELD))

(defsubst js2-now-all-set (before after mask)
  "Return whether or not the bits in the mask have changed to all set.
BEFORE is bits before change, AFTER is bits after change, and MASK is
the mask for bits.  Returns t if all the bits in the mask are set in AFTER
but not BEFORE."
  (and (/= (logand before mask) mask)
       (= (logand after mask) mask)))

(defun js2-parse-return-or-yield (tt expr-context)
  (let ((pos js2-token-beg)
        (end js2-token-end)
        (before js2-end-flags)
        (inside-function (js2-inside-function))
        e
        ret
        name)
    (unless inside-function
      (js2-report-error (if (eq tt js2-RETURN)
                            "msg.bad.return"
                          "msg.bad.yield")))
    (js2-consume-token)
    ;; This is ugly, but we don't want to require a semicolon.
    (unless (memq (js2-peek-token-or-eol) js2-parse-return-stmt-enders)
      (setq e (js2-parse-expr)
            end (js2-node-end e)))
    (cond
     ((eq tt js2-RETURN)
      (when inside-function
        (if (null e)
            (setq js2-end-flags (set-flag js2-end-flags js2-end-returns))
          (setq js2-end-flags (set-flag js2-end-flags js2-end-returns-value))))
      (setq ret (make-js2-return-node :pos pos
                                      :len (- end pos)
                                      :retval e))
      (js2-node-add-children ret e)
      ;; see if we need a strict mode warning
      (if (and inside-function
               js2-strict-inconsistent-return-warning
               (js2-now-all-set before js2-end-flags
                                (logior js2-end-returns js2-end-returns-value)))
          (js2-add-strict-warning "msg.return.inconsistent" nil pos end)))
     (t
      (setq js2-end-flags (set-flag js2-end-flags js2-end-yields)
            ret (make-js2-yield-node :pos pos
                                     :len (- end pos)
                                     :value e))
      (js2-node-add-children ret e)
      (unless expr-context
        (setq e ret
              ret (make-js2-expr-stmt-node :pos pos
                                           :len (- end pos)
                                           :expr e))
        (js2-node-add-children ret e))
      (js2-set-requires-activation)
      (js2-set-is-generator)))

    ;; see if we are mixing yields and value returns.
    (when (and inside-function
               (js2-now-all-set before js2-end-flags
                                (logior js2-end-yields js2-end-returns-value)))
      (setq name (js2-function-node-name js2-current-script-or-fn))
      (if name
          (setq name (js2-name-node-name name)))  ; get string from name-node
      (if (zerop (length name))
          (js2-report-error "msg.anon.generator.returns" nil pos end)
        (js2-report-error "msg.generator.returns" name pos end)))

    ret))

(defun js2-parse-debugger ()
  (js2-consume-token)
  (make-js2-debugger-node))

(defun js2-parse-default-xml-namespace ()
  "Parse a `default xml namespace = <expr>' e4x statement."
  (let ((pos js2-token-beg)
        end expr unary es)
    (js2-consume-token)
    (js2-must-have-xml)
    (js2-set-requires-activation)
    (unless (and (js2-match-token js2-NAME)
                 (string= js2-ts-string "xml"))
      (js2-report-error "msg.bad.namespace" nil pos js2-ts-cursor))
    (unless (and (js2-match-token js2-NAME)
                 (string= js2-ts-string "namespace"))
      (js2-report-error "msg.bad.namespace" nil pos js2-ts-cursor))
    (unless (js2-match-token js2-ASSIGN)
      (js2-report-error "msg.bad.namespace" nil pos js2-ts-cursor))
    (setq expr (js2-parse-expr)
          end (js2-node-end expr)
          unary (make-js2-unary-node :type js2-DEFAULTNAMESPACE
                                     :pos pos
                                     :len (- end pos)
                                     :operand expr))
    (js2-node-add-children unary expr)
    (make-js2-expr-stmt-node :pos pos
                             :len (- end pos)
                             :expr unary)))

(defun js2-parse-block ()
  "Parser for a curly-delimited statement block.
Last token matched must be js2-LC."
  (let* ((pos js2-token-beg)
         (block (make-js2-block-node :pos pos)))
    (js2-consume-token)
    (setf (js2-block-node-scope block) (make-js2-scope :type js2-BLOCK
                                                       :ast-node block))
    (js2-push-scope block)
    (unwind-protect
        (progn
          (js2-parse-statements block)
          (js2-must-match js2-RC "msg.no.brace.block")
          (setf (js2-node-len block) (- js2-token-end pos)))
      (js2-pop-scope))
    block))

;; for js2-ERROR too, to have a node for error recovery to work on
(defun js2-parse-semi ()
  "Parse a statement or handle an error.
Last matched token is js-SEMI or js-ERROR."
  (let ((tt (js2-peek-token)) pos len)
    (js2-consume-token)
    (if (eq tt js2-SEMI)
        (make-js2-empty-stmt-node :len 1)
      (setq pos js2-token-beg
            len (- js2-token-beg pos))
      (js2-report-error "msg.syntax" pos js2-token-end)
      (make-js2-error-node :pos pos :len len))))

(defun js2-parse-name ()
  "Parser for identifier or label.  Last token matched must be js2-NAME.

If the name is not followed by a colon, we return the name as a
`js2-expr-stmt-node'.

If it's followed by a colon, we parse the statement following the
colon and return the whole thing as a `js2-labeled-stmt-node'.
This node is used to accumulate any consecutive labels we find.

This strategy came from Rhino, presumably via SpiderMonkey."
  (let ((pos js2-token-beg)
        (end js2-token-end)
        (name js2-ts-string)
        dup-label
        stmt
        pn)
    (js2-set-check-for-label)
    (setq pn (js2-parse-expr))
    (if (/= (js2-node-type pn) js2-LABEL)
        (progn
          ;; Parsed non-label expression - wrap with statement node.
          (setq pn (make-js2-expr-stmt-node :pos pos
                                            :len (js2-node-len pn)
                                            :type (if (js2-inside-function)
                                                      js2-EXPR_VOID
                                                    js2-EXPR_RESULT)
                                            :expr pn))
          (js2-node-add-children pn (js2-expr-stmt-node-expr pn)))

      ;; Else parsed a label.
      (js2-consume-token)
      (when (setq dup-label (cdr (assoc name js2-label-set)))
        (setq dup-label (js2-get-label-by-name dup-label name))
        (js2-report-error "msg.dup.label" nil
                          (js2-node-abs-pos dup-label)
                          (js2-node-len dup-label))
        (js2-report-error "msg.dup.label" nil pos (js2-node-len pn)))

      ;; Add to buffer-local label set.
      (if (null js2-statement-label)
          (setq js2-statement-label
                (make-js2-labeled-stmt-node :labels (list pn)
                                            :pos (js2-node-pos pn)))
        (js2-labeled-stmt-node-add-label js2-labeled-stmt pn))
      (js2-node-add-children js2-labeled-stmt pn)

      ;; Add one reference to the bundle per label in `js2-label-set'
      (push (cons name js2-labeled-stmt) js2-label-set)

      ;; Parse the following statement, then remove label from label set.
      (unwind-protect
          (setf stmt (js2-statement-helper))
        (setq js2-label-set (js2-delete-if (lambda (entry)
                                             (string= (car entry) name))
                                           js2-label-set)))
      ;; At this point we've got at least one label in `js2-labeled-stmt'
      ;; and we've parsed stmt, a statement.  stmt may be a labeled statement
      ;; node, in which case we just pass it up tail-recursively.  Otherwise
      ;; we save it in the accumulator labled-stmt node and return that.
      (if (js2-labeled-stmt-node-p stmt)
          (setq pn stmt)  ; return tail-recursively
        (setf (js2-labeled-stmt-node-stmt js2-labeled-stmt) stmt
              ;; set entire length now that stmt is parsed
              (js2-node-len js2-labeled-stmt)
              (- (js2-node-end stmt)
                 (js2-node-pos js2-labeled-stmt)))
        (js2-node-add-children js2-labeled-stmt stmt)
        (setq pn js2-labeled-stmt)))      ; return `js2-labeled-stmt-node'
    pn))

(defun js2-parse-expr-stmt ()
  (let* ((pos js2-token-beg)
         (expr (js2-parse-expr))
         (pn (make-js2-expr-stmt-node :pos pos
                                      :len (js2-node-len expr)
                                      :type (if (js2-inside-function)
                                                js2-EXPR_VOID
                                              js2-EXPR_RESULT)
                                      :expr expr)))
    (js2-node-add-children pn expr)
    pn))

(defun js2-parse-variables (decl-type)
  "Parse a comma-separated list of variable declarations.
Could be a 'var', 'const' or 'let' stmt/expr or for-loop initializer.
DECL-TYPE is a token value: either VAR, CONST, or LET depending on context.
For 'var' or 'const', the keyword must be the token last scanned.
Returns the parsed `js2-var-decl-node' expression node.  If var/const, the
node length includes the var/const keyword.  If it's a let-stmt/expr, then
the node position coincides with the first var-init child."
  (let* ((pos (and (/= decl-type js2-LET) js2-token-beg))
         (result (make-js2-var-decl-node :decl-type decl-type
                                         :pos pos))
         destructuring-init
         destructuring
         kid-pos
         s
         tt
         init
         name
         node
         end
         nbeg nend
         (continue t))
    ;; Example:
    ;; var foo = {a: 1, b: 2}, bar = [3, 4];
    ;; var {b: s2, a: s1} = foo, x = 6, y, [s3, s4] = bar;
    (while continue
      (setq destructuring nil
            s nil
            tt (js2-peek-token)
            kid-pos js2-token-beg
            init nil)
      (unless pos
        (setf pos kid-pos
              (js2-node-pos result) kid-pos))
      (if (or (eq tt js2-LB) (eq tt js2-LC))
          ;; Destructuring assignment, e.g., var [a, b] = ...
          (setq destructuring (js2-parse-primary-expr))
        ;; Simple variable name
        (js2-must-match js2-NAME "msg.bad.var")
        (setq name (make-js2-name-node)
              nbeg js2-token-beg
              nend js2-token-end)
        (js2-define-symbol decl-type js2-ts-string name js2-in-for-init))
      (setq end js2-token-end)

      (when (js2-match-token js2-ASSIGN)
        (setq init (js2-parse-assign-expr)
              end (js2-node-end init))
        (when js2-parse-ide-mode
          (if (or (js2-object-node-p init)
                  (js2-function-node-p init))
              (js2-record-imenu-functions init name))))

      (when nbeg
        (js2-set-face nbeg nend
                      (if (js2-function-node-p init)
                          'font-lock-function-name-face
                        'font-lock-variable-name-face)
                      'record))
      (if destructuring
          (progn
            (if (null init)
                ;; for (var [k, v] in foo) is initialized differently
                (unless js2-in-for-init
                  (js2-report-error "msg.destruct.assign.no.init")))
            (setq node (make-js2-destructuring-init-node :pos kid-pos
                                                         :len (- end kid-pos)
                                                         :lhs destructuring
                                                         :initializer init))
            (js2-node-add-children node destructuring init))
        ;; simple variable, possibly with initializer
        (setq node (make-js2-var-init-node :pos kid-pos
                                           :len (- end kid-pos)
                                           :name name
                                           :initializer init))
        (js2-node-add-children node name init))

      (js2-block-node-push result node)
      (unless (js2-match-token js2-COMMA)
        (setq continue nil)))
    (setf (js2-node-len result) (- end pos))
    result))

(defun js2-parse-let-stmt (&optional stmt-p let-pos)
  (let ((pos (or let-pos js2-token-beg))
        (scope (make-js2-scope :type js2-LET))
        pn vars stmt expr
        pos2 len lp rp)
    (if (js2-must-match js2-LP "msg.no.paren.after.let")
        (setq lp js2-token-beg))
    (js2-push-scope scope)
    (unwind-protect
        (progn
          (setq vars (js2-parse-variables js2-LET))
          (if (js2-must-match js2-RP "msg.no.paren.let")
              (setq rp js2-token-beg))
          (if (and stmt-p (eq (js2-peek-token) js2-LC))
              ;; let statement
              (progn
                (js2-consume-token)
                (setf pos2 js2-token-beg ; pos stmt at LC
                      stmt (js2-parse-statements)
                      (js2-node-len stmt) (- js2-ts-cursor pos2)
                      pn (make-js2-let-stmt-node :pos pos
                                                 :vars vars
                                                 :body stmt
                                                 :lp (js2-relpos lp pos)
                                                 :rp (js2-relpos rp pos))
                      (js2-let-stmt-node-scope pn) scope
                      (js2-scope-ast-node scope) pn)
                (js2-node-add-children pn vars stmt)
                (js2-must-match js2-RC "msg.no.curly.let")
                (setf (js2-node-len pn) (- js2-ts-cursor pos)))
            ;; let expression
            (setf expr (js2-parse-expr)
                  (js2-scope-type scope) js2-LETEXPR
                  pn (make-js2-let-expr-node :pos pos
                                             :len (- (js2-node-end expr) pos)
                                             :vars vars
                                             :expr expr
                                             :lp (js2-relpos lp pos)
                                             :rp (js2-relpos rp pos))
                  (js2-let-expr-node-scope pn) scope
                  (js2-scope-ast-node scope) pn)
            (js2-node-add-children pn vars expr)
            (when stmt-p
              ;; let expression in statement context
              (setq stmt (make-js2-expr-stmt-node :pos pos
                                                  :len (js2-node-len pn)
                                                  :type (if (js2-inside-function)
                                                            js2-EXPR_VOID
                                                          js2-EXPR_RESULT)
                                                  :expr pn))
              (js2-node-add-children stmt pn)
              (setq pn stmt))))
      (js2-pop-scope))
    pn))

(defsubst js2-define-new-symbol (decl-type name node)
  (js2-scope-put-symbol js2-current-scope
                        name
                        (make-js2-symbol decl-type name node)))

(defun js2-define-symbol (decl-type name &optional node ignore-in-block)
  "Define a symbol in the current scope.
If NODE is non-nil, it is the AST node associated with the symbol."
  (let* ((defining-scope (js2-get-defining-scope js2-current-scope name))
         (symbol (if defining-scope
                     (js2-scope-get-symbol defining-scope name)))
         (sdt (if symbol (js2-symbol-decl-type symbol))))
    (cond
     ((and symbol                       ; already defined
           ;; new version is const
           (or (eq decl-type js2-CONST)
               ;; old version is const
               (eq sdt js2-CONST)
               ;; two let-bound vars in this block have same name
               (and (eq decl-type js2-LET)
                    (eq defining-scope js2-current-scope))))
      (js2-report-error
       (cond
        ((eq sdt js2-CONST) "msg.const.redecl")
        ((eq sdt js2-LET) "msg.let.redecl")
        ((eq sdt js2-VAR) "msg.var.redecl")
        ((eq sdt js2-FUNCTION) "msg.function.redecl")
        (t "msg.parm.redecl"))
       name))

     ((eq decl-type js2-LET)
      (if (and (not ignore-in-block)
               (let ((node (js2-scope-ast-node js2-current-scope)))
                 (or (eq (js2-node-type node) js2-IF)
                     (js2-loop-node-p node))))
          (js2-report-error "msg.let.decl.not.in.block")
        (js2-define-new-symbol decl-type name node)))

     ((or (eq decl-type js2-VAR)
          (eq decl-type js2-CONST)
          (eq decl-type js2-FUNCTION))
      (if symbol
          (if (and (eq sdt js2-VAR)
                   js2-strict-var-redeclaration-warning)
              (js2-add-strict-warning "msg.var.redecl" name)
            (if (and (eq sdt js2-LP)
                     js2-strict-var-hides-function-arg-warning)
                (js2-add-strict-warning "msg.var.hides.arg" name)))
        (js2-scope-put-symbol js2-current-scope
                              name
                              (make-js2-symbol decl-type name node))))
     ((eq decl-type js2-LP)
      (if symbol
          ;; must be duplicate parameter. Second parameter hides the
          ;; first, so go ahead and add the second pararameter
          (js2-report-warning "msg.dup.parms" name))
      (js2-define-new-symbol decl-type name node))
     (t
      (js2-code-bug)))))

(defun js2-parse-expr ()
  (let* ((pn (js2-parse-assign-expr))
         (pos (js2-node-pos pn))
         left
         right
         op-pos)
    (while (js2-match-token js2-COMMA)
      (setq op-pos (- js2-token-beg pos))  ; relative
      (unless (js2-node-has-side-effects pn)
        (js2-add-strict-warning "msg.no.side.effects" nil
                                pos
                                (js2-node-end pn)))
      (if (eq (js2-peek-token) js2-YIELD)
          (js2-report-error "msg.yield.parenthesized"))
      (setq right (js2-parse-assign-expr)
            left pn
            pn (make-js2-infix-node :type js2-COMMA
                                    :pos pos
                                    :len (- js2-ts-cursor pos)
                                    :op-pos op-pos
                                    :left left
                                    :right right))
      (js2-node-add-children pn left right))
    pn))

(defun js2-parse-assign-expr ()
  (let ((tt (js2-peek-token))
        (pos js2-token-beg)
        pn
        left
        right
        op-pos)
    (if (eq tt js2-YIELD)
        (js2-parse-return-or-yield tt t)
      ;; not yield - parse assignment expression
      (setq pn (js2-parse-cond-expr)
            tt (js2-peek-token))
      (when (and (<= js2-first-assign tt)
                 (<= tt js2-last-assign))
        (js2-consume-token)
        (setq op-pos (- js2-token-beg pos)  ; relative
              left pn
              right (js2-parse-assign-expr)
              pn (make-js2-assign-node :type tt
                                       :pos pos
                                       :len (- (js2-node-end right) pos)
                                       :op-pos op-pos
                                       :left left
                                       :right right))
        (when js2-parse-ide-mode
          (js2-highlight-assign-targets pn left right)
          (if (or (js2-function-node-p right)
                  (js2-object-node-p right))
              (js2-record-imenu-functions right left)))
        ;; do this last so ide checks above can use absolute positions
        (js2-node-add-children pn left right))
      pn)))

(defun js2-parse-cond-expr ()
  (let ((pos js2-token-beg)
        (pn (js2-parse-or-expr))
        test-expr
        if-true
        if-false
        q-pos
        c-pos)
    (when (js2-match-token js2-HOOK)
      (setq q-pos (- js2-token-beg pos)
            if-true (js2-parse-assign-expr))
      (js2-must-match js2-COLON "msg.no.colon.cond")
      (setq c-pos (- js2-token-beg pos)
            if-false (js2-parse-assign-expr)
            test-expr pn
            pn (make-js2-cond-node :pos pos
                                   :len (- (js2-node-end if-false) pos)
                                   :test-expr test-expr
                                   :true-expr if-true
                                   :false-expr if-false
                                   :q-pos q-pos
                                   :c-pos c-pos))
      (js2-node-add-children pn test-expr if-true if-false))
    pn))

(defun js2-make-binary (type left parser)
  "Helper for constructing a binary-operator AST node.
LEFT is the left-side-expression, already parsed, and the
binary operator should have just been matched.
PARSER is a function to call to parse the right operand,
or a `js2-node' struct if it has already been parsed."
  (let* ((pos (js2-node-pos left))
         (op-pos (- js2-token-beg pos))
         (right (if (js2-node-p parser)
                    parser
                  (funcall parser)))
         (pn (make-js2-infix-node :type type
                                  :pos pos
                                  :len (- (js2-node-end right) pos)
                                  :op-pos op-pos
                                  :left left
                                  :right right)))
    (js2-node-add-children pn left right)
    pn))

(defun js2-parse-or-expr ()
  (let ((pn (js2-parse-and-expr)))
    (when (js2-match-token js2-OR)
      (setq pn (js2-make-binary js2-OR
                                pn
                                'js2-parse-or-expr)))
    pn))

(defun js2-parse-and-expr ()
  (let ((pn (js2-parse-bit-or-expr)))
    (when (js2-match-token js2-AND)
      (setq pn (js2-make-binary js2-AND
                                pn
                                'js2-parse-and-expr)))
    pn))

(defun js2-parse-bit-or-expr ()
  (let ((pn (js2-parse-bit-xor-expr)))
    (while (js2-match-token js2-BITOR)
      (setq pn (js2-make-binary js2-BITOR
                                pn
                                'js2-parse-bit-xor-expr)))
    pn))

(defun js2-parse-bit-xor-expr ()
  (let ((pn (js2-parse-bit-and-expr)))
    (while (js2-match-token js2-BITXOR)
      (setq pn (js2-make-binary js2-BITXOR
                                pn
                                'js2-parse-bit-and-expr)))
    pn))

(defun js2-parse-bit-and-expr ()
  (let ((pn (js2-parse-eq-expr)))
    (while (js2-match-token js2-BITAND)
      (setq pn (js2-make-binary js2-BITAND
                                pn
                                'js2-parse-eq-expr)))
    pn))

(defconst js2-parse-eq-ops
  (list js2-EQ js2-NE js2-SHEQ js2-SHNE))

(defun js2-parse-eq-expr ()
  (let ((pn (js2-parse-rel-expr))
        tt)
    (while (memq (setq tt (js2-peek-token)) js2-parse-eq-ops)
      (js2-consume-token)
      (setq pn (js2-make-binary tt
                                pn
                                'js2-parse-rel-expr)))
    pn))

(defconst js2-parse-rel-ops
  (list js2-IN js2-INSTANCEOF js2-LE js2-LT js2-GE js2-GT))

(defun js2-parse-rel-expr ()
  (let ((pn (js2-parse-shift-expr))
        (continue t)
        tt)
    (while continue
      (setq tt (js2-peek-token))
      (cond
       ((and js2-in-for-init (eq tt js2-IN))
        (setq continue nil))
       ((memq tt js2-parse-rel-ops)
        (js2-consume-token)
        (setq pn (js2-make-binary tt pn 'js2-parse-shift-expr)))
       (t
        (setq continue nil))))
    pn))

(defconst js2-parse-shift-ops
  (list js2-LSH js2-URSH js2-RSH))

(defun js2-parse-shift-expr ()
  (let ((pn (js2-parse-add-expr))
        tt
        (continue t))
    (while continue
      (setq tt (js2-peek-token))
      (if (memq tt js2-parse-shift-ops)
          (progn
            (js2-consume-token)
            (setq pn (js2-make-binary tt pn 'js2-parse-add-expr)))
        (setq continue nil)))
    pn))

(defun js2-parse-add-expr ()
  (let ((pn (js2-parse-mul-expr))
        tt
        (continue t))
    (while continue
      (setq tt (js2-peek-token))
      (if (or (eq tt js2-ADD) (eq tt js2-SUB))
          (progn
            (js2-consume-token)
            (setq pn (js2-make-binary tt pn 'js2-parse-mul-expr)))
        (setq continue nil)))
    pn))

(defconst js2-parse-mul-ops
  (list js2-MUL js2-DIV js2-MOD))

(defun js2-parse-mul-expr ()
  (let ((pn (js2-parse-unary-expr))
        tt
        (continue t))
    (while continue
      (setq tt (js2-peek-token))
      (if (memq tt js2-parse-mul-ops)
          (progn
            (js2-consume-token)
            (setq pn (js2-make-binary tt pn 'js2-parse-unary-expr)))
        (setq continue nil)))
    pn))

(defsubst js2-make-unary (type parser &rest args)
  "Make a unary node of type TYPE.
PARSER is either a node (for postfix operators) or a function to call
to parse the operand (for prefix operators)."
  (let* ((pos js2-token-beg)
         (postfix (js2-node-p parser))
         (expr (if postfix
                   parser
                 (apply parser args)))
         end
         pn)
    (if postfix  ; e.g. i++
        (setq pos (js2-node-pos expr)
              end js2-token-end)
      (setq end (js2-node-end expr)))
    (setq pn (make-js2-unary-node :type type
                                  :pos pos
                                  :len (- end pos)
                                  :operand expr))
    (js2-node-add-children pn expr)
    pn))

(defconst js2-incrementable-node-types
  (list js2-NAME js2-GETPROP js2-GETELEM js2-GET_REF js2-CALL)
  "Node types that can be the operand of a ++ or -- operator.")

(defsubst js2-check-bad-inc-dec (tt beg end unary)
  (unless (memq (js2-node-type (js2-unary-node-operand unary))
                js2-incrementable-node-types)
    (js2-report-error (if (eq tt js2-INC)
                          "msg.bad.incr"
                        "msg.bad.decr")
                      nil beg (- end beg))))

(defun js2-parse-unary-expr ()
  (let ((tt (js2-peek-token))
        pn expr beg end)
    (cond
     ((or (eq tt js2-VOID)
          (eq tt js2-NOT)
          (eq tt js2-BITNOT)
          (eq tt js2-TYPEOF))
      (js2-consume-token)
      (js2-make-unary tt 'js2-parse-unary-expr))

     ((eq tt js2-ADD)
      (js2-consume-token)
      ;; Convert to special POS token in decompiler and parse tree
      (js2-make-unary js2-POS 'js2-parse-unary-expr))

     ((eq tt js2-SUB)
      (js2-consume-token)
      ;; Convert to special NEG token in decompiler and parse tree
      (js2-make-unary js2-NEG 'js2-parse-unary-expr))

     ((or (eq tt js2-INC)
          (eq tt js2-DEC))
      (js2-consume-token)
      (prog1
          (setq beg js2-token-beg
                end js2-token-end
                expr (js2-make-unary tt 'js2-parse-member-expr t))
        (js2-check-bad-inc-dec tt beg end expr)))

     ((eq tt js2-DELPROP)
      (js2-consume-token)
      (js2-make-unary js2-DELPROP 'js2-parse-unary-expr))

     ((eq tt js2-ERROR)
      (js2-consume-token)
      (make-js2-error-node))  ; try to continue

     ((and (eq tt js2-LT)
           js2-compiler-xml-available)
      ;; XML stream encountered in expression.
      (js2-consume-token)
      (js2-parse-member-expr-tail t (js2-parse-xml-initializer)))
     (t
      (setq pn (js2-parse-member-expr t)
            ;; Don't look across a newline boundary for a postfix incop.
            tt (js2-peek-token-or-eol))
      (when (or (eq tt js2-INC) (eq tt js2-DEC))
        (js2-consume-token)
        (setf expr pn
              pn (js2-make-unary tt expr))
        (js2-node-set-prop pn 'postfix t)
        (js2-check-bad-inc-dec tt js2-token-beg js2-token-end pn))
      pn))))

(defun js2-parse-xml-initializer ()
  "Parse an E4X XML initializer.
I'm parsing it the way Rhino parses it, but without the tree-rewriting.
Then I'll postprocess the result, depending on whether we're in IDE
mode or codegen mode, and generate the appropriate rewritten AST.
IDE mode uses a rich AST that models the XML structure.  Codegen mode
just concatenates everything and makes a new XML or XMLList out of it."
  (let ((tt (js2-get-first-xml-token))
        (pos js2-token-beg)
        pn-xml
        pn
        n
        expr
        kids
        expr-pos
        prepend)
    (when (not (or (eq tt js2-XML) (eq tt js2-XMLEND)))
      (js2-report-error "msg.syntax"))
    (setq pn-xml (make-js2-xml-node))
    (catch 'return
      (while t
        (cond
         ;; js2-XML means we found a {expr} in the XML stream.
         ;; The js2-ts-string is the XML up to the left-curly.
         ((eq tt js2-XML)
          (push (make-js2-string-node :pos js2-token-beg
                                      :len (- js2-ts-cursor js2-token-beg))
                kids)
          (js2-must-match js2-LC "msg.syntax")
          (setq expr-pos js2-ts-cursor
                expr (if (eq (js2-peek-token) js2-RC)
                         (make-js2-string-node :pos expr-pos
                                               :len 0
                                               :value "")
                       (js2-parse-expr)))
          (js2-must-match js2-RC "msg.syntax")
          (setq pn (make-js2-xml-js-expr-node :pos expr-pos
                                              :len (js2-node-len expr)
                                              :expr expr))
          (js2-node-add-children pn expr)
          (push pn kids))

         ;; a js2-XMLEND token means we hit the final close-tag.
         ((eq tt js2-XMLEND)
          (push (make-js2-string-node :pos js2-token-beg
                                      :len (- js2-ts-cursor js2-token-beg))
                kids)
          (dolist (kid (nreverse kids))
            (js2-block-node-push pn-xml kid))
          (setf (js2-node-len pn-xml) (- js2-ts-cursor
                                         (js2-node-pos pn-xml)))
          (throw 'return pn-xml))

         (t
          (js2-report-error "msg.syntax")
          (throw 'return pn-xml)))
        (setq tt (js2-get-next-xml-token))))))

(defun js2-parse-argument-list ()
  "Parse an argument list and return it as a lisp list of nodes.
Returns the list in reverse order.  Consumes the right-paren token."
  (let (result)
    (unless (js2-match-token js2-RP)
      (loop do
            (if (eq (js2-peek-token) js2-YIELD)
                (js2-report-error "msg.yield.parenthesized"))
            (push (js2-parse-assign-expr) result)
            while
            (js2-match-token js2-COMMA))
      (js2-must-match js2-RP "msg.no.paren.arg")
      result)))

(defun js2-parse-member-expr (&optional allow-call-syntax)
  (let ((tt (js2-peek-token))
        pn
        pos
        target
        args
        beg
        end
        init
        result)
    (if (neq tt js2-NEW)
        (setq pn (js2-parse-primary-expr))
      (js2-consume-token)
      (setq pos js2-token-beg
            target (js2-parse-member-expr)
            end (js2-node-end target)
            pn (make-js2-new-node :pos pos
                                  :target target
                                  :len (- end pos)))
      (js2-node-add-children pn (js2-new-node-target pn))
      (when (js2-match-token js2-LP)
        ;; Add the arguments to pn, if any are supplied.
        (setf beg pos  ; start of "new" keyword
              pos js2-token-beg
              args (js2-parse-argument-list)
              end js2-token-end
              (js2-new-node-lp pn) (- pos beg)
              (js2-new-node-rp pn) (- end 1 beg))

        (dolist (arg args)
          (push arg (js2-new-node-args pn))
          (js2-node-add-children pn arg)))

      (when (and js2-allow-rhino-new-expr-initializer
                 (eq (js2-peek-token) js2-LC))
        (js2-consume-token)
        (setf init (js2-parse-object-literal)
              end (js2-node-end init)
              (js2-new-node-initializer pn) init)
        (js2-node-add-children pn init))

        (incf (js2-node-len pn) (- end pos)))
    (setq result (js2-parse-member-expr-tail allow-call-syntax pn))
    (if (>= js2-highlight-level 2)
        (js2-parse-highlight-member-expr-node result))
    result))

(defun js2-parse-member-expr-tail (allow-call-syntax pn)
  "Parse a chain of property/array accesses or function calls.
Includes parsing for E4X operators like `..' and `.@'.
If ALLOW-CALL-SYNTAX is nil, stops when we encounter a left-paren.
Returns an expression tree that includes PN, the parent node."
  (let (tt args pos expr lb rb
        (beg (js2-node-pos pn)))
    (catch 'tail-loop
      (while t
        (setq tt (js2-peek-token))
        (catch 'break-outer
          (cond
           ((or (eq tt js2-DOT) (eq tt js2-DOTDOT))
            (setq pn (js2-parse-property-access tt pn)))

           ((eq tt js2-DOTQUERY)
            (js2-consume-token)
            (js2-must-have-xml)
            (setq pos js2-token-beg
                  expr (js2-parse-expr)
                  pn (make-js2-xml-dot-query-node :left pn
                                                  :pos beg
                                                  :op-pos pos
                                                  :right expr))
            (js2-node-add-children pn
                                   (js2-xml-dot-query-node-left pn)
                                   (js2-xml-dot-query-node-right pn))
            (js2-must-match js2-RP "msg.no.paren")
            (setf (js2-node-len pn) (- js2-token-end beg)))

           ((eq tt js2-LB)
            (js2-consume-token)
            (setq lb js2-token-beg
                  pos (js2-node-pos pn)
                  expr (js2-parse-expr))
            (if (js2-must-match js2-RB "msg.no.bracket.index")
                (setq rb js2-token-beg))
            (setq pn (make-js2-elem-get-node :target pn
                                             :pos pos
                                             :prop expr
                                             :lb (js2-relpos lb pos)
                                             :rb (js2-relpos rb pos)
                                             :len (- js2-token-end pos)))
            (js2-node-add-children pn
                                   (js2-elem-get-node-target pn)
                                   (js2-elem-get-node-prop pn)))

           ((eq tt js2-LP)
            (unless allow-call-syntax
              (throw 'tail-loop nil))
            (js2-consume-token)
            (setq pn (make-js2-call-node :pos beg
                                         :expr pn
                                         :lp (- js2-token-beg beg)))
            (js2-node-add-children pn (js2-call-node-expr pn))
            ;; Add the arguments to pn, if any are supplied.
            (setf args (js2-parse-argument-list)
                  (js2-call-node-rp pn) (- js2-token-beg beg))
            (dolist (arg args)
              (push arg (js2-call-node-args pn))
              (js2-node-add-children pn arg))
            (setf (js2-node-len pn) (- js2-ts-cursor (js2-node-pos pn))))
           (t
            (throw 'tail-loop nil))))))
    pn))

(defun js2-parse-property-access (tt pn)
  "Parse a property access, XML descendants access, or XML attr access."
  (let ((member-type-flags 0)
        (dot-pos js2-token-beg)
        (dot-len (if (eq tt js2-DOTDOT) 2 1))
        result)
    (js2-consume-token)
    (when (eq tt js2-DOTDOT)
      (js2-must-have-xml)
      (setq member-type-flags js2-descendants-flag))
    (if (not js2-compiler-xml-available)
        (progn
          (js2-must-match-prop-name "msg.no.name.after.dot")
          (setq result
                (make-js2-prop-get-node :target pn
                                        :pos js2-token-beg
                                        :prop (make-js2-name-node)
                                        :len (- js2-token-end js2-token-beg)))
          (js2-node-add-children pn
                                 (js2-prop-get-node-target pn)
                                 (js2-prop-get-node-prop pn))
          result)
      (setq tt (js2-next-token))
      (cond
       ;; needed for generator.throw();
       ((eq tt js2-THROW)
        (js2-parse-property-name pn
                                 (make-js2-name-node) ; "throw"
                                 member-type-flags))

       ;; handles: name, ns::name, ns::*, ns::[expr]
       ((js2-valid-prop-name-token tt)
        (js2-parse-property-name pn
                                 (make-js2-name-node)  ; name|ns
                                 member-type-flags))

       ;; handles: *, *::name, *::*, *::[expr]
       ((eq tt js2-MUL)
        (js2-parse-property-name pn
                                 (make-js2-name-node :name "*")
                                 member-type-flags))
       ;; handles: '@attr', '@ns::attr', '@ns::*', '@ns::*',
       ;;          '@::attr', '@::*', '@*', '@*::attr', '@*::*'
       ((eq tt js2-XMLATTR)
        (js2-parse-attribute-access pn member-type-flags))
       (t
        (js2-report-error "msg.no.name.after.dot" nil dot-pos dot-len)
        (make-js2-error-node :pos dot-pos
                             :len dot-len))))))

(defun js2-parse-attribute-access (pn member-type-flags)
  "Parse an E4X XML attribute expression.
PN (parent node) is the XML target to the left of the @ operator."
  (let ((tt (js2-next-token))
        expr pos beg len)
    (setq member-type-flags (set-flag member-type-flags js2-attribute-flag))
    (cond
     ((js2-valid-prop-name-token tt)
      ;; handles: @name, @ns::name, @ns::*, @ns::[expr]
      (setq pn (js2-parse-property-name pn
                                        (make-js2-name-node)
                                        member-type-flags)))
     ;; handles: @*, @*::name, @*::*, @*::[expr]
     ((eq tt js2-MUL)
      (setq pn (js2-parse-property-name pn
                                        (make-js2-name-node :name "*")
                                        member-type-flags)))
     ;; handles @[expr]
     ((eq tt js2-LB)
      (setq pos (1- js2-ts-cursor)
            pn (make-js2-xml-ref-node :pos pos
                                      :ref-expr (js2-parse-expr)
                                      :len (- js2-ts-cursor pos)
                                      :flags member-type-flags))
      (js2-node-add-children pn (js2-xml-ref-node-ref-expr pn))
      (js2-must-match js2-RB "msg.no.bracket.index"))
     (t
      (if pn
          (setq len (js2-node-len pn)
                beg (- js2-ts-cursor len))
        (save-excursion
          (goto-char js2-ts-cursor)
          (setq beg (point-at-bol)
                len (- (point-at-eol) beg))))
      (js2-report-error "msg.no.name.after.xmlAttr" nil beg len)
      (setq pos (1- js2-ts-cursor)
            pn (make-js2-xml-ref-node :pos pos
                                      :target pn
                                      :len (if pn (js2-node-len pn))
                                      :flags member-type-flags))
      (js2-node-add-children pn (js2-xml-ref-node-target pn))))
    pn))

(defsubst js2-create-property-get (target namespace name flags)
  "Create either a simple property-get or an XML element-get.
We potentially need to examine quite a few tokens before we can be
sure which one to create, so we pass the state along to this helper
function.  NAMESPACE is nil or a string node."
  (let (pn pos)
    (if (and (null namespace) (zerop flags))
        (if (null target)
            name
          (prog1
              (setq pos (js2-node-pos target)
                    pn
                    (make-js2-prop-get-node :pos pos
                                            :len (- (js2-node-end name) pos)
                                            :target target
                                            :prop name))
            (js2-node-add-children pn target name)))
      (setq flags (set-flag flags js2-property-flag))
      (js2-create-member-ref-get target namespace name nil flags))))

(defsubst js2-create-element-get (target namespace elem flags)
  "Create either a simple element-get or an XML element-get.
ELEM is an expression node.  NAMESPACE is nil or a string node."
  (let (pn)
    (if (and (null namespace) (zerop flags))
        (prog1
            (setq pn (make-js2-elem-get-node :pos (js2-node-pos elem)
                                             :target target
                                             :prop elem))
          (js2-node-add-children pn target elem))
      (js2-create-member-ref-get target namespace nil elem flags))))

(defun js2-create-member-ref-get (target namespace name expr flags)
  (let ((node (make-js2-xml-ref-node :pos (js2-node-pos (or namespace name))
                                     :target target
                                     :namespace namespace
                                     :propname name
                                     :ref-expr expr
                                     :flags flags)))
    (js2-node-add-children node namespace name expr)
    node))

(defun js2-parse-property-name (pn name flags)
  "Check if :: follows name in which case it becomes qualified name.
PN is the target - the left side of the preceding '.' or '..' operator.
NAME is the identifier we just parsed, a `js2-name-node'.  In some
e4x/xml cases, it may not be a valid identifier (e.g. `*')."
  (let (namespace
        tt
        lb
        (pos (if pn (js2-node-pos pn))))
    (catch 'return
      (when (js2-match-token js2-COLONCOLON)
        (setq namespace (make-js2-string-node :value name)
              tt (js2-next-token))
        (cond
         ((js2-valid-prop-name-token tt) ; handles name::name
          (setq name (make-js2-string-node)))
         ((eq tt js2-MUL)                 ; handles name::*
          (setq name (make-js2-string-node :value "*")))
         ((eq tt js2-LB)                  ; handles name::[expr]
          (setq lb (- js2-token-beg pos)
                pn (js2-create-element-get pn
                                           namespace
                                           (js2-parse-expr)
                                           flags))
          (setf (js2-elem-get-node-lb pn) lb)
          (if (js2-must-match js2-RB "msg.no.bracket.index")
              (setf (js2-elem-get-node-rb pn) (- js2-token-beg pos)))
          (throw 'return pn))
         (t
          (js2-report-error "msg.no.name.after.coloncolon"))))
      (js2-create-property-get pn namespace name flags))))

(defun js2-parse-primary-expr ()
  (let* (pn  ; parent node
         (tt-flagged (js2-next-flagged-token))
         (tt (logand tt-flagged js2-clear-ti-mask))
         px-pos
         name
         name-pos
         name-end
         flags
         index
         expr)
    (cond
     ((eq tt js2-FUNCTION)
      (js2-parse-function 'FUNCTION_EXPRESSION))

     ((eq tt js2-LB)
      (js2-parse-array-literal))

     ((eq tt js2-LC)
      (js2-parse-object-literal))

     ((eq tt js2-LET)
      (js2-parse-let-stmt))

     ((eq tt js2-LP)
      (setq px-pos js2-token-beg
            expr (js2-parse-expr))
      (js2-must-match js2-RP "msg.no.paren")
      (setq pn (make-js2-paren-node :pos px-pos
                                    :expr expr
                                    :len (- js2-token-end px-pos)))
      (js2-node-add-children pn (js2-paren-node-expr pn))
      pn)

     ((eq tt js2-XMLATTR)
      (js2-must-have-xml)
      (js2-parse-attribute-access nil 0))

     ((eq tt js2-NAME)
      (setq name js2-ts-string
            name-pos js2-token-beg
            name-end js2-token-end)
      (if (and (flag-set-p tt-flagged js2-ti-check-label)
               (eq (js2-peek-token) js2-COLON))
          (prog1
            ;; Do not consume colon, it is used as unwind indicator
            ;; to return to statementHelper.
            (make-js2-label-node :pos name-pos
                                 :len (- js2-token-end name-pos)
                                 :label name)
            (js2-set-face name-pos
                          js2-token-end
                          'font-lock-variable-name-face 'record))
        ;; otherwise not a label, just a name
        (setq name (make-js2-name-node :name name
                                       :pos name-pos
                                       :len (- name-end name-pos)))
        (if js2-compiler-xml-available
            (js2-parse-property-name nil name 0)
          name)))

     ((eq tt js2-NUMBER)
      (make-js2-number-node))

     ((eq tt js2-STRING)
      (prog1
          (make-js2-string-node)
        (js2-set-face js2-token-beg js2-token-end 'font-lock-string-face 'record)))

     ((or (eq tt js2-DIV) (eq tt js2-ASSIGN_DIV))
      ;; Got / or /= which should be treated as regexp in fact
      (setq px-pos js2-token-beg)
      (js2-read-regexp tt)
      (setq flags js2-ts-regexp-flags
            js2-ts-regexp-flags nil)
      (prog1
          (make-js2-regexp-node :pos px-pos
                                :len (- js2-ts-cursor px-pos)
                                :value js2-ts-string
                                :flags flags)
        (js2-set-face px-pos js2-ts-cursor 'font-lock-string-face 'record)))
     ((or (eq tt js2-NULL)
          (eq tt js2-THIS)
          (eq tt js2-FALSE)
          (eq tt js2-TRUE))
      (make-js2-literal-node :type tt))

     ((eq tt js2-RESERVED)
      (js2-report-error "msg.reserved.id")
      (make-js2-name-node))

     ((eq tt js2-ERROR)
      ;; the scanner or one of its subroutines reported the error.
      (make-js2-error-node))

     ((eq tt js2-EOF)
      (js2-report-error "msg.unexpected.eof")
      (make-js2-error-node))

     (t
      (js2-report-error "msg.syntax")
      (make-js2-error-node)))))

(defsubst js2-parse-warn-trailing-comma (msg pos elems comma-pos)
  (js2-add-strict-warning
   msg nil
   ;; back up from comma to beginning of line or array/objlit
   (max (if elems
            (js2-node-pos (car elems))
          pos)
        (save-excursion
          (goto-char comma-pos)
          (back-to-indentation)
          (point)))
   comma-pos))

;; Pulled out of giant switch case in Parser.primaryExpr()
(defun js2-parse-array-literal ()
  (let ((pos js2-token-beg)
        (after-lb-or-comma t)
        after-comma
        tt
        elems
        result
        (continue t))
    (while continue
      (setq tt (js2-peek-token))
      (cond
       ((eq tt js2-COMMA)
        (js2-consume-token)
        (setq after-comma js2-ts-cursor)
        (if (not after-lb-or-comma)
            (setq after-lb-or-comma t)
          (push nil elems)))

       ((or (eq tt js2-RB)
            (eq tt js2-EOF))
        (if (eq tt js2-EOF)
            (js2-report-error "msg.no.bracket.arg" nil pos)
          (js2-consume-token))
        (setq continue nil
              result (make-js2-array-node :pos pos
                                          :len (- js2-ts-cursor pos)))
        (when after-comma
          (js2-parse-warn-trailing-comma "msg.array.trailing.comma"
                                         pos elems after-comma))
        (dolist (elem elems)
          (js2-node-add-children result elem)
          (push elem (js2-array-node-elems result))))

       ((and (>= js2-language-version 170)
             (eq tt js2-FOR)          ; check for array comprehension
             (not after-lb-or-comma) ; "for" can't follow a comma
             elems                   ; must have at least 1 element
             (not (cdr elems)))      ; but no 2nd element
        (setf continue nil
              result (js2-parse-array-comprehension (car elems) pos)))
       (t
        (unless after-lb-or-comma
          (js2-report-error "msg.no.bracket.arg"))
        (push (js2-parse-assign-expr) elems)
        (setq after-lb-or-comma nil
              after-comma nil))))
    result))

(defun js2-parse-array-comprehension (expr pos)
  "Parse a JavaScript 1.7 Array Comprehension.
EXPR is the first expression after the opening left-bracket.
POS is the beginning of the LB token preceding EXPR.
We should have just parsed the 'for' keyword before calling this function."
  (let (loops
        filter
        if-pos
        result
        (continue t))
    (while continue
      (if (eq (js2-peek-token) js2-FOR)
          (push (js2-parse-array-comp-loop) loops)
        (setq continue nil)))
    (when (eq (js2-peek-token) js2-IF)
      (js2-consume-token)
      (setq if-pos (- js2-token-beg pos)  ; relative
            filter (js2-parse-condition)))
    (js2-must-match js2-RB "msg.no.bracket.arg" pos)
    (setq result (make-js2-array-comp-node :pos pos
                                           :len (- js2-ts-cursor pos)
                                           :result expr
                                           :filter (car filter)
                                           :lp (js2-relpos (second filter) pos)
                                           :rp (js2-relpos (third filter) pos)
                                           :if-pos if-pos))
    (js2-node-add-children result expr (car filter))
    ;; reverse and append loops
    (dolist (lp loops)
      (push lp (js2-array-comp-node-loops result))
      (js2-node-add-children result lp))
    result))

(defun js2-parse-array-comp-loop ()
  "Parse a 'for [each] (foo in bar)' expression in an Array comprehension.
Last token peeked should be the initial FOR."
  (let ((pos js2-token-beg)
        (pn (make-js2-array-comp-loop-node))
        tt
        iter
        obj
        foreach-p
        in-pos
        each-pos
        lp
        rp)
  (unless (eq (js2-next-token) js2-FOR)  ; consumes token
    (js2-code-bug))   ; shouldn't be here if next token isn't 'for'

  (js2-enter-loop pn)
  (unwind-protect
      (progn
        (when (js2-match-token js2-NAME)
          (if (string= js2-ts-string "each")
              (progn
                (setq foreach-p t
                      each-pos (- js2-token-beg pos)) ; relative
                (js2-set-face js2-token-beg js2-token-end
                              'font-lock-keyword-face 'record))
            (js2-report-error "msg.no.paren.for")))
        (if (js2-must-match js2-LP "msg.no.paren.for")
            (setq lp (- js2-token-beg pos)))
        (setq tt (js2-peek-token))
        (cond
         ((or (eq tt js2-LB) (eq tt js2-LC))
          ;; handle destructuring assignment
          (setq iter (js2-parse-primary-expr)))
         ((eq tt js2-NAME)
          (js2-consume-token)
          (setq iter (make-js2-name-node)))
         (t
          (js2-report-error "msg.bad.var")))

        ;; Define as a let since we want the scope of the variable to
        ;; be restricted to the array comprehension
        (if (js2-name-node-p iter)
            (js2-define-symbol js2-LET (js2-name-node-name iter) pn))

        (if (js2-must-match js2-IN "msg.in.after.for.name")
            (setq in-pos (- js2-token-beg pos)))
        (setq obj (js2-parse-expr))
        (if (js2-must-match js2-RP "msg.no.paren.for.ctrl")
            (setq rp (- js2-token-beg pos)))
        (setf (js2-node-pos pn) pos
              (js2-node-len pn) (- js2-ts-cursor pos)
              (js2-array-comp-loop-node-iterator pn) iter
              (js2-array-comp-loop-node-object pn) obj
              (js2-array-comp-loop-node-in-pos pn) in-pos
              (js2-array-comp-loop-node-each-pos pn) each-pos
              (js2-array-comp-loop-node-foreach-p pn) foreach-p
              (js2-array-comp-loop-node-lp pn) lp
              (js2-array-comp-loop-node-rp pn) rp)
        (js2-node-add-children pn iter obj))
    (js2-exit-loop))
  pn))

(defun js2-parse-object-literal ()
  (let ((pos js2-token-beg)
        tt
        elems
        result
        prop
        node
        ppos
        pend
        expr
        get-or-set
        after-comma
        (continue t))
    (while continue
      (setq tt (js2-peek-token))
      (cond
       ((or (js2-valid-prop-name-token tt)
            (eq tt js2-STRING))
        (js2-consume-token)
        (setq after-comma nil
              ppos js2-token-beg
              pend js2-token-end
              prop js2-ts-string
              node (make-js2-name-node))
        (if (not (and (eq tt js2-NAME)
                      (eq (js2-peek-token) js2-NAME)
                      (or (string= prop "get")
                          (string= prop "set"))))
            (progn
              (setq expr (js2-parse-plain-property node))
              (js2-set-face ppos pend
                            (if (js2-function-node-p
                                 (js2-object-prop-node-right expr))
                                'font-lock-function-name-face
                              'font-lock-variable-name-face)
                            'record)
              (push expr elems))
          (js2-consume-token)
          (js2-set-face ppos pend 'font-lock-keyword-face 'record)
          (js2-set-face js2-token-beg js2-token-end
                        'font-lock-function-name-face 'record)
          (setq get-or-set (make-js2-string-node :pos ppos
                                                 :len (- pend ppos)
                                                 :value prop)
                result (js2-parse-getter-setter-prop (make-js2-name-node)
                                                     get-or-set))
          (when (and (null result)
                     (not js2-recover-from-parse-errors))
            (setq continue nil))
          (push result elems)))

       ((eq tt js2-NUMBER)
        (js2-consume-token)
        (setq after-comma nil)
        (push (js2-parse-plain-property (make-js2-number-node)) elems))

       ;; trailing comma
       ((eq tt js2-RC)
        (setq continue nil)
        (if after-comma
            (js2-parse-warn-trailing-comma "msg.extra.trailing.comma"
                                           pos elems after-comma)))
       (t
        (js2-report-error "msg.bad.prop")
        (unless js2-recover-from-parse-errors
          (setq continue nil))))  ; end switch

      (if (js2-match-token js2-COMMA)
          (setq after-comma js2-token-end)
        (setq continue nil))) ; end loop

    (js2-must-match js2-RC "msg.no.brace.prop")
    (setq result (make-js2-object-node :pos pos
                                       :len (- js2-ts-cursor pos)))
    (dolist (e elems)
      (js2-node-add-children result e)
      (push e (js2-object-node-elems result)))
    result))

(defun js2-parse-plain-property (prop)
  "Parse a non-getter/setter property in an object literal.
PROP is the node representing the property:  a number, name or string."
  (let ((pos (js2-node-pos prop))
        colon
        result
        expr)
    (js2-must-match js2-COLON "msg.no.colon.prop")
    (setq colon (- js2-token-beg pos)
          expr (js2-parse-assign-expr)
          result (make-js2-object-prop-node
                  :pos pos
                  ;; don't include last consumed token in length
                  :len (- (+ (js2-node-pos expr)
                             (js2-node-len expr))
                          pos)
                  :left prop
                  :right expr
                  :op-pos colon))
    (js2-node-add-children result prop expr)
    result))

(defun js2-parse-getter-setter-prop (prop get-or-set)
  "Parse getter or setter property in object literal.
PROP is the `js2-name-node' representing the property name.
GET-OR-SET is a `js2-string-node' representing the get/set keyword."
  (let ((f (js2-parse-function 'FUNCTION_EXPRESSION))
        result
        (pos (js2-node-pos get-or-set))
        (type (if (string= "get" (js2-string-node-value get-or-set))
                  js2-GET
                js2-SET)))
    (if (/= (js2-node-type f) js2-FUNCTION)
        (js2-report-error "msg.bad.prop")
      (if (plusp (length (js2-function-name f)))
          (js2-report-error "msg.bad.prop")))
    (js2-node-set-prop f 'GETTER_SETTER type)
    (setq result (make-js2-getter-setter-node :type type
                                              :pos pos
                                              :len (- js2-ts-cursor pos)
                                              :left prop
                                              :right f
                                              :kwd get-or-set))
    (js2-node-add-children result prop f get-or-set)
    result))

(provide 'js2-parse)

;;; js2-parse.el ends here
