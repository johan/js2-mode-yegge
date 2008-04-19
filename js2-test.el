;;; js2-test.el -- JavaScript parser tests

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

(require 'js2-parse)

;; run rhino interpreter:
;;  cd /java/rhino/src/mozilla/js
;;  java -jar ./rhino/build/rhino1_7R1pre/js.jar -version 170

(defun js2-load ()
  (interactive)
  (find-file-noselect
   "/java/rhino/src/mozilla/js/rhino/src/org/mozilla/javascript/Parser.java"))

(defun js2-macload ()
  (interactive)
  (find-file-noselect
   (concat emacs-root
           "src/mozilla/js/rhino/src/org/mozilla/javascript/Parser.java")))

(defun js2-workload ()
  (interactive
   (find-file-noselect
    "/home/stevey/no_crawl/src/js/rhino/src/org/mozilla/javascript/Parser.java")))

(defun js2-mode-load ()
  (interactive)
  (let ((files (find-file-noselect
                (concat emacs-root "emacs/es4/js2-*.el") t nil t)))
    (dolist (buf files)
      (unless (member (buffer-name buf) '("js2-interp.el" "js2-ir.el"))
        (message "evaluating %s" buf)
        (eval-buffer buf)
        (save-excursion
          (set-buffer buf)
          (goto-char (point-min)))))))
  
(defun js2-test-check-parents (buf)
  "Parse the buffer and check that all nodes have parent links."
  (let ((ast (js2-parse buf)))
    (save-excursion
      (set-buffer buf)
      (js2-visit-ast ast #'js2-test-parent-checker))))

(defun js2-test-parent-checker (n end-p)
  (unless end-p
    (or (js2-ast-root-p n)
        (js2-parent-node n)
        (error "no parent for %s" n))))
        
(defun js2-print-quick-ast (ast-or-buf)
  "Print a short representation of AST-or-BUF."
  (let* ((ast (if (js2-ast-root-p ast-or-buf)
                  ast-or-buf
                (js2-parse ast-or-buf)))
         (max-lisp-eval-depth (max max-lisp-eval-depth 1500)))
    (js2-visit-ast ast #'js2-quick-ast-printer)
    (dolist (err (js2-ast-root-errors ast))
      (insert (format "%s\n" err)))))

(defalias 'js2-quick-print-ast 'js2-print-quick-ast)
    
(defun js2-node-depth (node)
  "Return number of steps from ast root for NODE."
  (if (js2-ast-root-p node)
      0
    (1+ (js2-node-depth
         (js2-node-parent node)))))

(defun js2-quick-ast-printer (n end-p)
  (unless end-p
    (let ((indent (js2-node-depth n))
          (name (js2-node-short-name n))
          (pos (js2-node-pos n))
          (len (js2-node-len n)))
      (insert (number-to-string (js2-node-abs-pos n)) "\t")
      (insert (make-string (* 2 indent) ? ))
      (insert name " "
              (number-to-string pos) " "
              (number-to-string len))
      (if (js2-name-node-p n)
          (insert " " (js2-node-string n)))
      (insert "\n")
      t)))

(defun cg ()
  (interactive)
  (if (string= mode-name "JavaScript[Js2]")
      (message "%s" (js2-syntax-at-point))
    (message "%s" (c-guess-basic-syntax))))

(defun js2-test-scanner (&optional bufname)
  "Smoke test of the scanner.
Make sure you byte-compile first or it'll be way slow."
  (interactive)
  (let ((buf (get-buffer (or bufname "lang.js")))
        tok
        (count 0))
    (js2-init-scanner buf)
    (save-excursion
      (set-buffer buf)
      (while (not (eq (setq tok (js2-get-token)) js2-EOF))
        ;; Use heuristic for distinguishing regexps -
        ;; assume my test files always have a space after
        ;; div operators and never as first char in regexp.
        (if (and (memq tok (list js2-DIV js2-ASSIGN_DIV))
                 (not (memq (js2-peek-char) '(?  ?\n))))
            (js2-read-regexp tok))
        (goto-char js2-ts-cursor)
        (incf count))
      (message "%d tokens" count))))

;;; stuff for dealing with my Rhino parser rewrite

(defun rhino-load ()
  (interactive)
   (find-file
    "/home/stevey/trunk/google3/third_party/java/rhino/src/mozilla/js/rhino/src/org/mozilla/javascript/ast/Parser.java"))

(defun rhino-compile-setup ()
  (interactive)
  (require 'compile)
  (set (make-local-variable 'compile-command)
       "cd /home/stevey/trunk/google3/third_party/java/rhino/src/mozilla/js/rhino && ant -emacs compile-parser")
  (message "compile command set for rhino"))

(defalias 'rcs 'rhino-compile-setup)

(defvar rhino-jdb-port 5005)

(defvar rhino-jdb-root "/home/stevey/trunk/google3/third_party/java/rhino/src/mozilla/js/rhino")

(defvar rhino-java-class-path
  '("/home/stevey/trunk/google3/third_party/java/rhino/src/mozilla/js/rhino/build/classes"))

(defvar rhino-source-path
  '("/home/stevey/trunk/google3/third_party/java/rhino/src/mozilla/js/rhino/src/"))

;; alt root:  /home/stevey/no_crawl/src/js/rhino

(defun rhino-jdb ()
  "Start JDB and attach it to a running process on `rhino-jdb-port'.
Sets source path to `rhino-jdb-root', where the rhino build.xml lives."
  (interactive)
  (require 'gud)
  (let ((jdb-command
         (format "%s -attach %s -sourcepath%s"
                 gud-jdb-command-name
                 rhino-jdb-port
                 (mapconcat #'identity (append rhino-source-path
                                               rhino-java-class-path)
                            ":"))))
    (if (not (string= jdb-command (car gud-jdb-history)))
        (push jdb-command gud-jdb-history))
    (jdb jdb-command)))


(provide 'js2-test)

;;; js2-test.el ends here
