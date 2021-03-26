;;; daedalus.el --- A major mode for editing DaeDaLus files  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Galois, Inc.

;; Author: David Thrane Christiansen <dtc@galois.com>
;; Keywords: languages

;;; Commentary:

;;

;;; Code:

(defgroup daedalus '()
  "Settings for DaeDaLus"
  :group 'languages)

(defcustom daedalus-command "daedalus"
  "How to invoke DaeDaLus."
  :group 'daedalus
  :type 'command
  :options (list "daedalus"
                 "cabal v2-run exe:daedalus --"))

(defvar daedalus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'daedalus-check-current-buffer)
    (define-key map (kbd "C-c C-i") 'daedalus-interpret-current-buffer)
    map)
  "The keymap used in `daedalus-mode'.")

(defun daedalus--buffer-file-name (buf)
  "Get the file name that BUF is visiting, or fail otherwise.

BUF may be a buffer name or a buffer."
  (let ((the-buffer (get-buffer buf)))
    (unless the-buffer
      (error "Buffer not found"))
    (let ((file-name (buffer-file-name the-buffer)))
      (unless file-name
        (error "Buffer is not visiting a file"))
      file-name)))

(defun daedalus-check-buffer (buf)
  "Check BUF, which may be a buffer or the name of a buffer, using daedalus.

Customize the variable `daedalus-command' to change how it is invoked."
  (interactive "bBuffer: ")
  (let* ((file-name (daedalus--buffer-file-name buf))
         (compilation-buffer-name-function (lambda (_mode-name) "*DaeDaLus*")))
    (compile (concat daedalus-command " " file-name))))

(defun daedalus-check-current-buffer ()
  "Check the current buffer with DaeDaLus."
  (interactive)
  (daedalus-check-buffer (current-buffer)))

(defun daedalus-interpret-buffer (buf file)
  "Check BUF and use its contents to interpret FILE.

Customize the variable `daedalus-command' to change how it is invoked."
  (interactive "bBuffer: \nfInput file: ")
  (let* ((file-name (daedalus--buffer-file-name buf))
         (compilation-buffer-name-function (lambda (_mode-name) "*DaeDaLus*")))
    (compile (concat daedalus-command " " file-name " --interp " file))))

(defun daedalus-interpret-current-buffer (file)
  "Check BUF and use its contents to interpret FILE.

Customize the variable `daedalus-command' to change how it is invoked."
  (interactive "fInput file: ")
  (let* ((file-name (daedalus--buffer-file-name (current-buffer)))
         (compilation-buffer-name-function (lambda (_mode-name) "*DaeDaLus*")))
    (compile (concat daedalus-command " " file-name " --interp " file))))

(defconst daedalus-keywords
  ;; These are based on the contents of
  ;;    src/Daedalus/Parser/Lexer.x .
  ;; Please maintain the same ordering and grouping here as there for
  ;; easy updates.
  (list "import" "def" "block" "let" "for" "map" "in" "is" "if" "try" "then" "else" "as" "as!"
        "Choose" "Choose1" "Optional" "Optional?" "Many" "Many?" "UInt8" "$uint" "END" "commit" "Fail"
        "empty" "Insert" "Lookup"
        "Offset" "SetStream" "GetStream" "Take" "Drop" "arrayStream"
        "Index" "concat" "length" "rangeUp" "rangeDown"
        "true" "false"
        "just" "nothing"
        "int" "uint" "sint" "bool" "maybe" "stream")
  "Keywords to highlight in `daedalus-mode'.")

(defconst daedalus-syntax-table
  (let ((s (make-syntax-table)))
    ;; Make motion commands work with the various keywords we have
    (modify-syntax-entry ?! "w" s)
    (modify-syntax-entry ?$ "w" s)
    (modify-syntax-entry ?? "w" s)
    (cl-loop for ch in "1234567890"
             do (modify-syntax-entry ch "w" s))

    ;; Matching parens
    (modify-syntax-entry ?\( "()" s)
    (modify-syntax-entry ?\) ")(" s)
    (modify-syntax-entry ?\[ "(]" s)
    (modify-syntax-entry ?\] ")[" s)

    ;; Matching {} needs fancy comment work
    (modify-syntax-entry ?\{ "(} 1bn" s)
    (modify-syntax-entry ?\} "){ 4bn" s)
    (modify-syntax-entry ?\n ">" s)

    ;; - is an operator char but may also be 1st or 2nd char of comment starter
    ;; -- and the 1st char of comment end -}
    (modify-syntax-entry ?\- ". 123" s)

    ;; Whitespace
    (modify-syntax-entry ?\  " " s)
    (modify-syntax-entry ?\t " " s)

    ;; Strings
    (modify-syntax-entry ?\" "\"" s)
    (modify-syntax-entry ?\\ "/" s)

    (modify-syntax-entry ?' "\"" s)
    s))

(defconst daedalus--kw-regexp (regexp-opt daedalus-keywords 'words)
  "Regexp matching DaeDaLus keywords for use in highlighting.")

(defconst daedalus--magic-symbol-regexp (regexp-opt '("$$" "@" "^" ".." ";"))
  "Regexp matching special DaeDaLus magic symbols.")

(defconst daedalus--big-ident-regexp "\\<[A-Z][a-zA-Z0-9]*\\>")
(defconst daedalus--small-ident-regexp "\\<[a-z][a-zA-Z0-9]*\\>")
(defconst daedalus--set-ident-regexp "\\<\\$[a-z][a-zA-Z0-9]*\\>")

(defconst daedalus-font-lock-defaults
  `((,daedalus--kw-regexp . font-lock-keyword-face)
    (,daedalus--magic-symbol-regexp . font-lock-builtin-face)
    (,daedalus--big-ident-regexp . font-lock-function-name-face)
    (,daedalus--small-ident-regexp . font-lock-variable-name-face)
    (,daedalus--set-ident-regexp . font-lock-reference-face)))

(define-derived-mode daedalus-mode prog-mode "DaeDaLus"
  "Major mode for editing DaeDaLus files"
  :group 'daedalus
  :syntax-table daedalus-syntax-table
  (setq font-lock-defaults '((daedalus-font-lock-defaults))))

(add-to-list 'auto-mode-alist (cons "\\.ddl\\'" 'daedalus-mode))

(provide 'daedalus)
;;; daedalus.el ends here
