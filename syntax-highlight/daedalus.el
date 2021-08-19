;;; daedalus.el --- A major mode for editing DaeDaLus files  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Galois, Inc.

;; Author: David Thrane Christiansen <dtc@galois.com>
;; Keywords: languages

;;; Commentary:

;;

;;; Code:
(require 'lsp-mode)
(require 'dash)

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
    (define-key map (kbd "C-c C-c") 'lsp-daedalus-run)
    (define-key map (kbd "C-c C-w") 'lsp-daedalus-watch)
    (define-key map (kbd "C-c C-e") 'lsp-daedalus-regions)
    
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

;; (defconst daedalus--kw-regexp (regexp-opt daedalus-keywords 'words)
;;   "Regexp matching DaeDaLus keywords for use in highlighting.")

;; (defconst daedalus--magic-symbol-regexp (regexp-opt '("$$" "@" "^" ".." ";"))
;;   "Regexp matching special DaeDaLus magic symbols.")

;; (defconst daedalus--big-ident-regexp "\\<[A-Z][a-zA-Z0-9]*\\>")
;; (defconst daedalus--small-ident-regexp "\\<[a-z][a-zA-Z0-9]*\\>")
;; (defconst daedalus--set-ident-regexp "\\<\\$[a-z][a-zA-Z0-9]*\\>")

;; (defconst daedalus-font-lock-defaults
;;   `((,daedalus--kw-regexp . font-lock-keyword-face)
;;     (,daedalus--magic-symbol-regexp . font-lock-builtin-face)
;;     (,daedalus--big-ident-regexp . font-lock-function-name-face)
;;     (,daedalus--small-ident-regexp . font-lock-variable-name-face)
;;     (,daedalus--set-ident-regexp . font-lock-reference-face)))

(define-derived-mode daedalus-mode prog-mode "DaeDaLus"
  "Major mode for editing DaeDaLus files"
  :group 'daedalus
  :syntax-table daedalus-syntax-table
  (setq font-lock-defaults '(()))
  (setq-local lsp-semantic-tokens-enable t) ;; use lsp for fontification
  )

;; We use LSP for fontification
;; (setq font-lock-defaults '((daedalus-font-lock-defaults))))

(add-to-list 'auto-mode-alist (cons "\\.ddl\\'" 'daedalus-mode))

;; LSP setup

(defgroup lsp-daedalus nil
  "LSP support for Daedalus."
  :group 'lsp-mode
  :link '(url-link "https://github.com/GaloisInc/Daedalus"))

(defcustom lsp-daedalus-server-path "daedalus-language-server"
  "Executable path for the server."
  :group 'lsp-daedalus
  :type 'string
  :package-version '(lsp-mode . "7.1"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-daedalus-server-path))
                  :major-modes '(daedalus-mode)
		  :notification-handlers (ht ("daedalus/run/watchResult" 'lsp-daedalus--run-result-notfication))
                  :server-id 'daedalus-lsp))

(add-to-list 'lsp-language-id-configuration '(daedalus-mode . "daedalus"))

;; (lsp-consistency-check lsp-daedalus)

(add-hook 'daedalus-mode-hook #'lsp)


;; LSP commands

;; Running the definition under the cursor

(defun lsp-daedalus-run (p)
  (interactive "d")
  (let* ((args (vector (lsp-text-document-identifier) (lsp-point-to-position p)))
	 (res (lsp-send-execute-command "run" args)))
    (display-buffer
     (with-current-buffer (get-buffer-create "*LSP Daedalus Results*")
       (lsp-daedalus-watch-mode)
       (lsp-daedalus--watch-update (current-buffer) res)
       (current-buffer)))))
			    
(defun lsp-daedalus-watch (p) 
  (interactive "d")
  (let* ((buf (generate-new-buffer "*LSP Daedalus Watch*"))
	 (args (vector (lsp-text-document-identifier) (lsp-point-to-position p) (buffer-name buf)))
	 (res (lsp-send-execute-command "run/watch" args))
	 (workspaces (lsp-workspaces)))
    (with-current-buffer buf
      (lsp-daedalus-watch-mode)
      (setq-local lsp-daedalus--watch-tag res)
      ;; This seems risky?  I don't know how workspaces are managed.
      (setq-local lsp-daedalus--workspaces workspaces)
      )))
    
;; (lsp-defun lsp-daedalus--run-result-notfication (_workspace (&hash :clientHandle buf :result msg))
;;   (display-buffer
;;    (with-current-buffer buf
;;      (erase-buffer)
;;      (insert msg)
;;      (current-buffer))))

(defvar lsp-daedalus-watch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'lsp-daedalus-watch-quit)
    map)

  "Keymap for `lsp-daedalus-watch-mode'")

(define-derived-mode lsp-daedalus-watch-mode special-mode "Daedalus Results"
  "A mode for showing the results of a Daedalus run"
  :keymap  lsp-daedalus-watch-mode-map
  (add-hook (make-local-variable 'kill-buffer-hook) 'lsp-daedalus--watch-cancel)
  )

;; Called in the buffer to quit, which is also in a window (otherwise we couldn't call this)
(defun lsp-daedalus-watch-quit ()
  (interactive)
  (lsp-daedalus--watch-cancel)
  (quit-window t))

;; Needs to be run in the watch buffer.
(defun lsp-daedalus--watch-cancel ()
  (when (boundp 'lsp-daedalus--watch-tag)
    (with-lsp-workspaces lsp-daedalus--workspaces
      (let* ((args (vector lsp-daedalus--watch-tag)))
	(lsp-send-execute-command "run/cancel" args))
      (makunbound 'lsp-daedalus--watch-tag)
      )))

(defun lsp-daedalus--watch-update (buf res)
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert res))))

(defun lsp-daedalus--run-result-notfication (workspace params)
  (-let [(&hash "clientHandle" buf "result" msg) params]
    (when (buffer-live-p (get-buffer buf))
      (lsp-daedalus--watch-update buf msg)
      (display-buffer buf))))

;; Setting the region over surrounding expressions

(defun lsp-daedalus-regions (p)
  (interactive "d")
  (let* ((args (vector (lsp-text-document-identifier) (lsp-point-to-position p)))
	 (ranges (lsp-send-execute-command "positionToRegions" args)))
    (lsp-daedalus-regions-mode)
    (lsp-daedalus--region-set-list ranges)
    (lsp-daedalus--region-update)))
    
;; Following isearch mode for its modality
(defvar-local lsp-daedalus--region-list nil
  "The list of the regions in the currently active selection")

(defvar-local lsp-daedalus--region-index 0
  "The index of the currently selected region")

(defun lsp-daedalus--region-set-list (rs)
  (setq lsp-daedalus--region-list rs
	lsp-daedalus--region-index 0))

(defun lsp-daedalus--region-dec ()
  (when (> lsp-daedalus--region-index 0)
    (setq lsp-daedalus--region-index (- lsp-daedalus--region-index 1))))

(defun lsp-daedalus--region-inc ()
  (when (< lsp-daedalus--region-index (- (length lsp-daedalus--region-list) 1))
    (setq lsp-daedalus--region-index (+ lsp-daedalus--region-index 1))))

(defun lsp-daedalus--region-update ()
  (let ((r (lsp--range-to-region (lsp-elt lsp-daedalus--region-list lsp-daedalus--region-index))))
    (set-mark (cdr r))
    (goto-char (car r))))

(defvar lsp-daedalus-regions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-e" 'lsp-daedalus-region-expand)
    (define-key map "\C-r" 'lsp-daedalus-region-contract)
    map)

  "Keymap for `lsp-daedalus-regions-mod'")

(defun lsp-daedalus--region-pre-command-hook ()
  "Figure out if we need to exit the region mode."
  (let* ((key (this-single-command-keys))
	 (main-event (aref key 0)))
    (cond
     ((commandp (lookup-key lsp-daedalus-regions-mode-map key nil)))
     (t (lsp-daedalus--region-done)))))

(define-minor-mode lsp-daedalus-regions-mode
  "A mode for selecting regions around an expression via Daedalus LSP"
  :init-value nil
  :lighter " Regions"
  :keymap  lsp-daedalus-regions-mode-map
  
  ;; c.f. isearch-mode
  (add-hook 'pre-command-hook 'lsp-daedalus--region-pre-command-hook) ;; to exit the mode if required
  (add-hook 'kbd-macro-termination-hook 'lsp-daedalus--region-done)

  ;; Do we need this?
  ;;(recursive-edit))
  )

(defun lsp-daedalus--region-done ()
  "Clean up after Daedalus LSP regions"
  (message "... done")
  (remove-hook 'pre-command-hook 'lsp-daedalus--region-pre-command-hook) ;; to exit the mode if required
  (remove-hook 'kbd-macro-termination-hook 'lsp-daedalus--region-done)
  (lsp-daedalus--region-set-list nil)
  (lsp-daedalus-regions-mode -1)
  )

(defun lsp-daedalus-region-expand ()
  (interactive)
  (lsp-daedalus--region-inc)
  (lsp-daedalus--region-update))

(defun lsp-daedalus-region-contract()
  (interactive)
  (lsp-daedalus--region-dec)
  (lsp-daedalus--region-update))

(provide 'daedalus)
;;; daedalus.el ends here
