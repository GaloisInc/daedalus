;;; lsp-daedalus.el --- lsp-mode daedalus integration    -*- lexical-binding: t; -*-

(require 'lsp-mode)

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
                  :server-id 'daedalus-lsp))


(add-to-list 'lsp-language-id-configuration '(daedalus-mode . "daedalus"))


;; (setq lsp-daedalus-server-path "/Users/sjw/galois/safedocs/daedalus/daedalus-language-server/server-wrapper.sh")

;; (lsp-consistency-check lsp-daedalus)

(add-hook 'daedalus-mode-hook #'lsp)

(provide 'lsp-daedalus)
