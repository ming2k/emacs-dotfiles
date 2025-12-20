;;; init-nushell.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Nushell scripting support
;;; Code:

;; Nushell mode for .nu files
(use-package nushell-mode
  :ensure t
  :mode "\\.nu\\'"
  :config
  (setq nushell-indent-offset 2))

;; Nushell-specific eglot configuration
;; Note: nushell LSP server (nuls) can be installed with: cargo install nuls
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(nushell-mode . ("nuls")))

  ;; Optionally enable eglot for nushell if nuls is available
  ;; Uncomment the line below to auto-enable LSP
  ;; (add-hook 'nushell-mode-hook 'eglot-ensure)
  )

(provide 'init-nushell)

;;; init-nushell.el ends here
