;;; go-setup.el -*- lexical-binding: t; -*-

;; Basic Go configuration with eglot LSP support

;; Go minor modes setup
(defun go-setup-minor-modes ()
  "Enable helpful minor modes for Go."
  ;; Go uses tabs by convention
  (setq-local tab-width 4
              indent-tabs-mode t
              fill-column 100))

;; Tree-sitter Go mode (preferred)
(use-package go-ts-mode
  :ensure nil
  :when (treesit-language-available-p 'go)
  :mode "\\.go\\'"
  :hook ((go-ts-mode . eglot-ensure)
         (go-ts-mode . flymake-mode)
         (go-ts-mode . go-setup-minor-modes)))

;; Fallback Go mode when tree-sitter is not available
(unless (treesit-language-available-p 'go)
  (use-package go-mode
    :ensure t
    :mode "\\.go\\'"
    :hook ((go-mode . eglot-ensure)
           (go-mode . flymake-mode)
           (go-mode . go-setup-minor-modes))))

;; Configure gopls LSP server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((go-mode go-ts-mode) . ("gopls"))))

;; Basic file associations
(if (treesit-language-available-p 'gomod)
    (add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-mod-ts-mode))
  (add-to-list 'auto-mode-alist '("go\\.mod\\'" . conf-mode)))
(add-to-list 'auto-mode-alist '("go\\.sum\\'" . fundamental-mode))

(provide 'go-setup)

;;; config/lang/go.el ends here
