;;; go-setup.el -*- lexical-binding: t; -*-

;; Basic Go configuration with eglot LSP support

;; Built-in Go tree-sitter mode (Emacs 29+)
(when (treesit-language-available-p 'go)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-hook 'go-ts-mode-hook #'eglot-ensure)
  (add-hook 'go-ts-mode-hook #'flymake-mode)
  (add-hook 'go-ts-mode-hook (lambda ()
                               ;; Go uses tabs by convention
                               (setq-local tab-width 4
                                          indent-tabs-mode t))))

;; Fallback Go mode for older Emacs
(unless (treesit-language-available-p 'go)
  (use-package go-mode
    :ensure t
    :mode "\\.go\\'"
    :hook ((go-mode . eglot-ensure)
           (go-mode . flymake-mode))
    :config
    (add-hook 'go-mode-hook (lambda ()
                              ;; Go uses tabs by convention
                              (setq-local tab-width 4
                                         indent-tabs-mode t)))))

;; Configure gopls LSP server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((go-mode go-ts-mode) . ("gopls"))))

;; Basic file associations
(add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("go\\.sum\\'" . fundamental-mode))

(provide 'go-setup)

;;; config/lang/go.el ends here
