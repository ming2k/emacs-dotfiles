;;; init-go.el -*- lexical-binding: t; -*-

;; Go mode configuration
(add-hook 'go-ts-mode-hook
          (lambda ()
            (setq-local tab-width 4
                        indent-tabs-mode t
                        fill-column 100)))

;; Enable eglot
(add-hook 'go-ts-mode-hook #'eglot-ensure)

;; Go module files
(add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-mod-ts-mode))

(provide 'init-go)
