;;; init-typescript.el -*- lexical-binding: t; -*-

;; TypeScript mode configuration
(add-hook 'typescript-ts-mode-hook
          (lambda ()
            (setq-local typescript-ts-mode-indent-offset 2
                        tab-width 2
                        indent-tabs-mode nil)))

(add-hook 'tsx-ts-mode-hook
          (lambda ()
            (setq-local typescript-ts-mode-indent-offset 2
                        tab-width 2
                        indent-tabs-mode nil)))

;; Enable eglot
(add-hook 'typescript-ts-mode-hook #'eglot-ensure)
(add-hook 'tsx-ts-mode-hook #'eglot-ensure)

(provide 'init-typescript)
