;;; init-javascript.el -*- lexical-binding: t; -*-

;; JavaScript mode configuration
(add-hook 'js-ts-mode-hook
          (lambda ()
            (setq-local js-indent-level 2
                        tab-width 2
                        indent-tabs-mode nil)))

;; Enable eglot
(add-hook 'js-ts-mode-hook #'eglot-ensure)

(provide 'init-javascript)