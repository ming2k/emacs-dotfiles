;;; init-cc.el -*- lexical-binding: t; -*-

;; C mode configuration
(add-hook 'c-ts-mode-hook
          (lambda ()
            (setq-local c-basic-offset 4
                        tab-width 4
                        indent-tabs-mode nil)))

;; C++ mode configuration
(add-hook 'c++-ts-mode-hook
          (lambda ()
            (setq-local c-basic-offset 4
                        tab-width 4
                        indent-tabs-mode nil)))

;; Enable eglot
(add-hook 'c-ts-mode-hook #'eglot-ensure)
(add-hook 'c++-ts-mode-hook #'eglot-ensure)

(provide 'init-cc)
