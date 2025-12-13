;;; init-cc.el -*- lexical-binding: t; -*-

;; Use tree-sitter modes
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))

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
