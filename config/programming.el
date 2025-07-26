;; =============================================================================
;; config/programming.el - General programming settings
;; =============================================================================
;;; Commentary:
;; General programming configuration
;;; Code:

;; Eglot - LSP client
(use-package eglot
  :ensure t
  :hook ((python-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (web-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t
        eglot-confirm-server-initiated-edits nil))

;; Tree-sitter
(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;; Flycheck
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe))

;; Company (alternative to corfu)
;; (use-package company
;;   :ensure t
;;   :init
;;   (global-company-mode)
;;   :config
;;   (setq company-idle-delay 0.0
;;         company-minimum-prefix-length 1
;;         company-selection-wrap-around t))

(provide 'programming)
;;; programming.el ends here
