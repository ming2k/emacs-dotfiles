;;; config/lang/javascript.el -*- lexical-binding: t; -*-
;;; Commentary:
;; JavaScript configuration using tree-sitter when available
;;; Code:

;; JavaScript settings
(setq js-indent-level 2
      js-switch-indent-offset 2)

;; Tree-sitter JavaScript mode (preferred)
(use-package js-ts-mode
  :ensure nil
  :when (treesit-language-available-p 'javascript)
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode)
         ("\\.mjs\\'" . js-ts-mode))
  :hook ((js-ts-mode . js-setup-minor-modes)
         (js-ts-mode . eglot-ensure)
         (js-ts-mode . flymake-mode))
  :config
  (setq js-indent-level 2))

;; Fallback to regular js-mode if tree-sitter is not available
(unless (treesit-language-available-p 'javascript)
  (use-package js
    :ensure nil
    :mode (("\\.js\\'" . js-mode)
           ("\\.jsx\\'" . js-mode)
           ("\\.mjs\\'" . js-mode))
    :hook ((js-mode . js-setup-minor-modes)
           (js-mode . eglot-ensure)
           (js-mode . flymake-mode))
    :config
    (setq js-indent-level 2
          js-switch-indent-offset 2)))

;; JavaScript minor modes setup
(defun js-setup-minor-modes ()
  "Enable helpful minor modes for JavaScript."
  (electric-indent-local-mode 1)
  (subword-mode 1)
  (hs-minor-mode 1)
  (setq-local comment-auto-fill-only-comments t
              tab-width 2
              indent-tabs-mode nil))

;; Configure JavaScript LSP server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((js-mode js-ts-mode) . ("typescript-language-server" "--stdio"))))

(provide 'javascript-setup)

;;; config/lang/javascript.el ends here