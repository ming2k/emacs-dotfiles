;;; config/lang/json.el -*- lexical-binding: t; -*-
;;; Commentary:
;; JSON configuration using built-in packages only
;;; Code:

;; Built-in JSON tree-sitter mode (Emacs 29+)
(use-package json-ts-mode
  :ensure nil
  :mode (("\\.json\\'" . json-ts-mode)
         ("package\\.json\\'" . json-ts-mode)
         ("\\.babelrc\\'" . json-ts-mode)
         ("\\.eslintrc\\'" . json-ts-mode)
         ("tsconfig\\.json\\'" . json-ts-mode))
  :when (treesit-language-available-p 'json)
  :hook (json-ts-mode . json-setup-minor-modes)
  :config
  (setq json-ts-mode-indent-offset 2))

;; Fallback to built-in js-mode for JSON files when tree-sitter is not available
(unless (treesit-language-available-p 'json)
  (add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))
  (add-to-list 'auto-mode-alist '("package\\.json\\'" . js-mode))
  (add-to-list 'auto-mode-alist '("\\.babelrc\\'" . js-mode))
  (add-to-list 'auto-mode-alist '("\\.eslintrc\\'" . js-mode))
  (add-to-list 'auto-mode-alist '("tsconfig\\.json\\'" . js-mode)))

;; JSON minor modes setup
(defun json-setup-minor-modes ()
  "Enable helpful minor modes for JSON."
  (electric-indent-local-mode 1)
  (hs-minor-mode 1)
  (flyspell-prog-mode)
  (setq-local tab-width 2
              indent-tabs-mode nil
              js-indent-level 2))

(provide 'json)

;;; config/lang/json.el ends here