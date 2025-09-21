;;; json-setup.el --- JSON mode configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; JSON mode configuration with flyspell disabled
;;; Code:

;; Tree-sitter JSON mode (preferred)
(use-package json-ts-mode
  :ensure nil
  :when (treesit-language-available-p 'json)
  :mode (("\\.json\\'" . json-ts-mode)
         ("\\.jsonl\\'" . json-ts-mode)
         ("\\.jsonc\\'" . json-ts-mode))
  :hook ((json-ts-mode . json-setup-minor-modes)
         (json-ts-mode . eglot-ensure)
         (json-ts-mode . flymake-mode))
  :config
  (setq json-ts-mode-indent-offset 2))

;; Fallback to json-mode when tree-sitter is not available
(unless (treesit-language-available-p 'json)
  (use-package json-mode
    :ensure t
    :mode (("\\.json\\'" . json-mode)
           ("\\.jsonl\\'" . json-mode)
           ("\\.jsonc\\'" . json-mode))
    :hook ((json-mode . json-setup-minor-modes)
           (json-mode . eglot-ensure)
           (json-mode . flymake-mode))
    :config
    (setq json-reformat:indent-width 2
          json-reformat:pretty-string? nil)))

;; Configure JSON LSP server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((json-ts-mode json-mode) . ("vscode-json-language-server" "--stdio"))))

;; JSON minor modes setup (without flyspell)
(defun json-setup-minor-modes ()
  "Enable helpful minor modes for JSON."
  (electric-indent-local-mode 1)
  (hs-minor-mode 1)
  ;; Explicitly disable flyspell for JSON
  (flyspell-mode -1)
  (setq-local tab-width 2
              indent-tabs-mode nil))

(provide 'json-setup)

;;; json-setup.el ends here