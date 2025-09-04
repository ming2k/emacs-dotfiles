;;; yaml-setup.el -*- lexical-binding: t; -*-
;;; Commentary:
;; YAML configuration
;;; Code:

;; YAML mode using built-in yaml-ts-mode (Emacs 29+) if available
(use-package yaml-ts-mode
  :ensure nil
  :mode (("\\.ya?ml\\'" . yaml-ts-mode)
         ("\\.ya?ml\\.j2\\'" . yaml-ts-mode)  ; Jinja2 YAML templates
         ("docker-compose.*\\.ya?ml\\'" . yaml-ts-mode)
         ("\\.clang-format\\'" . yaml-ts-mode)
         ("\\.github/workflows/.*\\.ya?ml\\'" . yaml-ts-mode))
  :when (treesit-language-available-p 'yaml)
  :hook ((yaml-ts-mode . yaml-setup-minor-modes)
         (yaml-ts-mode . eglot-ensure)
         (yaml-ts-mode . flymake-mode))
  :config
  (setq yaml-ts-mode-indent-offset 2))

;; Use yaml-mode when tree-sitter is not available
(use-package yaml-mode
  :ensure t
  :mode (("\\.ya?ml\\'" . yaml-mode)
         ("\\.ya?ml\\.j2\\'" . yaml-mode)
         ("docker-compose.*\\.ya?ml\\'" . yaml-mode)
         ("\\.clang-format\\'" . yaml-mode)
         ("\\.github/workflows/.*\\.ya?ml\\'" . yaml-mode))
  :hook ((yaml-mode . yaml-setup-minor-modes)
         (yaml-mode . eglot-ensure)
         (yaml-mode . flymake-mode))
  :config
  (setq yaml-indent-offset 2))

;; YAML minor modes setup
(defun yaml-setup-minor-modes ()
  "Enable helpful minor modes for YAML."
  (electric-indent-local-mode 1)
  (hs-minor-mode 1)
  (flyspell-prog-mode)
  (font-lock-mode 1)
  (setq-local tab-width 2
              indent-tabs-mode nil
              require-final-newline t
              font-lock-maximum-decoration t))

;; Configure YAML LSP server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((yaml-ts-mode yaml-mode) . ("yaml-language-server" "--stdio"))))

(provide 'yaml-setup)

;;; yaml-setup.el ends here