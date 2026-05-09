;;; init-yaml.el -*- lexical-binding: t; -*-
;;; Commentary:
;; YAML configuration
;;; Code:

;; YAML mode using built-in yaml-ts-mode
(use-package yaml-ts-mode
  :ensure nil
  :mode (("\\.ya?ml\\'" . yaml-ts-mode)
         ("\\.ya?ml\\.j2\\'" . yaml-ts-mode)  ; Jinja2 YAML templates
         ("docker-compose.*\\.ya?ml\\'" . yaml-ts-mode)
         ("\\.clang-format\\'" . yaml-ts-mode)
         ("\\.github/workflows/.*\\.ya?ml\\'" . yaml-ts-mode))
  :hook ((yaml-ts-mode . yaml-setup-minor-modes)
         (yaml-ts-mode . eglot-ensure)
         (yaml-ts-mode . flymake-mode))
  :config
  (setq yaml-ts-mode-indent-offset 2))

;; YAML minor modes setup
(defun yaml-setup-minor-modes ()
  "Enable helpful minor modes for YAML."
  (electric-indent-local-mode 1)
  (hs-minor-mode 1)
  (font-lock-mode 1)
  (setq-local tab-width 2
              indent-tabs-mode nil
              require-final-newline t
              font-lock-maximum-decoration t))

(provide 'init-yaml)

;;; init-yaml.el ends here
