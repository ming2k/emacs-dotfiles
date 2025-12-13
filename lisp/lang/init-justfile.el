;;; init-justfile.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Justfile configuration
;;; Code:

;; Just mode for justfile syntax highlighting and editing
(use-package just-mode
  :ensure t
  :mode (("\\(J\\|j\\)ustfile\\'" . just-mode)
         ("\\.just\\'" . just-mode)))

(provide 'init-justfile)
;;; init-justfile.el ends here
