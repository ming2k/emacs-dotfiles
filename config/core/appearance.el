;;; config/ui/appearance.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Visual appearance enhancements including line numbers, highlighting, and colors
;;; Code:

;; Enable syntax highlighting globally
(global-font-lock-mode 1)

;; Line numbers
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  :config
  (setq display-line-numbers-width-start t))

;; Highlight current line
(use-package hl-line
  :ensure nil
  :hook ((prog-mode text-mode conf-mode) . hl-line-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Rainbow mode for color visualization
(use-package rainbow-mode
  :ensure t
  :hook ((css-mode . rainbow-mode)
         (web-mode . rainbow-mode)
         (html-mode . rainbow-mode)
         (scss-mode . rainbow-mode)
         (sass-mode . rainbow-mode)
         (org-mode . rainbow-mode)))

(provide 'appearance)
;;; config/ui/appearance.el ends here