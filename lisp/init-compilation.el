;;; init-compilation.el -*- lexical-binding: t; -*-

;; Compilation mode with ANSI color support
(use-package compile
  :ensure nil
  :config
  ;; Enable ANSI color interpretation in compilation buffers
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

  ;; Compilation window settings
  (setq compilation-scroll-output t
        compilation-ask-about-save nil
        compilation-always-kill t))

(provide 'init-compilation)
