;;; modules/lang/markdown/config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Simple vanilla Markdown language support with visual line mode
;;; Code:

;; Markdown mode settings
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :config
  ;; Basic markdown settings
  (setq markdown-command "pandoc"
        markdown-fontify-code-blocks-natively t
        markdown-indent-on-enter nil))

(provide 'markdown-config)
;;; modules/lang/markdown/config.el ends here
