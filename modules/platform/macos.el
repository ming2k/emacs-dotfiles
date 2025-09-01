;;; modules/platform/macos.el -*- lexical-binding: t; -*-
;;; Commentary:
;; macOS-specific configuration
;;; Code:

;; macOS-specific settings
(when (eq system-type 'darwin)
  ;; Use Command key as Meta
  (setq mac-command-modifier 'meta
        mac-option-modifier 'alt
        mac-right-option-modifier 'alt)
  
  ;; Better scrolling
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  
  ;; Use system clipboard
  (setq select-enable-clipboard t))

(provide 'macos)
;;; modules/platform/macos.el ends here