;;; config/platform/windows.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Windows-specific configuration
;;; Code:

;; Windows-specific settings
(when (eq system-type 'windows-nt)
  ;; Better file handling
  (setq w32-pipe-read-delay 0
        w32-pipe-buffer-size (* 64 1024))
  
  ;; Use system clipboard
  (setq select-enable-clipboard t))

(provide 'windows)
;;; config/platform/windows.el ends here