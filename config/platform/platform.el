;;; config/platform/platform.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Platform-specific configuration loader
;;; Code:

;; Load platform-specific configuration based on system-type
(pcase system-type
  ('darwin (require 'macos))
  ('gnu/linux (require 'linux))
  ('windows-nt (require 'windows)))

(provide 'platform)
;;; config/platform/platform.el ends here