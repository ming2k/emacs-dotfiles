;;; config/core/package-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Package management configuration using use-package

;;; Code:

;; Bootstrap use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure use-package
(require 'use-package)
(setq use-package-always-ensure t
      use-package-verbose nil
      use-package-compute-statistics t
      use-package-minimum-reported-time 0.01)

;; Enable use-package statistics reporting
(use-package use-package
  :custom
  (use-package-always-ensure t)
  (use-package-expand-minimally t))

(provide 'package-config)
;;; config/core/package-config.el ends here