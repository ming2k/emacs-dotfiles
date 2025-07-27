;;; packages.el -*- lexical-binding: t; -*-

;; This file is for package-specific configurations that need to be loaded
;; after use-package is available but before modules are loaded.

;; use-package is already configured in early-init.el
;; Additional use-package optimizations
(setq use-package-expand-minimally t
      use-package-compute-statistics t)