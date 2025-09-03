;;; test-magit.el --- Test magit configuration

;; Basic setup
(add-to-list 'load-path "config/core")
(add-to-list 'load-path "config/tools")

;; Load package management
(load-file "config/core/package-config.el")

;; Load and test magit
(load-file "config/tools/magit-config.el")

;; Test that magit configuration loads without errors
(message "Magit configuration loaded successfully!")

;; Test that magit is available (but not loaded yet due to use-package deferring)
(if (fboundp 'magit-status)
    (message "Magit commands are available")
  (message "Magit will be loaded on first use"))