;;; init.el --- Simple Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Minimal configuration using require
;;; Code:

;; Add config directories to load path
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "config/languages" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "config/tools" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "config/system" user-emacs-directory))

;; System-specific
(require 'paths)  ; Load file path configurations
(pcase system-type
  ('darwin (require 'macos))
  ('gnu/linux (require 'linux))
  ('windows-nt (require 'windows)))

;; Essential settings
(setq select-enable-clipboard t
      select-enable-primary t)

;; Core modules
(require 'core)
(require 'ui)
(require 'editing)
(require 'completion)
(require 'programming)

;; Tools
(require 'git)
(require 'org-config)
(require 'org-roam-config)

;; Languages
(require 'c-cpp)
(require 'lisp)
(require 'python-lang)
(require 'rust)
(require 'javascript)
(require 'go)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
