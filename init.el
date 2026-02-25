;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; A clean, modern Emacs configuration

;;; Code:

;; UTF-8 everywhere
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(prefer-coding-system 'utf-8)

;; Authentication sources - used by mu4e, sieve-manage, TRAMP, ERC, etc.
;; Supports both encrypted .authinfo.gpg (recommended) and plain .authinfo
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))

;; Enable automatic reloading for all files
(global-auto-revert-mode 1)

;; Window navigation with Shift+arrow keys
(windmove-default-keybindings)

;; Winner mode - undo/redo window configuration changes
(winner-mode 1)

;; Unbind to disable middle click paste
(global-set-key [mouse-2] #'ignore)
;; Disable touchpad pinch scale text up and down
(global-set-key (kbd "<pinch>") 'ignore)

;; Disable middle-click (yank/paste)
(setq mouse-yank-at-point nil)

;; Enable mouse support in terminal
;;(unless (display-graphic-p) )

;; Package management setup
(require 'package)
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure nil) ; We use :ensure t explicitly per package

;; Custom file for Emacs-generated code
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Add lisp directory to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/lang" user-emacs-directory))

;;;; Make sure the file name and "provide" name are consistent

;; Load core config
(require 'init-completion)
(require 'init-lsp)
(require 'init-editing)
(require 'init-compilation)
(require 'init-session)
(require 'init-project)
(require 'init-ui)

;; Load language config
(require 'init-cc)
(require 'init-python)
(require 'init-rust)
(require 'init-go)
(require 'init-javascript)
(require 'init-typescript)
(require 'init-dart)
(require 'init-shell)
(require 'init-lua)
(require 'init-yaml)
(require 'init-json)
(require 'init-markdown)
(require 'init-zig)
(require 'init-lisp)
(require 'init-emacs-lisp)
(require 'init-nushell)
(require 'init-cmake)
(require 'init-verilog)

;; Load utils
(require 'init-utils)

;; Load tool config
(require 'init-git)
(require 'init-org)
(require 'init-web)
;; (require 'init-mail)
;; (require 'init-elfeed)
;; (require 'init-erc)
(require 'init-eww)

;; Add a specific directory for themes to custom-theme-load-path
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
;; Disable all themes before loading to avoid conflicts on reload
(mapc #'disable-theme custom-enabled-themes)
;; Load the theme using the standard function
(load-theme 'gruvbox-dark t)

;; Essential settings
(setq select-enable-clipboard t
      select-enable-primary nil) ; `primary` uses different clipboard mechanism with `clipboard`

;; Wayland clipboard integration
;; Use wl-clipboard for system clipboard access
(defun wl-copy (text)
  "Copy TEXT to Wayland clipboard asynchronously.
Uses make-process with pipe connection to handle large content without blocking."
  (when text
    (let ((proc (make-process
                 :name "wl-copy"
                 :buffer nil
                 :command '("wl-copy" "-f" "-n") ; -f: foreground, -n: no newline
                 :connection-type 'pipe)))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun wl-paste ()
  "Paste from Wayland clipboard."
  (shell-command-to-string "wl-paste -n 2>/dev/null")) ; -n: no newline

;; When running as a daemon, WAYLAND_DISPLAY may not be set at startup.
;; Discover the socket from XDG_RUNTIME_DIR when the first client frame appears.
(defun my/ensure-wayland-display (&optional _frame)
  (unless (getenv "WAYLAND_DISPLAY")
    (let* ((xdg (or (getenv "XDG_RUNTIME_DIR")
                    (format "/run/user/%d" (user-uid))))
           (sock (car (file-expand-wildcards
                       (expand-file-name "wayland-[0-9]" xdg)))))
      (when sock
        (setenv "WAYLAND_DISPLAY" (file-name-nondirectory sock))))))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'my/ensure-wayland-display)
  (my/ensure-wayland-display))

;; Integrate with Emacs clipboard system
(setq interprogram-cut-function 'wl-copy ; Use wl-copy when copying from Emacs
      interprogram-paste-function 'wl-paste ; Use wl-paste when pasting to Emacs
      save-interprogram-paste-before-kill t) ; Save clipboard to kill-ring before new kill

;; Performance optimizations
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq inhibit-compacting-font-caches t)

(provide 'init)
;;; init.el ends here
