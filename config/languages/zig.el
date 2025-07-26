;;; config/languages/zig.el --- Zig language configuration with Eglot LSP

;;; Commentary:
;; Zig development configuration using Eglot (built-in LSP client)
;; and completion-at-point for code completion

;;; Code:

;; Zig mode
(use-package zig-mode
  :ensure t
  :mode "\\.zig\\'"
  :config
  ;; Indentation settings
  (setq zig-indent-offset 4)
  
  ;; Electric pair mode for Zig
  (add-hook 'zig-mode-hook #'electric-pair-local-mode)
  
  ;; Enable automatic indentation
  (add-hook 'zig-mode-hook #'electric-indent-local-mode)
  
  ;; Subword mode for better navigation
  (add-hook 'zig-mode-hook #'subword-mode))

;; Eglot LSP client for Zig
(use-package eglot
  :ensure nil ; Built-in package
  :hook ((zig-mode . eglot-ensure))
  :config
  ;; Configure Zig LSP server (zls)
  (add-to-list 'eglot-server-programs
               '(zig-mode . ("zls")))
  
  ;; Eglot configuration
  (setq eglot-autoshutdown t)
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-extend-to-xref t)
  
  ;; Performance tuning
  (setq eglot-events-buffer-size 0)
  (setq eglot-sync-connect nil)
  
  ;; Custom keybindings for Eglot
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format)
              ("C-c l a" . eglot-code-actions)
              ("C-c l d" . eglot-find-declaration)
              ("C-c l i" . eglot-find-implementation)
              ("C-c l t" . eglot-find-typeDefinition)
              ("C-c l s" . eglot-shutdown)
              ("C-c l R" . eglot-reconnect)))

;; Enhanced completion with corfu
(use-package corfu
  :ensure t
  :hook ((zig-mode . corfu-mode))
  :config
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.2)
  (setq corfu-auto-prefix 2)
  (setq corfu-cycle t)
  (setq corfu-separator ?\s)
  (setq corfu-preview-current nil)
  (setq corfu-scroll-margin 5)
  
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ("<tab>" . corfu-next)
              ("S-TAB" . corfu-previous)
              ("<backtab>" . corfu-previous)
              ("RET" . corfu-insert)
              ("<return>" . corfu-insert)))

;; Cape for additional completion backends
(use-package cape
  :ensure t
  :config
  ;; Zig-specific backends
  (add-hook 'zig-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
              (add-to-list 'completion-at-point-functions #'cape-file t)
              (add-to-list 'completion-at-point-functions #'cape-keyword t))))

;; Eldoc for documentation
(use-package eldoc
  :ensure nil
  :hook ((zig-mode . eldoc-mode))
  :config
  (setq eldoc-echo-area-use-multiline-p 3)
  (setq eldoc-idle-delay 0.2))

;; Flymake for syntax checking
(use-package flymake
  :ensure nil
  :hook ((zig-mode . flymake-mode))
  :config
  (setq flymake-no-changes-timeout 0.5)
  (setq flymake-start-on-flymake-mode t)
  
  :bind (:map flymake-mode-map
              ("C-c f n" . flymake-goto-next-error)
              ("C-c f p" . flymake-goto-prev-error)
              ("C-c f l" . flymake-show-buffer-diagnostics)))

;; Xref for cross-references
(use-package xref
  :ensure nil
  :config
  (setq xref-search-program 'ripgrep)
  
  :bind (("M-." . xref-find-definitions)
         ("M-," . xref-go-back)
         ("M-?" . xref-find-references)
         ("C-M-." . xref-find-apropos)))

;; Project management for Zig
(use-package project
  :ensure nil
  :config
  ;; Add Zig project detection
  (defun my/zig-project-root (dir)
    "Find Zig project root by looking for build.zig."
    (when-let ((root (locate-dominating-file dir "build.zig")))
      (cons 'zig root)))
  
  (add-hook 'project-find-functions #'my/zig-project-root))

;; Zig-specific utilities
(defun my/zig-setup ()
  "Setup Zig development environment."
  (interactive)
  ;; Set up completion
  (setq-local completion-at-point-functions
              (list #'eglot-completion-at-point
                    #'cape-dabbrev
                    #'cape-file))
  
  ;; Enable helpful minor modes
  (display-line-numbers-mode 1)
  (hl-line-mode 1)
  (show-paren-mode 1)
  
  ;; Set up imenu for better navigation
  (setq imenu-create-index-function #'eglot-imenu))

;; Add Zig setup hook
(add-hook 'zig-mode-hook #'my/zig-setup)

;; Zig compilation and testing utilities
(defun my/zig-build ()
  "Build current Zig project."
  (interactive)
  (save-buffer)
  (if-let ((project-root (project-root (project-current))))
      (let ((default-directory project-root))
        (compile "zig build"))
    (compile (format "zig build-exe %s" (buffer-file-name)))))

(defun my/zig-run ()
  "Run current Zig file or project."
  (interactive)
  (save-buffer)
  (if-let ((project-root (project-root (project-current))))
      (let ((default-directory project-root))
        (compile "zig build run"))
    (compile (format "zig run %s" (buffer-file-name)))))

(defun my/zig-test ()
  "Run Zig tests."
  (interactive)
  (save-buffer)
  (if-let ((project-root (project-root (project-current))))
      (let ((default-directory project-root))
        (compile "zig build test"))
    (compile (format "zig test %s" (buffer-file-name)))))

(defun my/zig-fmt ()
  "Format current Zig file."
  (interactive)
  (save-buffer)
  (shell-command (format "zig fmt %s" (buffer-file-name)))
  (revert-buffer t t))

(defun my/zig-check ()
  "Check current Zig file for errors."
  (interactive)
  (save-buffer)
  (compile (format "zig ast-check %s" (buffer-file-name))))

;; Zig keybindings
(defun my/zig-mode-keys ()
  "Set up Zig mode keybindings."
  (local-set-key (kbd "C-c C-c") #'my/zig-build)
  (local-set-key (kbd "C-c C-r") #'my/zig-run)
  (local-set-key (kbd "C-c C-t") #'my/zig-test)
  (local-set-key (kbd "C-c C-f") #'my/zig-fmt)
  (local-set-key (kbd "C-c C-k") #'my/zig-check))

(add-hook 'zig-mode-hook #'my/zig-mode-keys)

(provide 'zig)
;;; zig.el ends here
