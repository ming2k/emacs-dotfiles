;;; config/lang/zig.el -*- lexical-binding: t; -*-

;; Enhanced Zig settings
(setq zig-indent-offset 4
      zig-format-on-save nil)

;; Built-in Zig tree-sitter mode (Emacs 29+)
(use-package zig-ts-mode
  :ensure nil
  :mode "\\.zig\\'"
  :when (treesit-language-available-p 'zig)
  :hook ((zig-ts-mode . zig-setup-minor-modes)
         (zig-ts-mode . eglot-ensure)
         (zig-ts-mode . flymake-mode))
  :bind (:map zig-ts-mode-map
              ("C-c C-b" . zig-build)
              ("C-c C-r" . zig-run)
              ("C-c C-t" . zig-test)
              ("C-c C-k" . zig-check)
              ("C-c C-f" . zig-format-buffer)
              ("C-c C-h" . zig-doc)
              ("C-c C-i" . zig-init))
  :config
  (setq zig-ts-mode-indent-offset 4))

;; Fallback Zig mode for older Emacs
(use-package zig-mode
  :ensure t
  :mode "\\.zig\\'"
  :unless (treesit-language-available-p 'zig)
  :hook ((zig-mode . zig-setup-minor-modes)
         (zig-mode . eglot-ensure)
         (zig-mode . flymake-mode))
  :bind (:map zig-mode-map
              ("C-c C-b" . zig-build)
              ("C-c C-r" . zig-run)
              ("C-c C-t" . zig-test)
              ("C-c C-k" . zig-check)
              ("C-c C-f" . zig-format-buffer)
              ("C-c C-h" . zig-doc)
              ("C-c C-i" . zig-init))
  :config
  (setq zig-indent-offset 4
        zig-format-on-save nil))


;; Zig minor modes setup
(defun zig-setup-minor-modes ()
  "Enable helpful minor modes for Zig."
  (electric-indent-local-mode 1)
  (subword-mode 1)
  (hs-minor-mode 1)
  (setq tab-width 4
        indent-tabs-mode nil))


;; Configure Zig LSP server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((zig-mode zig-ts-mode) . ("zls")))
  
  ;; Zig-specific LSP settings
  (defun zig-eglot-workspace-config ()
    "Return Zig workspace configuration for zls."
    '(:zls
      (:enable_snippets t
       :enable_ast_check_diagnostics t
       :enable_autofix t
       :enable_import_embedfile_argument_completions t
       :warn_style t
       :highlight_global_var_declarations t)))
  
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (derived-mode-p 'zig-mode 'zig-ts-mode)
                (setq-local eglot-workspace-configuration
                           (zig-eglot-workspace-config))))))

;; Zig utility functions
(defun zig-build ()
  "Build Zig project with zig build."
  (interactive)
  (if (locate-dominating-file default-directory "build.zig")
      (compile "zig build")
    (message "Not in a Zig project (no build.zig found)")))

(defun zig-run ()
  "Run Zig project with zig build run."
  (interactive)
  (if (locate-dominating-file default-directory "build.zig")
      (compile "zig build run")
    (message "Not in a Zig project (no build.zig found)")))

(defun zig-test ()
  "Run Zig tests with zig build test."
  (interactive)
  (if (locate-dominating-file default-directory "build.zig")
      (compile "zig build test")
    (message "Not in a Zig project (no build.zig found)")))

(defun zig-check ()
  "Check Zig project with zig build check."
  (interactive)
  (if (locate-dominating-file default-directory "build.zig")
      (compile "zig build check")
    (message "Not in a Zig project (no build.zig found)")))

(defun zig-format-buffer ()
  "Format current buffer with zig fmt or eglot."
  (interactive)
  (cond
   ((and (fboundp 'eglot-current-server) (eglot-current-server))
    (eglot-format-buffer))
   ((executable-find "zig")
    (let ((original-point (point)))
      (shell-command-on-region
       (point-min) (point-max)
       "zig fmt --stdin"
       (current-buffer) t)
      (goto-char original-point)))
   (t (message "Zig not found. Install Zig from https://ziglang.org/"))))

(defun zig-doc ()
  "Open Zig documentation."
  (interactive)
  (browse-url "https://ziglang.org/documentation/"))

(defun zig-init ()
  "Initialize a new Zig project with zig init."
  (interactive)
  (if (executable-find "zig")
      (let ((default-directory (read-directory-name "Project directory: ")))
        (compile "zig init"))
    (message "Zig not found. Install Zig from https://ziglang.org/")))

(provide 'zig-setup)

;;; config/lang/zig.el ends here