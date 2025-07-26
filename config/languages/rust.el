;;; config/languages/rust.el --- Rust language support with modern completion -*- lexical-binding: t; -*-
;;; Commentary:
;; Modern Rust development setup using built-in packages, eglot LSP, and corfu completion
;;; Code:

;; Enhanced Rust settings
(setq rust-indent-offset 4
      rust-format-on-save nil)

;; Built-in Rust tree-sitter mode (Emacs 29+)
(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'"
  :when (treesit-language-available-p 'rust)
  :hook ((rust-ts-mode . rust-setup-corfu)
         (rust-ts-mode . rust-setup-minor-modes)
         (rust-ts-mode . rust-setup-lsp))
  :bind (:map rust-ts-mode-map
              ("C-c C-b" . rust-compile)
              ("C-c C-r" . rust-run)
              ("C-c C-t" . rust-test)
              ("C-c C-k" . rust-check)
              ("C-c C-c" . rust-clippy)
              ("C-c C-f" . rust-format-buffer)
              ("C-c C-h" . rust-doc-std))
  :config
  (setq rust-ts-mode-indent-offset 4))

;; Fallback Rust mode for older Emacs
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :unless (treesit-language-available-p 'rust)
  :hook ((rust-mode . rust-setup-corfu)
         (rust-mode . rust-setup-minor-modes)
         (rust-mode . rust-setup-lsp))
  :bind (:map rust-mode-map
              ("C-c C-b" . rust-compile)
              ("C-c C-r" . rust-run)
              ("C-c C-t" . rust-test)
              ("C-c C-k" . rust-check)
              ("C-c C-c" . rust-clippy)
              ("C-c C-f" . rust-format-buffer)
              ("C-c C-h" . rust-doc-std))
  :config
  (setq rust-indent-offset 4
        rust-format-on-save nil))

;; Enhanced Rust completion setup
(defun rust-setup-corfu ()
  "Setup corfu completion for Rust with LSP priority."
  (setq-local corfu-auto-delay 0.0
              corfu-auto-prefix 1
              completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'cape-dabbrev
                     #'cape-file
                     #'cape-keyword))))

;; Rust minor modes setup
(defun rust-setup-minor-modes ()
  "Enable helpful minor modes for Rust."
  (electric-pair-local-mode 1)
  (electric-indent-local-mode 1)
  (subword-mode 1)
  (hs-minor-mode 1)
  (flyspell-prog-mode)
  (setq tab-width 4
        indent-tabs-mode nil))

;; Rust LSP setup
(defun rust-setup-lsp ()
  "Setup LSP for Rust if available."
  (when (executable-find "rust-analyzer")
    (eglot-ensure)))

;; Configure Rust LSP server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) . ("rust-analyzer")))
  
  ;; Rust-specific LSP settings
  (add-to-list 'eglot-workspace-configuration
               '(rust-analyzer
                 (cargo (buildScripts (enable . t)))
                 (procMacro (enable . t))
                 (diagnostics (enable . t))
                 (completion (addCallParentheses . t)))))

;; Rust utility functions
(defun rust-compile ()
  "Compile Rust project with cargo build."
  (interactive)
  (if (locate-dominating-file default-directory "Cargo.toml")
      (compile "cargo build")
    (message "Not in a Rust project (no Cargo.toml found)")))

(defun rust-run ()
  "Run Rust project with cargo run."
  (interactive)
  (if (locate-dominating-file default-directory "Cargo.toml")
      (compile "cargo run")
    (message "Not in a Rust project (no Cargo.toml found)")))

(defun rust-test ()
  "Run Rust tests with cargo test."
  (interactive)
  (if (locate-dominating-file default-directory "Cargo.toml")
      (compile "cargo test")
    (message "Not in a Rust project (no Cargo.toml found)")))

(defun rust-check ()
  "Check Rust project with cargo check."
  (interactive)
  (if (locate-dominating-file default-directory "Cargo.toml")
      (compile "cargo check")
    (message "Not in a Rust project (no Cargo.toml found)")))

(defun rust-clippy ()
  "Run Rust linter with cargo clippy."
  (interactive)
  (if (locate-dominating-file default-directory "Cargo.toml")
      (if (executable-find "cargo-clippy")
          (compile "cargo clippy")
        (compile "cargo clippy --help 2>/dev/null || echo 'Install clippy with: rustup component add clippy'"))
    (message "Not in a Rust project (no Cargo.toml found)")))

(defun rust-format-buffer ()
  "Format current buffer with rustfmt."
  (interactive)
  (if (executable-find "rustfmt")
      (progn
        (save-buffer)
        (shell-command (format "rustfmt %s" (buffer-file-name)))
        (revert-buffer t t t))
    (message "rustfmt not found. Install with: rustup component add rustfmt")))

(defun rust-doc-std ()
  "Open Rust standard library documentation."
  (interactive)
  (browse-url "https://doc.rust-lang.org/std/"))

(defun rust-expand-macro ()
  "Expand macro at point using cargo expand."
  (interactive)
  (if (locate-dominating-file default-directory "Cargo.toml")
      (if (executable-find "cargo-expand")
          (compile "cargo expand")
        (message "cargo-expand not found. Install with: cargo install cargo-expand"))
    (message "Not in a Rust project (no Cargo.toml found)")))


(provide 'rust)
;;; rust.el ends here
