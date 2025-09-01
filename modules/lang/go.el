;;; modules/lang/go.el -*- lexical-binding: t; -*-

;; Declare functions to suppress native compilation warnings
(declare-function eglot-rename "eglot")
(declare-function eglot-code-actions "eglot") 
(declare-function eglot--code-action-bounds "eglot")
(declare-function lsp-rename "lsp-mode")
(declare-function lsp-execute-code-action-by-kind "lsp-mode")

;; Enhanced Go settings
(setq go-ts-mode-indent-offset 4
      gofmt-command "gofmt"
      gofmt-args '("-s")
      go-fontify-function-calls t)

;; Built-in Go tree-sitter mode (Emacs 29+)
(use-package go-ts-mode
  :ensure nil
  :mode "\\.go\\'"
  :when (treesit-language-available-p 'go)
  :hook ((go-ts-mode . go-setup-minor-modes)
         (go-ts-mode . eglot-ensure)
         (go-ts-mode . flymake-mode))
  :bind (:map go-ts-mode-map
              ("C-c C-r" . go-run-buffer)
              ("C-c C-t" . go-test-current-package)
              ("C-c C-a" . go-test-all)
              ("C-c C-b" . go-build)
              ("C-c C-f" . go-format-buffer)
              ("C-c C-i" . go-format-imports)
              ("C-c C-g" . go-get-package)
              ("C-c C-d" . xref-find-definitions)
              ("C-c C-h" . go-doc-at-point))
  :config
  (setq go-ts-mode-indent-offset 4))

;; Fallback Go mode for older Emacs
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :unless (treesit-language-available-p 'go)
  :hook ((go-mode . go-setup-minor-modes)
         (go-mode . eglot-ensure)
         (go-mode . flymake-mode))
  :bind (:map go-mode-map
              ("C-c C-r" . go-run-buffer)
              ("C-c C-t" . go-test-current-package)
              ("C-c C-a" . go-test-all)
              ("C-c C-b" . go-build)
              ("C-c C-f" . go-format-buffer)
              ("C-c C-i" . go-format-imports)
              ("C-c C-g" . go-get-package)
              ("C-c C-d" . xref-find-definitions)
              ("C-c C-h" . go-doc-at-point))
  :config
  (setq gofmt-command "gofmt"
        gofmt-args '("-s")
        go-fontify-function-calls t))


;; Go minor modes setup
(defun go-setup-minor-modes ()
  "Enable helpful minor modes for Go."
  (electric-indent-local-mode 1)
  (subword-mode 1)
  (hs-minor-mode 1)
  (flyspell-prog-mode)
  ;; Go uses tabs by convention
  (setq-local tab-width 4
              indent-tabs-mode t))


;; Configure Go LSP server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((go-mode go-ts-mode) . ("gopls")))
  
  ;; Go-specific LSP settings
  (defun go-eglot-workspace-config ()
    "Return Go workspace configuration for gopls."
    '(:gopls
      (:gofumpt t
       :staticcheck t
       :analyses (:unusedparams t
                  :shadow t))))
  
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (derived-mode-p 'go-mode 'go-ts-mode)
                (setq-local eglot-workspace-configuration
                           (go-eglot-workspace-config))))))

;; Go utility functions
(defun go-run-buffer ()
  "Run current Go file."
  (interactive)
  (if (executable-find "go")
      (progn
        (save-buffer)
        (compile (format "go run %s" (buffer-file-name))))
    (message "Go not found. Install from https://golang.org/")))

(defun go-test-current-package ()
  "Run go test in current package."
  (interactive)
  (if (executable-find "go")
      (progn
        (save-buffer)
        (compile "go test ."))
    (message "Go not found. Install from https://golang.org/")))

(defun go-test-all ()
  "Run go test on all packages."
  (interactive)
  (if (executable-find "go")
      (if-let ((project-root (locate-dominating-file default-directory "go.mod")))
          (let ((default-directory project-root))
            (compile "go test ./..."))
        (compile "go test ./..."))
    (message "Go not found. Install from https://golang.org/")))

(defun go-build ()
  "Build current Go project."
  (interactive)
  (if (executable-find "go")
      (progn
        (save-buffer)
        (if-let ((project-root (locate-dominating-file default-directory "go.mod")))
            (let ((default-directory project-root))
              (compile "go build"))
          (compile "go build .")))
    (message "Go not found. Install from https://golang.org/")))

(defun go-format-buffer ()
  "Format current buffer with gofmt or LSP."
  (interactive)
  (cond
   ((and (fboundp 'eglot-current-server) (eglot-current-server))
    (eglot-format-buffer))
   ((executable-find "gofmt")
    (let ((original-point (point)))
      (shell-command-on-region
       (point-min) (point-max)
       "gofmt"
       (current-buffer) t)
      (goto-char original-point)))
   (t (message "No Go formatter found. Install Go toolchain"))))

(defun go-format-imports ()
  "Organize imports with goimports."
  (interactive)
  (if (executable-find "goimports")
      (let ((original-point (point)))
        (shell-command-on-region
         (point-min) (point-max)
         "goimports"
         (current-buffer) t)
        (goto-char original-point))
    (message "goimports not found. Install with: go install golang.org/x/tools/cmd/goimports@latest")))

(defun go-get-package ()
  "Run go get for package under cursor or prompt for package."
  (interactive)
  (if (executable-find "go")
      (let ((package (or (thing-at-point 'url)
                         (read-string "Go package: "))))
        (when package
          (compile (format "go get %s" package))))
    (message "Go not found. Install from https://golang.org/")))

(defun go-doc-at-point ()
  "Show Go documentation for symbol at point."
  (interactive)
  (if (executable-find "go")
      (if-let ((symbol (thing-at-point 'symbol)))
          (compile (format "go doc %s" symbol))
        (message "No symbol at point"))
    (message "Go not found. Install from https://golang.org/")))

;; Go file associations
(add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("go\\.sum\\'" . fundamental-mode))

;; Optional: format on save
(defun go-format-on-save ()
  "Format Go buffer before saving."
  (when (or (eq major-mode 'go-mode) (eq major-mode 'go-ts-mode))
    (if (executable-find "goimports")
        (go-format-imports)
      (go-format-buffer))))

;; Uncomment to enable format on save
;; (add-hook 'before-save-hook 'go-format-on-save)

(provide 'go)

;;; modules/lang/go.el ends here