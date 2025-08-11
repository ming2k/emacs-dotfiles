;;; modules/lang/svelte/config.el -*- lexical-binding: t; -*-

;; Svelte mode configuration - use web-mode for optimal support
(use-package web-mode
  :ensure t
  :mode ("\\.svelte\\'" . web-mode)
  :hook (web-mode . svelte-setup-if-svelte)
  :bind (:map web-mode-map
              ("C-c C-t" . svelte-run-test)
              ("C-c C-s" . svelte-run-dev)
              ("C-c C-b" . svelte-run-build)
              ("C-c C-f" . svelte-format-buffer)
              ("C-c C-e" . web-mode-element-wrap)
              ("C-c C-r" . web-mode-element-rename)))

;; Fallback svelte-mode (minimal configuration to avoid conflicts)
(use-package svelte-mode
  :ensure t
  :defer t
  :config
  (setq svelte-disable-company t))

;; Main setup function for Svelte files
(defun svelte-setup-if-svelte ()
  "Setup Svelte configuration only for .svelte files."
  (when (and buffer-file-name (string-match-p "\\.svelte\\'" buffer-file-name))
    (svelte-configure-web-mode)
    (svelte-setup-completion)
    (svelte-setup-minor-modes)
    (svelte-setup-lsp)))

;; Streamlined web-mode configuration for Svelte
(defun svelte-configure-web-mode ()
  "Configure web-mode optimally for Svelte files."
  (setq-local web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-script-padding 0
              web-mode-style-padding 0
              web-mode-block-padding 0
              web-mode-comment-style 2
              web-mode-enable-auto-pairing t
              web-mode-enable-auto-closing t
              web-mode-enable-current-element-highlight t
              web-mode-enable-part-face t
              web-mode-enable-block-face t
              web-mode-engine "svelte"
              tab-width 2
              indent-tabs-mode nil)
  (when (fboundp 'web-mode-set-engine)
    (web-mode-set-engine "svelte")))

;; Optimized completion setup
(defun svelte-setup-completion ()
  "Setup completion for Svelte files."
  (when (fboundp 'eglot-completion-at-point)
    (setq-local completion-at-point-functions
                (list #'eglot-completion-at-point
                      #'dabbrev-completion
                      #'comint-filename-completion))))

;; Minimal minor modes setup
(defun svelte-setup-minor-modes ()
  "Enable essential minor modes for Svelte development."
  (subword-mode 1)
  (setq-local tab-width 2
              indent-tabs-mode nil))

;; Simplified LSP setup
(defun svelte-setup-lsp ()
  "Setup LSP for Svelte files."
  (when (and (fboundp 'eglot-ensure)
             (svelte-lsp-server-available-p))
    (condition-case err
        (eglot-ensure)
      (error 
       (message "Svelte LSP setup failed: %s" (error-message-string err))))))

;; Check if LSP server is available
(defun svelte-lsp-server-available-p ()
  "Check if a suitable LSP server is available for Svelte."
  (or (executable-find "svelteserver")
      (executable-find "typescript-language-server")))

;; LSP server configuration
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(web-mode . ("svelteserver" "--stdio"))))

;; Project utilities
(defun svelte-project-root ()
  "Find Svelte project root directory."
  (or (locate-dominating-file default-directory "svelte.config.js")
      (locate-dominating-file default-directory "svelte.config.ts")
      (locate-dominating-file default-directory "vite.config.js")
      (locate-dominating-file default-directory "vite.config.ts")
      (locate-dominating-file default-directory "package.json")))

;; Simplified npm command runner
(defun svelte-run-npm-command (command)
  "Run npm COMMAND in project root."
  (if-let ((project-root (svelte-project-root)))
      (let ((default-directory project-root))
        (compile (format "npm run %s" command)))
    (user-error "No Svelte project found")))

;; Development commands
(defun svelte-run-dev ()
  "Run npm run dev in project root."
  (interactive)
  (svelte-run-npm-command "dev"))

(defun svelte-run-build ()
  "Run npm run build in project root."
  (interactive)
  (svelte-run-npm-command "build"))

(defun svelte-run-test ()
  "Run npm test in project root."
  (interactive)
  (svelte-run-npm-command "test"))

;; Optimized formatting function
(defun svelte-format-buffer ()
  "Format Svelte buffer using available formatter."
  (interactive)
  (cond
   ((and (eglot-current-server) (fboundp 'eglot-format-buffer))
    (eglot-format-buffer))
   ((svelte-prettier-available-p)
    (svelte-format-with-prettier))
   (t (message "No formatter available. Consider installing Prettier with svelte plugin."))))

(defun svelte-prettier-available-p ()
  "Check if Prettier with Svelte plugin is available."
  (and (executable-find "prettier")
       (or (executable-find "prettier-plugin-svelte")
           (locate-dominating-file default-directory "node_modules/prettier-plugin-svelte"))))

(defun svelte-format-with-prettier ()
  "Format buffer with Prettier."
  (let ((original-point (point)))
    (if (zerop (shell-command-on-region
                (point-min) (point-max)
                "prettier --stdin-filepath tmp.svelte --plugin=prettier-plugin-svelte"
                (current-buffer) t))
        (goto-char original-point)
      (message "Prettier formatting failed"))))

;; File associations
(add-to-list 'auto-mode-alist '("svelte\\.config\\.[jt]s\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("vite\\.config\\.[jt]s\\'" . javascript-mode))