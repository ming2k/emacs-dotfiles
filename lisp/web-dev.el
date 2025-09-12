;;; config/tools/web-dev.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Modern web development configuration for React, Vue, and Svelte
;; Uses web-mode and typescript-mode for optimal support
;;; Code:

;; ===== COMMON WEB DEVELOPMENT SETUP =====

;; Web-mode for various web templates and frameworks
(use-package web-mode
  :ensure t
  :mode (("\\.svelte\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.htm\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))
  :hook ((web-mode . web-dev-setup-common)
         (web-mode . web-dev-setup-framework-specific))
  :config
  ;; Basic web-mode settings
  (setq web-mode-markup-indent-offset 2
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
        web-mode-enable-css-colorization t
        web-mode-enable-javascript-keywords t
        web-mode-enable-html-entities-fontification t)
  :bind (:map web-mode-map
              ("C-c C-t" . web-dev-run-test)
              ("C-c C-s" . web-dev-run-dev)
              ("C-c C-b" . web-dev-run-build)
              ("C-c C-f" . web-dev-format-buffer)
              ("C-c C-e" . web-mode-element-wrap)
              ("C-c C-r" . web-mode-element-rename)))

;; TypeScript mode for .ts files
(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :hook (typescript-mode . web-dev-setup-typescript)
  :config
  (setq typescript-indent-level 2))

;; ===== REACT CONFIGURATION =====

;; React/JSX support
(use-package rjsx-mode
  :ensure t
  :mode (("\\.js\\'" . rjsx-mode)
         ("\\.jsx\\'" . rjsx-mode))
  :hook (rjsx-mode . web-dev-setup-react)
  :config
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-basic-offset 2
        sgml-basic-offset 2))

;; ===== VUE CONFIGURATION =====

;; Vue-mode for enhanced Vue.js support
(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  :hook (vue-mode . web-dev-setup-vue)
  :config
  (setq vue-html-tab-width 2
        vue-html-extra-indent 0))

;; ===== SVELTE CONFIGURATION =====

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

;; ===== COMMON WEB DEVELOPMENT FUNCTIONS =====

;; Common setup for all web development modes
(defun web-dev-setup-common ()
  "Common setup for all web development modes."
  (setq-local tab-width 2
              indent-tabs-mode nil)
  (subword-mode 1))

;; Framework-specific setup based on file extension
(defun web-dev-setup-framework-specific ()
  "Setup framework-specific configuration based on buffer file name."
  (when buffer-file-name
    (cond
     ((string-match-p "\\.svelte\\'" buffer-file-name)
      (svelte-setup-if-svelte))
     ((string-match-p "\\.vue\\'" buffer-file-name)
      (web-dev-setup-vue-in-web-mode))
     ((string-match-p "\\.[jt]sx?\\'" buffer-file-name)
      (web-dev-setup-react-in-web-mode)))))

;; ===== REACT SETUP FUNCTIONS =====

(defun web-dev-setup-react ()
  "Setup React development environment."
  (web-dev-setup-common)
  (web-dev-setup-completion)
  (web-dev-setup-lsp-for-framework "typescript-language-server"))

(defun web-dev-setup-react-in-web-mode ()
  "Setup React-specific configuration in web-mode."
  (setq-local web-mode-content-type "jsx")
  (web-mode-set-content-type "jsx")
  (web-dev-setup-completion)
  (web-dev-setup-lsp-for-framework "typescript-language-server"))

;; ===== VUE SETUP FUNCTIONS =====

(defun web-dev-setup-vue ()
  "Setup Vue development environment."
  (web-dev-setup-common)
  (web-dev-setup-completion)
  (web-dev-setup-lsp-for-framework "vue-language-server"))

(defun web-dev-setup-vue-in-web-mode ()
  "Setup Vue-specific configuration in web-mode."
  (setq-local web-mode-engine "vue")
  (web-mode-set-engine "vue")
  (web-dev-setup-completion)
  (web-dev-setup-lsp-for-framework "vue-language-server"))

;; ===== TYPESCRIPT SETUP FUNCTIONS =====

(defun web-dev-setup-typescript ()
  "Setup TypeScript development environment."
  (web-dev-setup-common)
  (web-dev-setup-completion)
  (web-dev-setup-lsp-for-framework "typescript-language-server"))

;; ===== COMMON UTILITY FUNCTIONS =====

;; Generic completion setup
(defun web-dev-setup-completion ()
  "Setup completion for web development."
  (when (fboundp 'eglot-completion-at-point)
    (setq-local completion-at-point-functions
                (list #'eglot-completion-at-point
                      #'dabbrev-completion
                      #'comint-filename-completion))))

;; Generic LSP setup
(defun web-dev-setup-lsp-for-framework (server-command)
  "Setup LSP for web development with SERVER-COMMAND."
  (when (and (fboundp 'eglot-ensure)
             (executable-find server-command))
    (condition-case err
        (eglot-ensure)
      (error 
       (message "LSP setup failed for %s: %s" server-command (error-message-string err))))))

;; LSP server configuration
(with-eval-after-load 'eglot
  ;; Svelte LSP server
  (add-to-list 'eglot-server-programs
               '(web-mode . ("svelteserver" "--stdio")))
  ;; TypeScript/JavaScript LSP server
  (add-to-list 'eglot-server-programs
               '(typescript-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(rjsx-mode . ("typescript-language-server" "--stdio")))
  ;; Vue LSP server (falls back to TypeScript server if vue-language-server not available)
  (add-to-list 'eglot-server-programs
               '(vue-mode . ("vue-language-server" "--stdio"))))

;; Project utilities
(defun web-dev-project-root ()
  "Find web development project root directory."
  (or (locate-dominating-file default-directory "package.json")
      (locate-dominating-file default-directory "vite.config.js")
      (locate-dominating-file default-directory "vite.config.ts")
      (locate-dominating-file default-directory "webpack.config.js")
      (locate-dominating-file default-directory "next.config.js")
      (locate-dominating-file default-directory "nuxt.config.js")
      (locate-dominating-file default-directory "vue.config.js")
      (locate-dominating-file default-directory "svelte.config.js")
      (locate-dominating-file default-directory "svelte.config.ts")
      (locate-dominating-file default-directory "tsconfig.json")))

;; Generic npm/yarn command runner
(defun web-dev-run-npm-command (command)
  "Run npm/yarn COMMAND in project root."
  (if-let ((project-root (web-dev-project-root)))
      (let ((default-directory project-root)
            (package-manager (if (file-exists-p (expand-file-name "yarn.lock" project-root))
                                "yarn" "npm")))
        (compile (format "%s run %s" package-manager command)))
    (user-error "No web development project found")))

;; Development commands
(defun web-dev-run-dev ()
  "Run development server in project root."
  (interactive)
  (web-dev-run-npm-command "dev"))

(defun web-dev-run-build ()
  "Run build command in project root."
  (interactive)
  (web-dev-run-npm-command "build"))

(defun web-dev-run-test ()
  "Run test command in project root."
  (interactive)
  (web-dev-run-npm-command "test"))

;; Legacy Svelte function aliases for backward compatibility
(defalias 'svelte-run-dev 'web-dev-run-dev)
(defalias 'svelte-run-build 'web-dev-run-build)
(defalias 'svelte-run-test 'web-dev-run-test)
(defalias 'svelte-project-root 'web-dev-project-root)

;; Generic formatting function
(defun web-dev-format-buffer ()
  "Format web development buffer using available formatter."
  (interactive)
  (cond
   ((and (eglot-current-server) (fboundp 'eglot-format-buffer))
    (eglot-format-buffer))
   ((web-dev-prettier-available-p)
    (web-dev-format-with-prettier))
   (t (message "No formatter available. Consider installing Prettier."))))

(defun web-dev-prettier-available-p ()
  "Check if Prettier is available."
  (executable-find "prettier"))

(defun web-dev-format-with-prettier ()
  "Format buffer with Prettier."
  (let ((original-point (point))
        (file-extension (if buffer-file-name
                           (file-name-extension buffer-file-name)
                         "js"))
        (prettier-cmd (format "prettier --stdin-filepath tmp.%s" file-extension)))
    (if (zerop (shell-command-on-region
                (point-min) (point-max)
                prettier-cmd
                (current-buffer) t))
        (goto-char original-point)
      (message "Prettier formatting failed"))))

;; Legacy Svelte formatting function for backward compatibility
(defalias 'svelte-format-buffer 'web-dev-format-buffer)

;; File associations for configuration files
(add-to-list 'auto-mode-alist '("svelte\\.config\\.[jt]s\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("vite\\.config\\.[jt]s\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("vue\\.config\\.[jt]s\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("nuxt\\.config\\.[jt]s\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("next\\.config\\.[jt]s\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("webpack\\.config\\.[jt]s\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("tailwind\\.config\\.[jt]s\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("postcss\\.config\\.[jt]s\\'" . javascript-mode))

(provide 'web-dev)

;;; config/tools/web-dev.el ends here