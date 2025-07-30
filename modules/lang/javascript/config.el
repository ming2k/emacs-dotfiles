;;; modules/lang/javascript/config.el -*- lexical-binding: t; -*-

;; Enhanced JavaScript/TypeScript settings
(setq js-indent-level 2
      js-switch-indent-offset 2
      typescript-indent-level 2)

;; Built-in JavaScript mode
(use-package js
  :ensure nil
  :mode (("\\.js\\'" . js-mode)
         ("\\.jsx\\'" . js-mode)
         ("\\.mjs\\'" . js-mode))
  :hook ((js-mode . js-setup-corfu)
         (js-mode . js-setup-minor-modes)
         (js-mode . js-setup-lsp))
  :bind (:map js-mode-map
              ("C-c C-r" . js-run-node)
              ("C-c C-t" . js-run-npm-test)
              ("C-c C-s" . js-run-npm-start)
              ("C-c C-b" . js-run-npm-build)
              ("C-c C-f" . js-format-buffer))
  :config
  (setq js-indent-level 2
        js-switch-indent-offset 2))

;; Built-in TypeScript tree-sitter mode (Emacs 29+)
(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :when (treesit-language-available-p 'typescript)
  :hook ((typescript-ts-mode . js-setup-corfu)
         (typescript-ts-mode . js-setup-minor-modes)
         (typescript-ts-mode . js-setup-lsp)
         (tsx-ts-mode . js-setup-corfu)
         (tsx-ts-mode . js-setup-minor-modes)
         (tsx-ts-mode . js-setup-lsp))
  :bind (:map typescript-ts-mode-map
              ("C-c C-r" . js-run-node)
              ("C-c C-t" . js-run-npm-test)
              ("C-c C-s" . js-run-npm-start)
              ("C-c C-b" . js-run-npm-build)
              ("C-c C-f" . js-format-buffer))
  :config
  (setq typescript-ts-mode-indent-offset 2))

;; Fallback TypeScript mode for older Emacs
(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :unless (treesit-language-available-p 'typescript)
  :hook ((typescript-mode . js-setup-corfu)
         (typescript-mode . js-setup-minor-modes)
         (typescript-mode . js-setup-lsp))
  :bind (:map typescript-mode-map
              ("C-c C-r" . js-run-node)
              ("C-c C-t" . js-run-npm-test)
              ("C-c C-s" . js-run-npm-start)
              ("C-c C-b" . js-run-npm-build)
              ("C-c C-f" . js-format-buffer))
  :config
  (setq typescript-indent-level 2))

;; Enhanced JavaScript/TypeScript completion setup
(defun js-setup-corfu ()
  "Setup corfu completion for JavaScript/TypeScript with LSP priority."
  (setq-local corfu-auto-delay 0.0
              corfu-auto-prefix 1
              completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'cape-dabbrev
                     #'cape-file
                     #'cape-keyword))))

;; JavaScript/TypeScript minor modes setup
(defun js-setup-minor-modes ()
  "Enable helpful minor modes for JavaScript/TypeScript."
  (electric-indent-local-mode 1)
  (subword-mode 1)
  (hs-minor-mode 1)
  (flyspell-prog-mode)
  (setq-local comment-auto-fill-only-comments t
              tab-width 2
              indent-tabs-mode nil))

;; JavaScript/TypeScript LSP setup
(defun js-setup-lsp ()
  "Setup LSP for JavaScript/TypeScript if available."
  (when (or (executable-find "typescript-language-server")
            (executable-find "deno"))
    (eglot-ensure)))

;; Configure JavaScript/TypeScript LSP server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((js-mode typescript-mode typescript-ts-mode tsx-ts-mode) . ("typescript-language-server" "--stdio"))))

;; Built-in JSON modes
(use-package json-ts-mode
  :ensure nil
  :mode (("\\.json\\'" . json-ts-mode)
         ("package\\.json\\'" . json-ts-mode)
         ("\\.babelrc\\'" . json-ts-mode)
         ("\\.eslintrc\\'" . json-ts-mode)
         ("tsconfig\\.json\\'" . json-ts-mode))
  :when (treesit-language-available-p 'json)
  :config
  (setq json-ts-mode-indent-offset 2))

;; Fallback JSON mode
(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)
         ("package\\.json\\'" . json-mode)
         ("\\.babelrc\\'" . json-mode)
         ("\\.eslintrc\\'" . json-mode)
         ("tsconfig\\.json\\'" . json-mode))
  :unless (treesit-language-available-p 'json)
  :config
  (setq json-reformat:indent-width 2))

;; JavaScript/TypeScript utility functions
(defun js-run-node ()
  "Run current buffer with Node.js."
  (interactive)
  (if (executable-find "node")
      (progn
        (save-buffer)
        (compile (format "node %s" (buffer-file-name))))
    (message "Node.js not found. Install from https://nodejs.org/")))

(defun js-run-npm-test ()
  "Run npm test in project root."
  (interactive)
  (if-let ((project-root (locate-dominating-file default-directory "package.json")))
      (let ((default-directory project-root))
        (compile "npm test"))
    (message "No package.json found. Not in a Node.js project")))

(defun js-run-npm-start ()
  "Run npm start in project root."
  (interactive)
  (if-let ((project-root (locate-dominating-file default-directory "package.json")))
      (let ((default-directory project-root))
        (compile "npm start"))
    (message "No package.json found. Not in a Node.js project")))

(defun js-run-npm-build ()
  "Run npm run build in project root."
  (interactive)
  (if-let ((project-root (locate-dominating-file default-directory "package.json")))
      (let ((default-directory project-root))
        (compile "npm run build"))
    (message "No package.json found. Not in a Node.js project")))

(defun js-format-buffer ()
  "Format buffer with available formatter."
  (interactive)
  (cond
   ((executable-find "prettier")
    (let ((original-point (point))
          (file-ext (file-name-extension (buffer-file-name))))
      (shell-command-on-region
       (point-min) (point-max)
       (format "prettier --stdin-filepath tmp.%s" (or file-ext "js"))
       (current-buffer) t)
      (goto-char original-point)))
   ((fboundp 'eglot-format-buffer)
    (eglot-format-buffer))
   (t (message "No JavaScript formatter found. Install prettier with: npm install -g prettier"))))

;; File mode associations
(add-to-list 'auto-mode-alist '("package\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.babelrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.eslintrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("tsconfig\\.json\\'" . json-mode))