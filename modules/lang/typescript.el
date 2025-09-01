;;; modules/lang/typescript.el -*- lexical-binding: t; -*-
;;; Commentary:
;; TypeScript configuration using built-in packages
;;; Code:

;; TypeScript settings
(setq typescript-indent-level 2)

;; Built-in TypeScript tree-sitter mode (Emacs 29+)
(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :when (treesit-language-available-p 'typescript)
  :hook ((typescript-ts-mode . typescript-setup-minor-modes)
         (typescript-ts-mode . eglot-ensure)
         (typescript-ts-mode . flymake-mode)
         (tsx-ts-mode . typescript-setup-minor-modes)
         (tsx-ts-mode . eglot-ensure)
         (tsx-ts-mode . flymake-mode))
  :bind (:map typescript-ts-mode-map
              ("C-c C-r" . typescript-run-node)
              ("C-c C-t" . typescript-run-npm-test)
              ("C-c C-s" . typescript-run-npm-start)
              ("C-c C-b" . typescript-run-npm-build)
              ("C-c C-f" . typescript-format-buffer))
  :config
  (setq typescript-ts-mode-indent-offset 2))

;; Fallback TypeScript mode for older Emacs (using built-in js-mode for .ts files)
(unless (treesit-language-available-p 'typescript)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . js-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . js-mode)))

;; TypeScript minor modes setup
(defun typescript-setup-minor-modes ()
  "Enable helpful minor modes for TypeScript."
  (electric-indent-local-mode 1)
  (subword-mode 1)
  (hs-minor-mode 1)
  (flyspell-prog-mode)
  (setq-local comment-auto-fill-only-comments t
              tab-width 2
              indent-tabs-mode nil))

;; Configure TypeScript LSP server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode) . ("typescript-language-server" "--stdio"))))

;; TypeScript utility functions
(defun typescript-run-node ()
  "Run current TypeScript buffer with ts-node."
  (interactive)
  (cond
   ((executable-find "ts-node")
    (save-buffer)
    (compile (format "ts-node %s" (buffer-file-name))))
   ((executable-find "npx")
    (save-buffer)
    (compile (format "npx ts-node %s" (buffer-file-name))))
   (t (message "ts-node not found. Install with: npm install -g ts-node"))))

(defun typescript-run-npm-test ()
  "Run npm test in project root."
  (interactive)
  (if-let ((project-root (locate-dominating-file default-directory "package.json")))
      (let ((default-directory project-root))
        (compile "npm test"))
    (message "No package.json found. Not in a Node.js project")))

(defun typescript-run-npm-start ()
  "Run npm start in project root."
  (interactive)
  (if-let ((project-root (locate-dominating-file default-directory "package.json")))
      (let ((default-directory project-root))
        (compile "npm start"))
    (message "No package.json found. Not in a Node.js project")))

(defun typescript-run-npm-build ()
  "Run npm run build in project root."
  (interactive)
  (if-let ((project-root (locate-dominating-file default-directory "package.json")))
      (let ((default-directory project-root))
        (compile "npm run build"))
    (message "No package.json found. Not in a Node.js project")))

(defun typescript-format-buffer ()
  "Format buffer with available formatter."
  (interactive)
  (cond
   ((and (fboundp 'eglot-current-server) (eglot-current-server))
    (eglot-format-buffer))
   ((executable-find "prettier")
    (let ((original-point (point))
          (file-ext (file-name-extension (buffer-file-name))))
      (shell-command-on-region
       (point-min) (point-max)
       (format "prettier --stdin-filepath tmp.%s" (or file-ext "ts"))
       (current-buffer) t)
      (goto-char original-point)))
   (t (message "No TypeScript formatter found. Install prettier with: npm install -g prettier"))))

(provide 'typescript)

;;; modules/lang/typescript.el ends here