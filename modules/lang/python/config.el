;;; modules/lang/python/config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Comprehensive Python language support
;;; Code:

;; Python settings
(setq python-indent-offset 4
      python-indent-guess-indent-offset nil
      python-shell-interpreter "python3"
      python-shell-completion-native-enable nil
      python-shell-prompt-detect-failure-warning nil
      python-shell-prompt-detect-enabled nil
      python-shell-completion-native-disabled-interpreters '("python3"))

;; Built-in Python mode configuration
(use-package python
  :ensure nil
  :mode (("\\.py\\'" . python-mode)
         ("\\.pyw\\'" . python-mode))
  :interpreter (("python" . python-mode)
                ("python3" . python-mode))
  :hook ((python-mode . python-setup-completion)
         (python-mode . python-setup-minor-modes)
         (python-mode . python-setup-lsp)
         (python-mode . python-setup-keybindings))
  :config
  (setq python-indent-offset 4
        python-indent-guess-indent-offset nil
        python-shell-interpreter "python3"
        python-shell-completion-native-enable nil))

;; Python completion setup
(defun python-setup-completion ()
  "Setup built-in completion for Python with LSP priority."
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'cape-dabbrev
                     #'cape-file
                     #'cape-keyword))))

;; Python minor modes setup
(defun python-setup-minor-modes ()
  "Enable helpful minor modes for Python."
  (subword-mode 1)
  (hs-minor-mode 1)
  (flyspell-prog-mode)
  ;; Disable problematic flymake backends
  (when (boundp 'flymake-diagnostic-functions)
    (setq-local flymake-diagnostic-functions
                (remq 'python-flymake flymake-diagnostic-functions)))
  (setq-local tab-width 4
              indent-tabs-mode nil
              fill-column 88
              tab-always-indent t
              electric-indent-inhibit nil)
  (local-set-key (kbd "TAB") 'indent-for-tab-command))

;; Python LSP setup
(defun python-setup-lsp ()
  "Setup Pyright LSP for Python."
  (when (executable-find "pyright-langserver")
    (eglot-ensure)))

;; Python keybindings setup
(defun python-setup-keybindings ()
  "Setup Python mode keybindings."
  (local-set-key (kbd "C-c C-c") 'python-shell-send-buffer)
  (local-set-key (kbd "C-c C-r") 'python-shell-send-region)
  (local-set-key (kbd "C-c C-l") 'python-shell-send-file)
  (local-set-key (kbd "C-c C-z") 'python-shell-switch-to-shell)
  (local-set-key (kbd "C-c C-f") 'python-format-buffer)
  (local-set-key (kbd "C-c C-d") 'eglot-find-declaration)
  (local-set-key (kbd "C-c C-i") 'eglot-find-implementation)
  (local-set-key (kbd "C-c C-x") 'python-execute-file))

;; Python LSP server configuration
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))
  
  (defun python-eglot-workspace-config ()
    "Return comprehensive workspace configuration for Pyright."
    '(:python 
      (:analysis 
       (:typeCheckingMode "basic"
        :autoImportCompletions t
        :autoSearchPaths t
        :useLibraryCodeForTypes t
        :diagnosticMode "workspace"))))
  
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (derived-mode-p 'python-mode)
                (setq-local eglot-workspace-configuration
                           (python-eglot-workspace-config))
                (setq-local eglot-ignored-server-capabilities 
                           '(:documentHighlightProvider))))))

;; Python utility functions
(defun python-format-buffer ()
  "Format Python buffer with available formatter."
  (interactive)
  (cond
   ((executable-find "black")
    (let ((original-point (point)))
      (shell-command-on-region
       (point-min) (point-max)
       "black --quiet -"
       (current-buffer) t)
      (goto-char original-point)))
   ((executable-find "autopep8")
    (let ((original-point (point)))
      (shell-command-on-region
       (point-min) (point-max)
       "autopep8 -"
       (current-buffer) t)
      (goto-char original-point)))
   ((fboundp 'eglot-format-buffer)
    (eglot-format-buffer))
   (t (message "No Python formatter found. Install black or autopep8"))))

(defun python-execute-file ()
  "Execute current Python file."
  (interactive)
  (save-buffer)
  (let ((file (buffer-file-name)))
    (if file
        (compile (format "%s %s" python-shell-interpreter file))
      (message "Buffer is not visiting a file"))))

;; Python testing support
(defun python-run-pytest ()
  "Run pytest in the current project."
  (interactive)
  (let ((default-directory (or (locate-dominating-file default-directory "pyproject.toml")
                              (locate-dominating-file default-directory "setup.py")
                              (locate-dominating-file default-directory "pytest.ini")
                              default-directory)))
    (compile "python -m pytest -v")))

(defun python-run-pytest-current ()
  "Run pytest on current file."
  (interactive)
  (when buffer-file-name
    (compile (format "python -m pytest -v %s" buffer-file-name))))

(defun python-run-unittest ()
  "Run unittest on current file or directory."
  (interactive)
  (if buffer-file-name
      (compile (format "python -m unittest %s -v" 
                      (file-name-sans-extension 
                       (file-name-nondirectory buffer-file-name))))
    (compile "python -m unittest discover -v")))

;; Virtual environment support
(defun python-activate-venv ()
  "Activate Python virtual environment in current directory."
  (interactive)
  (let ((venv-path (or (locate-dominating-file default-directory "venv/")
                      (locate-dominating-file default-directory ".venv/")
                      (locate-dominating-file default-directory "env/"))))
    (when venv-path
      (let* ((venv-dir (expand-file-name 
                       (cond ((file-exists-p (expand-file-name "venv/" venv-path)) "venv")
                             ((file-exists-p (expand-file-name ".venv/" venv-path)) ".venv")
                             ((file-exists-p (expand-file-name "env/" venv-path)) "env"))
                       venv-path))
             (python-exe (expand-file-name "bin/python" venv-dir)))
        (when (file-exists-p python-exe)
          (setq-local python-shell-interpreter python-exe)
          (message "Activated venv: %s" venv-dir))))))

;; Enhanced Python development features
(defun python-toggle-breakpoint ()
  "Toggle a breakpoint at the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at ".*breakpoint()")
        (progn
          (kill-whole-line)
          (message "Breakpoint removed"))
      (progn
        (python-indent-line)
        (insert "breakpoint()")
        (newline-and-indent)
        (message "Breakpoint added")))))

(defun python-check-syntax ()
  "Check Python syntax using python -m py_compile."
  (interactive)
  (when buffer-file-name
    (save-buffer)
    (compile (format "python3 -m py_compile %s" buffer-file-name))))

;; Auto-activate venv when opening Python files
(add-hook 'python-mode-hook 'python-activate-venv)

;; Additional keybindings
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-t") 'python-run-pytest)
  (define-key python-mode-map (kbd "C-c C-T") 'python-run-pytest-current)
  (define-key python-mode-map (kbd "C-c C-u") 'python-run-unittest)
  (define-key python-mode-map (kbd "C-c C-v") 'python-activate-venv)
  (define-key python-mode-map (kbd "C-c C-b") 'python-toggle-breakpoint)
  (define-key python-mode-map (kbd "C-c C-s") 'python-check-syntax))

(provide 'python-config)
;;; modules/lang/python/config.el ends here