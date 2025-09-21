;;; config/lang/python.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Modern Python development with eglot and corfu
;;; Code:

;; Python settings
(require 'eglot)
(setq python-indent-offset 4
      python-indent-guess-indent-offset nil
      python-shell-interpreter "python3"
      python-shell-completion-native-enable nil
      python-shell-prompt-detect-failure-warning nil
      python-shell-prompt-detect-enabled nil
      python-shell-completion-native-disabled-interpreters '("python3"))

;; Tree-sitter Python mode (preferred)
(use-package python-ts-mode
  :ensure nil
  :when (treesit-language-available-p 'python)
  :mode (("\\.py\\'" . python-ts-mode)
         ("\\.pyw\\'" . python-ts-mode))
  :custom
  (python-shell-interpreter "python3")
  :interpreter (("python" . python-ts-mode)
                ("python3" . python-ts-mode))
  :hook ((python-ts-mode . python-setup-minor-modes)
         (python-ts-mode . python-setup-keybindings)
         (python-ts-mode . python-activate-venv)
         (python-ts-mode . eglot-ensure)
         (python-ts-mode . flymake-mode))
  :config
  (setq python-indent-offset 4))

;; Fallback to regular python-mode if tree-sitter is not available
(unless (treesit-language-available-p 'python)
  (use-package python
    :ensure nil
    :mode (("\\.py\\'" . python-mode)
           ("\\.pyw\\'" . python-mode))
    :custom
    (python-shell-interpreter "python3")
    :interpreter (("python" . python-mode)
                  ("python3" . python-mode))
    :hook ((python-mode . python-setup-minor-modes)
           (python-mode . python-setup-keybindings)
           (python-mode . python-activate-venv)
           (python-mode . eglot-ensure)
           (python-mode . flymake-mode))
    :config
    (setq python-indent-offset 4
          python-indent-guess-indent-offset nil
          python-shell-interpreter "python3"
          python-shell-completion-native-enable nil)))

;; Python minor modes setup
(defun python-setup-minor-modes ()
  "Enable helpful minor modes for Python."
  (font-lock-mode 1)
  (subword-mode 1)
  (hs-minor-mode 1)
  (setq-local tab-width 4
              indent-tabs-mode nil
              fill-column 88
              tab-always-indent 'complete
              electric-indent-inhibit nil))

;; Python keybindings setup
(defun python-setup-keybindings ()
  "Setup Python mode keybindings."
  (local-set-key (kbd "C-c C-c") 'python-shell-send-buffer)
  (local-set-key (kbd "C-c C-r") 'python-shell-send-region)
  (local-set-key (kbd "C-c C-l") 'python-shell-send-file)
  (local-set-key (kbd "C-c C-z") 'python-shell-switch-to-shell)
  (local-set-key (kbd "C-c C-f") 'python-format-buffer)
  (local-set-key (kbd "C-c C-x") 'python-execute-file)
  (local-set-key (kbd "C-c C-t") 'python-run-pytest)
  (local-set-key (kbd "C-c C-T") 'python-run-pytest-current)
  (local-set-key (kbd "C-c C-u") 'python-run-unittest)
  (local-set-key (kbd "C-c C-v") 'python-activate-venv)
  (local-set-key (kbd "C-c C-b") 'python-toggle-breakpoint)
  (local-set-key (kbd "C-c C-s") 'python-check-syntax))

;; Python LSP server configuration for eglot
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . 
                 (lambda (interactive)
                   (cond
                    ((executable-find "pyright-langserver")
                     '("pyright-langserver" "--stdio"))
                    ((executable-find "pylsp")
                     '("pylsp"))
                    ((executable-find "jedi-language-server")  
                     '("jedi-language-server"))
                    (t (user-error "No Python LSP server found. Install pyright, pylsp, or jedi-language-server"))))))
  
  (defun python-eglot-workspace-config ()
    "Return workspace configuration for Python LSP servers."
    (let ((server-name (when (eglot-current-server)
                        (car (process-command (jsonrpc--process (eglot-current-server)))))))
      (cond
       ((or (string-match-p "pyright" (or server-name ""))
            (string-match-p "pylance" (or server-name "")))
        '(:python 
          (:analysis 
           (:typeCheckingMode "basic"
            :autoImportCompletions t
            :autoSearchPaths t  
            :useLibraryCodeForTypes t
            :diagnosticMode "workspace"
            :diagnosticSeverityOverrides
            (:reportMissingImports "error"
             :reportMissingTypeStubs "information"  
             :reportUnusedImport "information"
             :reportUnusedVariable "information")))))
       ((string-match-p "pylsp" (or server-name ""))
        '(:pylsp
          (:plugins
           (:pycodestyle (:enabled t :maxLineLength 88)
            :flake8 (:enabled nil)
            :autopep8 (:enabled t)
            :yapf (:enabled nil)
            :mccabe (:enabled t)
            :pyflakes (:enabled t)
            :pylint (:enabled nil)))))
       (t '()))))
  
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (or (derived-mode-p 'python-mode)
                       (derived-mode-p 'python-ts-mode))
                (setq-local eglot-workspace-configuration
                           (python-eglot-workspace-config))))))

;; Python utility functions
(defun python-format-buffer ()
  "Format Python buffer with available formatter."
  (interactive)
  (cond
   ((and (fboundp 'eglot-current-server) (eglot-current-server))
    (eglot-format-buffer))
   ((executable-find "black")
    (let ((original-point (point)))
      (if (use-region-p)
          (shell-command-on-region
           (region-beginning) (region-end)
           "black --quiet -"
           (current-buffer) t)
        (shell-command-on-region
         (point-min) (point-max)
         "black --quiet -"
         (current-buffer) t))
      (goto-char original-point)))
   ((executable-find "autopep8")
    (let ((original-point (point)))
      (if (use-region-p)
          (shell-command-on-region
           (region-beginning) (region-end)
           "autopep8 -"
           (current-buffer) t)
        (shell-command-on-region
         (point-min) (point-max)
         "autopep8 -"
         (current-buffer) t))
      (goto-char original-point)))
   (t (message "No Python formatter found. Install black, autopep8, or use eglot"))))

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


;; Improved Python shell setup
(defun python-setup-shell ()
  "Setup Python shell with better defaults."
  (setq-local python-shell-buffer-name "Python"
              python-shell-enable-font-lock t
              python-shell-completion-native-enable nil))

(add-hook 'python-mode-hook #'python-setup-shell)
(add-hook 'python-ts-mode-hook #'python-setup-shell)

;; Python import sorting (if available)
(defun python-sort-imports ()
  "Sort Python imports using isort if available."
  (interactive)
  (cond
   ((and (fboundp 'eglot-current-server) (eglot-current-server))
    (eglot-code-action-organize-imports))
   ((executable-find "isort")
    (let ((original-point (point)))
      (shell-command-on-region
       (point-min) (point-max)
       "isort --stdout -"
       (current-buffer) t)
      (goto-char original-point)))
   (t (message "No import sorter found. Install isort or use eglot"))))

;; Add import sorting keybinding
(defun python-setup-import-sorting-keybinding ()
  "Setup import sorting keybinding for python modes."
  (local-set-key (kbd "C-c C-i") 'python-sort-imports))

(add-hook 'python-mode-hook #'python-setup-import-sorting-keybinding)
(add-hook 'python-ts-mode-hook #'python-setup-import-sorting-keybinding)

(provide 'python-setup)
;;; config/lang/python.el ends here