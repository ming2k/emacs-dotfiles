;;; init-python.el -*- lexical-binding: t; -*-

;; Use tree-sitter mode
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; Auto-activate venv for Python buffers (buffer-local, no pollution)
(defun my/python-auto-activate-venv ()
  "Set buffer-local venv if found in project root."
  (when-let* ((root (and (project-current)
                         (project-root (project-current))))
              (venv-dir (seq-find #'file-directory-p
                                  (mapcar (lambda (name) (expand-file-name name root))
                                          '(".venv" "venv" "env")))))
    (setq-local python-shell-virtualenv-root venv-dir)
    (message "Python venv: %s" venv-dir)))

;; Python mode configuration
(add-hook 'python-ts-mode-hook
          (lambda ()
            (setq-local python-indent-offset 4
                        python-shell-interpreter "python3")
            (my/python-auto-activate-venv)))

;; Enable eglot for Python
(add-hook 'python-ts-mode-hook #'eglot-ensure)

(provide 'init-python)
