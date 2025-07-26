;;; config/languages/python-lang.el --- Python language support with modern completion -*- lexical-binding: t; -*-
;;; Commentary:
;; Minimal Python development setup using built-in packages, eglot LSP, and corfu completion
;; Simple and native approach without complex LSP configurations
;;; Code:

;; Python basic settings
(setq python-indent-offset 4
      python-indent-guess-indent-offset nil
      python-shell-interpreter "python3"
      python-shell-completion-native-enable nil)

;; Built-in Python mode configuration
(use-package python
  :ensure nil
  :mode (("\\.py\\'" . python-mode)
         ("\\.pyw\\'" . python-mode))
  :interpreter (("python" . python-mode)
                ("python3" . python-mode))
  :hook ((python-mode . python-setup-corfu)
         (python-mode . python-setup-minor-modes)
         (python-mode . python-setup-lsp)
         (python-mode . python-setup-keybindings))
  :config
  (setq python-indent-offset 4
        python-indent-guess-indent-offset nil
        python-shell-interpreter "python3"
        python-shell-completion-native-enable nil))

;; Enhanced Python completion setup
(defun python-setup-corfu ()
  "Setup corfu completion for Python with LSP priority."
  (setq-local corfu-auto-delay 0.0
              corfu-auto-prefix 1
              completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'cape-dabbrev
                     #'cape-file
                     #'cape-keyword))))

;; Python minor modes setup
(defun python-setup-minor-modes ()
  "Enable helpful minor modes for Python."
  (electric-pair-local-mode 1)
  (electric-indent-local-mode 1)
  (subword-mode 1)
  (hs-minor-mode 1)
  (flyspell-prog-mode)
  (setq-local tab-width 4
              indent-tabs-mode nil
              fill-column 88))  ; Black formatter default

;; Simple Python LSP setup
(defun python-setup-lsp ()
  "Setup LSP for Python if available."
  (when (or (executable-find "pylsp")
            (executable-find "pyright-langserver"))
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

;; Simple Python LSP server configuration
(with-eval-after-load 'eglot
  ;; 添加 pylsp 支持
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pylsp")))
  
  ;; 如果有 pyright，使用它
  (when (executable-find "pyright-langserver")
    (setf (alist-get 'python-mode eglot-server-programs)
          '("pyright-langserver" "--stdio"))))

;; Python utility functions
(defun python-format-buffer ()
  "Format Python buffer with available formatter."
  (interactive)
  (cond
   ;; 优先使用 black
   ((executable-find "black")
    (let ((original-point (point)))
      (shell-command-on-region
       (point-min) (point-max)
       "black --quiet -"
       (current-buffer) t)
      (goto-char original-point)))
   ;; 使用 autopep8
   ((executable-find "autopep8")
    (let ((original-point (point)))
      (shell-command-on-region
       (point-min) (point-max)
       "autopep8 -"
       (current-buffer) t)
      (goto-char original-point)))
   ;; 使用 LSP 格式化
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

(provide 'python-lang)
;;; python-config.el ends here
