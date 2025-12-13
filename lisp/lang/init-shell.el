;;; init-shell.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Shell scripting support with bash-language-server
;;; Code:


;; Shell-specific eglot configuration
(with-eval-after-load 'eglot
  (add-hook 'sh-mode-hook 'eglot-ensure)

  ;; Shell-specific LSP settings
  (add-hook 'sh-mode-hook
            (lambda ()
              (when (eglot-managed-p)
                ;; Configure shell-specific LSP settings
                (setq-local eglot-workspace-configuration
                           '((:bashIde . (:globPattern "**/*@(.sh|.inc|.bash|.command)"))))))))

;; Enhanced shell mode settings
(use-package sh-script
  :ensure nil
  :mode (("\\.sh\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode)
         ("\\.zsh\\'" . sh-mode)
         ("\\.fish\\'" . sh-mode))
  :config
  ;; Set default shell for sh-mode
  (setq sh-basic-offset 2
        sh-indentation 2)
  
  ;; Auto-detect shell type
  (add-hook 'sh-mode-hook
            (lambda ()
              (if (string-match "\\.zsh\\'" (buffer-name))
                  (sh-set-shell "zsh")
                (if (string-match "\\.fish\\'" (buffer-name))
                    (sh-set-shell "fish")
                  (sh-set-shell "bash"))))))

;; Key bindings for shell development
(defun shell-mode-setup ()
  "Setup shell mode with custom keybindings."
  (local-set-key (kbd "C-c C-c") 'executable-interpret)
  (local-set-key (kbd "C-c C-x") 'executable-set-magic))

(add-hook 'sh-mode-hook 'shell-mode-setup)

(provide 'init-shell)

;;; init-shell.el ends here