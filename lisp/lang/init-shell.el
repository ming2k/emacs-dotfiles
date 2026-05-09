;;; init-shell.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Shell scripting support with bash-language-server
;;; Code:


;; Shell-specific LSP settings
(defun my/bash-eglot-workspace-config ()
  "Configure bash-language-server for the current workspace."
  (when (eglot-managed-p)
    (setq-local eglot-workspace-configuration
                '((:bashIde . (:globPattern "**/*@(.sh|.inc|.bash|.command)"))))))

;; Enhanced shell mode settings.
(use-package sh-script
  :ensure nil
  :mode (("\\.sh\\'" . bash-ts-mode)
         ("\\.bash\\'" . bash-ts-mode)
         ("\\.zsh\\'" . sh-mode)
         ("\\.fish\\'" . sh-mode))
  :interpreter (("bash" . bash-ts-mode)
                ("sh" . bash-ts-mode))
  :hook ((bash-ts-mode . eglot-ensure)
         (eglot-managed-mode . my/bash-eglot-workspace-config))
  :config
  (setq sh-basic-offset 2
        sh-indentation 2)

  ;; Keep unsupported shell dialects on sh-mode.
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
(add-hook 'bash-ts-mode-hook 'shell-mode-setup)

(provide 'init-shell)

;;; init-shell.el ends here
