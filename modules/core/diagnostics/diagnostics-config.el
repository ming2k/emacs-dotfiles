;;; modules/core/diagnostics/diagnostics-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Diagnostics module - error checking and linting configuration
;;; Code:

;; Flymake configuration - LSP-only backend (enabled per language module)
(use-package flymake
  :ensure nil
  :config
  ;; Disable all non-LSP backends
  (setq flymake-no-changes-timeout nil
        flymake-start-on-flymake-mode t
        flymake-start-on-save-buffer t
        flymake-proc-compilation-prevents-syntax-check nil)
  
  ;; Clear all diagnostic functions to only use LSP
  (setq-default flymake-diagnostic-functions nil)
  
  ;; Keybindings for flymake navigation
  :bind (:map flymake-mode-map
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! L" . flymake-show-project-diagnostics)
              ("C-c ! c" . flymake-start)))

;; Configure flymake to work only with eglot
(with-eval-after-load 'eglot
  ;; Ensure eglot adds itself to flymake when starting
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Clear any existing diagnostic functions
              (setq-local flymake-diagnostic-functions nil)
              ;; Only enable eglot's diagnostics
              (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
              ;; Start flymake if not already active
              (unless flymake-mode
                (flymake-mode 1)))))

;; Clear non-LSP flymake backends in programming modes  
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local flymake-diagnostic-functions nil)))

;; Disable flymake's built-in syntax checkers
(with-eval-after-load 'flymake
  ;; Remove the legacy flymake-proc backend
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))

(provide 'diagnostics-config)
;;; modules/core/diagnostics/diagnostics-config.el ends here