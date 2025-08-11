;;; modules/core/completion/config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Modern completion system using corfu, orderless, vertico, and eglot
;;; Code:

;; Built-in completion settings
(setq completion-cycle-threshold 3
      tab-always-indent 'complete
      completion-ignore-case t
      read-extended-command-predicate #'command-completion-default-include-p
      enable-recursive-minibuffers t
      resize-mini-windows t
      max-mini-window-height 0.33
      completion-pcm-complete-word-inserts-delimiters t
      completion-pcm-word-delimiters "-_./:| ")

;; Enhanced completion-in-region settings for corfu
(setq completion-in-region-function nil
      completion-auto-help t
      completion-auto-select nil
      completion-show-help t
      completion-show-inline-help t)

;; Orderless completion style for flexible matching
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion orderless))
                                       (buffer (styles basic partial-completion orderless))
                                       (project-file (styles orderless))
                                       (command (styles orderless))
                                       (variable (styles orderless))
                                       (symbol (styles orderless))
                                       (eglot (styles orderless))
                                       (eglot-capf (styles orderless))))
  
  ;; Orderless configuration for literal matching (no wildcards)
  (setq orderless-matching-styles '(orderless-literal
                                   orderless-prefixes
                                   orderless-initialism
                                   orderless-flex)
        orderless-component-separator " +"))

;; Vertico for enhanced minibuffer completion
(use-package vertico
  :ensure t
  :init
  (vertico-mode 1)
  :config
  (setq vertico-count 15
        vertico-resize t
        vertico-cycle t
        vertico-scroll-margin 2)
  
  ;; Standard vertico keybindings
  :bind (:map vertico-map
              ("C-n" . vertico-next)
              ("C-p" . vertico-previous)
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("RET" . vertico-directory-enter)
              ("M-RET" . vertico-exit)
              ("DEL" . vertico-directory-delete-char)
              ("C-<backspace>" . vertico-directory-delete-word)
              ("C-w" . vertico-directory-delete-word)))

;; Enhanced Marginalia for rich annotations in minibuffer
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1)
  :config
  ;; Enhanced marginalia settings for better formatting
  (setq marginalia-max-relative-age 0
        marginalia-align 'right
        marginalia-align-offset -3
        marginalia-truncate-width 100
        marginalia-separator "   "
        marginalia-field-width 30)
  
  ;; Bind marginalia-cycle to cycle between annotation levels
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

;; Corfu for in-buffer completion popup
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  (corfu-separator ?\\ )
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  (corfu-scroll-margin 5)
  :bind
  (:map corfu-map
        ("TAB" . corfu-complete)
        ([tab] . corfu-complete)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("S-<return>" . corfu-insert)
        ("RET" . nil))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay '(0.5 . 0.2)))

;; Built-in completion backends
(defun setup-text-mode-completion ()
  "Setup completion for text modes using built-in functions."
  ;; Skip org-mode and org-roam as they have their own completion setup
  (unless (or (derived-mode-p 'org-mode)
              (and (boundp 'org-roam-directory)
                   (buffer-file-name)
                   (string-prefix-p (expand-file-name org-roam-directory)
                                    (expand-file-name (buffer-file-name)))))
    (setq-local completion-at-point-functions
                (list #'dabbrev-capf #'comint-filename-completion))))

(add-hook 'text-mode-hook #'setup-text-mode-completion)

;; Global eglot configuration for all programming languages
(use-package eglot
  :ensure nil
  :hook ((python-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure))
  :config
  ;; Global eglot settings
  (setq eglot-sync-connect nil
        eglot-autoshutdown t
        eglot-extend-to-xref t
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.3
        eglot-ignored-server-capabilities '(:hoverProvider :documentHighlightProvider))
  
  ;; Better eglot keybindings
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format-buffer)
              ("C-c l a" . eglot-code-actions)
              ("C-c l o" . eglot-code-action-organize-imports)
              ("C-c l h" . eldoc)
              ("M-." . eglot-find-declaration)
              ("M-?" . eglot-find-references)
              ("C-M-." . eglot-find-implementation)))

;; Programming-specific completion enhancements with corfu
(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Prioritize eglot completion for corfu
              (setq-local completion-at-point-functions
                          (list #'eglot-completion-at-point
                                #'dabbrev-capf
                                #'comint-filename-completion))
)))

;; Better minibuffer history
(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1)
  :config
  (setq savehist-length 1000
        savehist-additional-variables '(search-ring regexp-search-ring
                                       extended-command-history
                                       kill-ring)))

;; Enhanced dabbrev for better word completion
(use-package dabbrev
  :ensure nil
  :config
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; Completion enhancements for different modes
(defun setup-prog-mode-completion ()
  "Setup completion for programming modes."
  (setq-local completion-at-point-functions
              (list #'dabbrev-capf #'comint-filename-completion)))

;; Apply completion setups
(add-hook 'prog-mode-hook #'setup-prog-mode-completion)

;; Enhanced marginalia integration helper functions
(defun marginalia-toggle-annotations ()
  "Toggle marginalia annotations on/off."
  (interactive)
  (if marginalia-mode
      (marginalia-mode -1)
    (marginalia-mode 1))
  (message "Marginalia annotations %s" (if marginalia-mode "enabled" "disabled")))

;; Manual completion triggers that work with corfu
(defun manual-completion ()
  "Manually trigger completion."
  (interactive)
  (if (and (bound-and-true-p corfu-mode) (not completion-in-region-mode))
      (corfu-complete)
    (completion-at-point)))

(global-set-key (kbd "C-c c") 'manual-completion)
(global-set-key (kbd "M-/") 'manual-completion)

;; Enable recentf for recent files
(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-saved-items 50
        recentf-max-menu-items 15))

;; Enhanced isearch
(setq isearch-allow-scroll t
      isearch-lazy-highlight t
      lazy-highlight-cleanup nil
      lazy-highlight-initial-delay 0)

;; Completion enhancements
(setq completion-show-help t
      completion-auto-help t
      completion-auto-select nil)

;; Flymake configuration - LSP-only backend
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
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

;; Remove other flymake backends globally
(defun remove-flymake-backends ()
  "Remove all non-LSP flymake backends."
  ;; Remove common non-LSP backends that might be added by other packages
  (when (boundp 'flymake-diagnostic-functions)
    (setq-local flymake-diagnostic-functions
                (seq-filter (lambda (fn)
                              (eq fn #'eglot-flymake-backend))
                            flymake-diagnostic-functions))))

;; Apply backend removal to all programming modes
(add-hook 'prog-mode-hook #'remove-flymake-backends)

;; Disable flymake's built-in syntax checkers
(with-eval-after-load 'flymake
  ;; Remove the legacy flymake-proc backend
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))

(provide 'completion-config)
;;; modules/core/completion/config.el ends here
