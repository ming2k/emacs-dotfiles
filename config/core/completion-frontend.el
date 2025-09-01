;;; config/core/completion-ui.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Modern completion system using corfu, orderless, and vertico
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

;; Ensure packages are available
(unless (package-installed-p 'orderless)
  (package-refresh-contents))

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

;; Load LSP module
(require 'lsp)

;; Enhanced dabbrev for better word completion
(use-package dabbrev
  :ensure nil
  :config
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; Programming mode completion setup
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list #'safe-dabbrev-capf #'comint-filename-completion))))

;; Manual completion triggers
(global-set-key (kbd "C-c c") 'completion-at-point)
(global-set-key (kbd "M-/") 'completion-at-point)

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

(provide 'completion-frontend)
;;; config/core/completion-ui.el ends here
