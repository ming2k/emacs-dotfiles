;;; ming-editing.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Core editing features including completion, diagnostics, and fill-column settings
;;; Code:

;;; Fill Column Settings
;; Set default fill column
(setq-default fill-column 80)

;;; Completion System
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
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("C-g" . corfu-quit)        
        ("TAB" . corfu-insert)
        ("RET" . nil))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay '(0.5 . 0.2))
  ;; Disable ispell for corfu completion
  (setq corfu-excluded-modes '(ispell-minor-mode flyspell-mode))
  ;; Remove ispell completion functions from completion-at-point-functions
  (add-hook 'corfu-mode-hook 
            (lambda ()
              (setq-local completion-at-point-functions
                          (remove #'ispell-completion-at-point completion-at-point-functions)))))

;; Enhanced dabbrev for better word completion
(use-package dabbrev
  :ensure nil
  :config
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;;; Folding

;; Enable hideshow minor mode for code folding
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Fold persistence
(use-package savefold
  :ensure t
  :init
  (setq savefold-backends '(outline org hideshow))
  (setq savefold-directory (locate-user-emacs-file "savefold"))  ;; default
  :config
  (savefold-mode 1))

;;; YASnippet - Template system
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  ;; Include both custom and official snippets
  (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  ;; Set TAB as the default expand key
  (setq yas-trigger-key "TAB")
  :bind
  (:map yas-minor-mode-map
        ("TAB" . yas-expand)
        ("C-c y" . yas-insert-snippet)))

;; YASnippet official snippets collection
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; Cape for completion extensions
(use-package cape
  :ensure t
  :after (corfu yasnippet)
  :config
  ;; Add cape completion functions (cape-yasnippet is built-in)
  (add-to-list 'completion-at-point-functions #'cape-yasnippet)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Enhanced isearch
(setq isearch-allow-scroll t
      isearch-lazy-highlight t
      lazy-highlight-cleanup nil
      lazy-highlight-initial-delay 0)

;; Completion enhancements
(setq completion-show-help t
      completion-auto-help t
      completion-auto-select nil)

;;; Diagnostics (Error Checking)
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

;; Configure hunspell as spell-checking backend
(setq ispell-program-name "hunspell"
      ispell-local-dictionary "en_US"
      ;; Set alternate dictionary to avoid plain word-list errors
      ispell-alternate-dictionary nil
      ;; Disable lookup words to avoid the error
      ispell-lookup-words nil
      ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

;; Disable flyspell-mode by default
(setq-default flyspell-mode nil)

;;; LSP Configuration
;; Eglot configuration - no auto-start hooks (opt-in per language module)
(use-package eglot
  :ensure nil
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


;; Programming-specific completion enhancements with eglot
(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Prioritize eglot completion when eglot is managing the buffer
              (setq-local completion-at-point-functions
                          (list #'eglot-completion-at-point
                                #'cape-yasnippet
                                #'cape-file
                                #'comint-filename-completion)))))

(provide 'ming-editing)
;;; ming-editing.el ends here
