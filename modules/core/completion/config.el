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
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("S-<return>" . corfu-insert)
        ("RET" . nil))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay '(0.5 . 0.2)))

;; Cape for additional completion backends
(use-package cape
  :ensure t
  :config
  ;; Add useful completion functions
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  
  ;; Programming mode enhancements
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'cape-keyword t)))
  
  ;; Enhanced completion setup
  (defun cape-setup-capf ()
    "Setup cape completion at point functions."
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'cape-dabbrev #'cape-file))))
  
  (add-hook 'text-mode-hook #'cape-setup-capf))

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
                          (list (cape-capf-properties
                                 #'eglot-completion-at-point
                                 :exclusive 'no)
                                #'cape-dabbrev
                                #'cape-file))
              ;; Disable traditional flymake checkers when eglot is active
              (when (bound-and-true-p flymake-mode)
                (setq-local flymake-diagnostic-functions
                           (list #'eglot-flymake-backend))))))

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
              (list #'cape-dabbrev #'cape-file #'cape-keyword)))

;; Org-mode specific completion setup  
(defun setup-org-mode-completion ()
  "Setup completion for org-mode."
  (setq-local completion-at-point-functions
              (list #'cape-dabbrev #'cape-file)))

;; Apply completion setups
(add-hook 'prog-mode-hook #'setup-prog-mode-completion)
(add-hook 'org-mode-hook #'setup-org-mode-completion)
(add-hook 'text-mode-hook 
          (lambda ()
            (unless (derived-mode-p 'org-mode)
              (setq-local completion-at-point-functions
                          (list #'cape-dabbrev #'cape-file)))))

;; Enhanced marginalia integration helper functions
(defun marginalia-toggle-annotations ()
  "Toggle marginalia annotations on/off."
  (interactive)
  (if marginalia-mode
      (marginalia-mode -1)
    (marginalia-mode 1))
  (message "Marginalia annotations %s" (if marginalia-mode "enabled" "disabled")))

;; Manual completion shortcuts
(global-set-key (kbd "C-c m t") 'marginalia-toggle-annotations)
(global-set-key (kbd "C-c TAB") 'completion-at-point)
(global-set-key (kbd "C-M-i") 'completion-at-point)
(global-set-key (kbd "C-M-/") 'dabbrev-completion)

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

(provide 'completion-config)
;;; modules/core/completion/config.el ends here