;;; modules/ui/completion/config.el -*- lexical-binding: t; -*-

;; Built-in completion settings
(setq completion-cycle-threshold 3
      tab-always-indent t
      completion-ignore-case t
      read-extended-command-predicate #'command-completion-default-include-p
      enable-recursive-minibuffers t
      resize-mini-windows t
      max-mini-window-height 0.33
      completion-pcm-complete-word-inserts-delimiters t
      completion-pcm-word-delimiters "-_./:| ")

;; Enhanced completion-in-region settings
(setq completion-in-region-function #'completion--in-region
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
        completion-category-overrides '((file (styles partial-completion))
                                       (command (styles orderless))
                                       (variable (styles orderless))
                                       (symbol (styles orderless))))
  
  ;; Orderless configuration for better matching
  (setq orderless-matching-styles '(orderless-literal
                                   orderless-regexp
                                   orderless-flex)))

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

;; Cape for additional completion backends
(use-package cape
  :ensure t
  :config
  ;; Add completion functions to completion-at-point-functions
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  
  ;; Programming mode enhancements
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'cape-keyword t)
              (add-to-list 'completion-at-point-functions #'cape-symbol t)))
  
  ;; Enhanced completion setup for better integration
  (defun cape-capf-setup-builtin ()
    "Setup cape completion functions for built-in completion."
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'cape-dabbrev
                       #'cape-file
                       #'cape-keyword))))
  
  (add-hook 'text-mode-hook #'cape-capf-setup-builtin))

;; Global eglot configuration for all programming languages
(use-package eglot
  :ensure nil
  :config
  ;; Global eglot settings
  (setq eglot-sync-connect nil
        eglot-autoshutdown t
        eglot-extend-to-xref t
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.5
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

;; Programming-specific completion enhancements
(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Prioritize eglot completion
              (setq-local completion-at-point-functions
                          (list (cape-capf-super
                                 #'eglot-completion-at-point
                                 #'cape-dabbrev
                                 #'cape-file
                                 #'cape-keyword))))))

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

;; Built-in completion enhancements
(defun setup-builtin-completion ()
  "Setup built-in completion for programming modes."
  ;; Enhanced completion settings for programming
  (setq-local completion-at-point-functions
              (append completion-at-point-functions
                      '(cape-dabbrev cape-file cape-keyword))))

;; Org-mode specific completion setup
(defun setup-org-mode-completion ()
  "Setup completion for org-mode with built-in completion."
  ;; Custom function to check if we should complete in org-mode
  (setq-local completion-at-point-functions
              (list (lambda ()
                      ;; Don't complete in org headers/keywords
                      (when (not (org-in-keyword-p))
                        (funcall (cape-capf-super
                                 #'cape-dabbrev
                                 #'cape-file)))))))

(defun org-in-keyword-p ()
  "Check if point is in an org keyword line (#+TITLE, #+AUTHOR, etc.)."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*#\\+")))

;; Apply built-in completion to programming modes
(add-hook 'prog-mode-hook #'setup-builtin-completion)
;; Use selective completion for org-mode
(add-hook 'org-mode-hook #'setup-org-mode-completion)
;; Apply built-in completion to other text modes (excluding org-mode)
(add-hook 'text-mode-hook 
          (lambda ()
            (unless (derived-mode-p 'org-mode)
              (setup-builtin-completion))))

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
(global-set-key (kbd "C-c SPC") 'completion-at-point)
(global-set-key (kbd "C-M-i") 'completion-at-point)
(global-set-key (kbd "C-M-/") 'dabbrev-completion)

;; Manual completion triggers
(defun manual-completion ()
  "Manually trigger completion."
  (interactive)
  (completion-at-point))

(global-set-key (kbd "C-c c") 'manual-completion)
(global-set-key (kbd "M-/") 'manual-completion)

;; Consult for enhanced completion commands
(use-package consult
  :ensure t
  :config
  ;; Configure consult for better formatting and performance
  (setq consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1
        consult-preview-key '(:debounce 0.2 any)
        consult-fontify-preserve t
        consult-fontify-max-size 1048576)
  
  ;; Configure project root detection for file searches
  (setq consult-project-function 
        (lambda (_) 
          (cond
           ((and (fboundp 'projectile-project-root) (projectile-project-root))
            (projectile-project-root))
           ((vc-root-dir))
           (t default-directory))))
  
  ;; Standard consult keybindings (2025 best practices)
  :bind (("C-c f" . consult-find)
         ("C-c r" . consult-recent-file)
         ("C-x b" . consult-buffer)
         ("C-s" . consult-line)
         ("C-M-s" . consult-line-multi)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; Configure the command line tools that consult uses
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure register formatting
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format))

;; Completion enhancements
(setq completion-show-help t
      completion-auto-help t
      completion-auto-select nil)

;; Normal TAB behavior (indent only)
(setq tab-always-indent t)

(provide 'completion-config)
;;; modules/ui/completion/config.el ends here