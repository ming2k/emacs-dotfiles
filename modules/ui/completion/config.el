;;; modules/ui/completion/config.el -*- lexical-binding: t; -*-

;; Enhanced completion settings
(setq completion-cycle-threshold 3
      tab-always-indent 'complete
      completion-ignore-case t
      read-extended-command-predicate #'command-completion-default-include-p
      enable-recursive-minibuffers t)

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
        vertico-cycle t)
  
  ;; Vertico keybindings
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              ("M-h" . vertico-directory-up)))

;; Enhanced Marginalia for rich annotations in minibuffer
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1)
  :config
  ;; Enhanced marginalia settings
  (setq marginalia-max-relative-age 0
        marginalia-align 'right
        marginalia-align-offset -1
        marginalia-truncate-width 80
        marginalia-separator "  ")
  
  ;; Bind marginalia-cycle to cycle between annotation levels
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

;; Corfu for in-buffer completion popup
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode 1)
  :config
  ;; Enhanced corfu settings for prominent popup
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 1
        corfu-quit-at-boundary nil
        corfu-quit-no-match nil
        corfu-preview-current t
        corfu-scroll-margin 5
        corfu-max-width 120
        corfu-min-width 15
        corfu-count 15
        corfu-preselect 'prompt)
  
  ;; Enhanced corfu keybindings
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ("<tab>" . corfu-next)
              ("S-TAB" . corfu-previous)
              ("<backtab>" . corfu-previous)
              ("RET" . corfu-insert)
              ("<return>" . corfu-insert)
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous)
              ("M-d" . corfu-info-documentation)
              ("M-l" . corfu-info-location)
              ("C-g" . corfu-quit)))

;; Remove redundant annotations by disabling them
(setq corfu-margin-formatters nil)

;; Optional: Keep kind-icon available but commented out
;; (use-package kind-icon
;;   :ensure t
;;   :after corfu
;;   :custom
;;   (kind-icon-use-icons t)
;;   (kind-icon-default-face 'corfu-default)
;;   (kind-icon-blend-background nil)
;;   (kind-icon-blend-frac 0.08)
;;   :config
;;   ;; Add kind-icon to corfu
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Corfu popupinfo for documentation popup
(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay '(0.5 . 0.2)
        corfu-popupinfo-max-width 80
        corfu-popupinfo-max-height 20)
  :bind (:map corfu-map
              ("M-p" . corfu-popupinfo-scroll-down)
              ("M-n" . corfu-popupinfo-scroll-up)
              ("M-t" . corfu-popupinfo-toggle)))

;; Corfu terminal support (for terminal Emacs)
(use-package corfu-terminal
  :ensure t
  :when (not (display-graphic-p))
  :config
  (corfu-terminal-mode 1))

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
  
  ;; Enhanced completion setup for better corfu integration
  (defun cape-capf-setup-corfu ()
    "Setup cape completion functions for corfu."
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'cape-dabbrev
                       #'cape-file
                       #'cape-keyword))))
  
  (add-hook 'text-mode-hook #'cape-capf-setup-corfu))

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

;; Enhanced dabbrev
(use-package dabbrev
  :ensure nil
  :config
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; Completion performance optimizations
(setq completion-pcm-complete-word-inserts-delimiters t
      completion-pcm-word-delimiters "-_./:| ")

;; Use corfu for in-region completion
(setq completion-in-region-function #'corfu-completion-in-region)

;; Enhanced corfu behavior
(defun setup-corfu-aggressive ()
  "Setup aggressive corfu completion."
  ;; Make corfu popup more often
  (setq-local corfu-auto-delay 0.0
              corfu-auto-prefix 1)
  ;; Enhance completion functions
  (setq-local completion-at-point-functions
              (append completion-at-point-functions
                      '(cape-dabbrev cape-file cape-keyword))))

;; Org-mode specific completion setup
(defun setup-org-mode-completion ()
  "Setup completion for org-mode with selective corfu."
  (setq-local corfu-auto-delay 0.3
              corfu-auto-prefix 2)
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

;; Apply aggressive corfu to programming modes
(add-hook 'prog-mode-hook #'setup-corfu-aggressive)
;; Use selective completion for org-mode
(add-hook 'org-mode-hook #'setup-org-mode-completion)
;; Apply aggressive corfu to other text modes (excluding org-mode)
(add-hook 'text-mode-hook 
          (lambda ()
            (unless (derived-mode-p 'org-mode)
              (setup-corfu-aggressive))))

;; Ensure corfu works in minibuffer when needed
(defun corfu-enable-in-minibuffer ()
  "Enable corfu in minibuffer if vertico is not active."
  (when (not (bound-and-true-p vertico--input))
    (setq-local corfu-auto nil)
    (corfu-mode 1)))

(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

;; Enhanced marginalia integration helper functions
(defun marginalia-toggle-annotations ()
  "Toggle marginalia annotations on/off."
  (interactive)
  (if marginalia-mode
      (marginalia-mode -1)
    (marginalia-mode 1))
  (message "Marginalia annotations %s" (if marginalia-mode "enabled" "disabled")))

;; Bind the toggle function
(global-set-key (kbd "C-c m t") 'marginalia-toggle-annotations)