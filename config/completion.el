;;; config/completion.el --- Modern completion stack with vertico, corfu, marginalia, and orderless
;;; Commentary:
;; Modern completion configuration using vertico for minibuffer completion,
;; corfu for in-buffer completion, marginalia for annotations, and orderless for matching
;;; Code:

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

;; Marginalia for rich annotations in minibuffer
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1)
  :config
  (setq marginalia-max-relative-age 0
        marginalia-align 'center)
  
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
        corfu-auto-delay 0.1          ; Faster popup
        corfu-auto-prefix 1           ; Show after 1 character
        corfu-quit-at-boundary nil    ; Don't quit at word boundary
        corfu-quit-no-match nil       ; Don't quit when no match
        corfu-preview-current t       ; Show preview
        corfu-scroll-margin 5
        corfu-max-width 120           ; Wider popup
        corfu-min-width 15
        corfu-count 15                ; More completion candidates
        corfu-preselect 'prompt)      ; Preselect first candidate
  
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

;; Corfu popupinfo for documentation popup
(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay '(0.5 . 0.2)  ; Show docs quickly
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

;; Consult for enhanced search and navigation
(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :config
  ;; Consult configuration
  (setq consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)
  
  ;; Configure consult-ripgrep
  (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip"))

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

;; Which-key for command hints
(use-package which-key
  :ensure nil
  :init
  (which-key-mode 1)
  :config
  (setq which-key-idle-delay 0.5
        which-key-prefix-prefix "◉ "
        which-key-sort-order 'which-key-key-order-alpha
        which-key-min-display-lines 3
        which-key-max-display-columns nil))

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

;; Recent files
(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-saved-items 200
        recentf-exclude '("/tmp/" "/ssh:" "/sudo:" "\\.git/")))

;; Enhanced dabbrev
(use-package dabbrev
  :ensure nil
  :config
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; Completion performance optimizations
(setq completion-pcm-complete-word-inserts-delimiters t
      completion-pcm-word-delimiters "-_./:| ")

;; Use corfu for in-region completion (not consult)
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

;; Apply aggressive corfu to programming modes
(add-hook 'prog-mode-hook #'setup-corfu-aggressive)
(add-hook 'text-mode-hook #'setup-corfu-aggressive)

;; Ensure corfu works in minibuffer when needed
(defun corfu-enable-in-minibuffer ()
  "Enable corfu in minibuffer if vertico is not active."
  (when (not (bound-and-true-p vertico--input))
    (setq-local corfu-auto nil)
    (corfu-mode 1)))

(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

(provide 'completion)
;;; completion.el ends here
