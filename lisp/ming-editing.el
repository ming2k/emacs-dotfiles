;;; ming-editing.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Core editing features including completion, diagnostics, and fill-column settings
;;; Code:


;;; TreeSitter Dir Config
;; (setq treesit-extra-load-path '("/home/ming/.emacs.d/tree-sitter"))
;; (unless (file-exists-p "/home/ming/.emacs.d/tree-sitter")
;;   (make-directory "/home/ming/.emacs.d/tree-sitter" t))

(global-visual-line-mode 1)

;;; Sentence Wrap
(add-hook 'prog-mode-hook
          (lambda ()
            (visual-line-mode -1)
            (setq truncate-lines t)
            (setq fill-column 80)
            (setq display-fill-column-indicator-column 80)
            (display-fill-column-indicator-mode 1)
            (auto-fill-mode 1)))

;;; Clear flymake backends in programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local flymake-diagnostic-functions nil)))

;;; Spell checking setup (disabled by default)
(setq flyspell-mode nil)

;;; Built-in Completion Settings
(setq completion-cycle-threshold 3
      tab-always-indent 'complete
      completion-ignore-case t
      read-extended-command-predicate #'command-completion-default-include-p
      enable-recursive-minibuffers t
      resize-mini-windows t
      max-mini-window-height 0.33)

;;; Enhanced Search
(setq isearch-allow-scroll t
      isearch-lazy-highlight t
      lazy-highlight-cleanup nil
      lazy-highlight-initial-delay 0)

;;; Orderless - Flexible completion matching
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion orderless))))
  (setq orderless-matching-styles '(orderless-literal orderless-prefixes orderless-initialism)))

;;; Vertico - Enhanced minibuffer completion
(use-package vertico
  :ensure t
  :init
  (vertico-mode 1)
  :config
  (setq vertico-count 15
        vertico-resize t
        vertico-cycle t))

;;; Marginalia - Rich annotations in minibuffer
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1))

;;; Corfu - In-buffer completion popup
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :config
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 1
        corfu-popupinfo-delay '(0.5 . 0.2))
  :bind (:map corfu-map
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous)
              ("C-g" . corfu-quit)
              ("TAB" . corfu-insert)
              ("RET" . nil)
              ("M-n" . corfu-popupinfo-scroll-up)
              ("M-p" . corfu-popupinfo-scroll-down)
              ("C-h" . corfu-popupinfo-documentation)))

;;; Enhanced dabbrev for word completion
(use-package dabbrev
  :ensure nil
  :config
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;;; Code Folding
(add-hook 'prog-mode-hook 'hs-minor-mode)

(use-package savefold
  :ensure t
  :init
  (savefold-mode 1)
  :config
  (setq savefold-backends '(outline org hideshow)))

;;; TreeSit Folding
(use-package treesit-fold
  :ensure t
  :config
  ;; Enable treesit-fold for actual treesit modes
  (defun enable-treesit-fold-conditionally ()
    "Enable treesit-fold only if current buffer uses tree-sitter."
    (when (and (bound-and-true-p treesit-parser-list)
               treesit-parser-list)
      (treesit-fold-mode 1)))

  ;; Enable for all treesit modes
  (dolist (mode '(python-ts-mode-hook
                  rust-ts-mode-hook
                  typescript-ts-mode-hook
                  tsx-ts-mode-hook
                  js-ts-mode-hook
                  go-ts-mode-hook
                  c-ts-mode-hook
                  c++-ts-mode-hook
                  yaml-ts-mode-hook
                  json-ts-mode-hook
                  css-ts-mode-hook
                  html-ts-mode-hook))
    (add-hook mode #'treesit-fold-mode))

  ;; Add conditional hook for any prog-mode that might have treesit
  (add-hook 'prog-mode-hook #'enable-treesit-fold-conditionally)

  :bind (:map prog-mode-map
              ("C-c f t" . treesit-fold-toggle)
              ("C-c f o" . treesit-fold-open-all)
              ("C-c f c" . treesit-fold-close-all)))

;;; YASnippet - Template system
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs (list
                          (expand-file-name "snippets" user-emacs-directory))
        yas-trigger-key (kbd "C-j")))

;;; Flymake - Error checking (LSP-only)
(use-package flymake
  :ensure nil
  :config
  (setq flymake-no-changes-timeout 0.2
        flymake-start-on-flymake-mode t
        flymake-start-on-save-buffer t)
  (setq-default flymake-diagnostic-functions nil)
  :bind (:map flymake-mode-map
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)))

;;; Eglot - LSP client
(use-package eglot
  :ensure nil
  :config
  (setq eglot-sync-connect nil
        eglot-autoshutdown t
        eglot-extend-to-xref t
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.1
        eglot-ignored-server-capabilities '(:hoverProvider :documentHighlightProvider))
  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("C-c a" . eglot-code-actions)))

;;; LSP integration with flymake
(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local flymake-diagnostic-functions nil)
              (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
              (unless flymake-mode (flymake-mode 1))))
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list #'eglot-completion-at-point)))))

(provide 'ming-editing)
;;; ming-editing.el ends here
