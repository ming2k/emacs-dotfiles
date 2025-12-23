;;; init-editing.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Core editing features including visual modes, code folding, snippets, and encryption

;;; Code:

;;; TreeSitter Dir Config
;; (setq treesit-extra-load-path '("/home/ming/.emacs.d/tree-sitter"))
;; (unless (file-exists-p "/home/ming/.emacs.d/tree-sitter")
;;   (make-directory "/home/ming/.emacs.d/tree-sitter" t))

(global-visual-line-mode 1)

;;; Fill column indicator - disabled by default, enabled only in prog-mode
(setq-default display-fill-column-indicator-mode nil)

;;; Sentence Wrap
;; Enable fill column indicator only in programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (visual-line-mode -1)
            (setq truncate-lines t)
            (setq fill-column 80)
            (setq display-fill-column-indicator-column 80)
            (display-fill-column-indicator-mode 1)
            (auto-fill-mode 1)))

;; Disable fill column indicator in *scratch* buffer (lisp-interaction-mode)
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (display-fill-column-indicator-mode -1)))

;;; Spell checking setup (disabled by default)
(setq flyspell-mode nil)

;;; Enhanced Search
(setq isearch-allow-scroll t
      isearch-lazy-highlight t
      lazy-highlight-cleanup nil
      lazy-highlight-initial-delay 0)

;;; Code Folding
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Savefold - Automatically save and restore code folding state across sessions
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

;;; EasyPG - GPG encryption support
;; All GPG operations use loopback pinentry (password prompts in minibuffer)
(use-package epa-file
  :ensure nil
  :config
  (epa-file-enable)
  ;; Use loopback pinentry for all GPG operations
  (setq epg-pinentry-mode 'loopback
        ;; Let GPG agent handle caching
        epa-file-cache-passphrase-for-symmetric-encryption nil
        epa-file-select-keys nil)

  ;; Ensure EPA uses loopback globally
  (setenv "GPG_AGENT_INFO" nil))  ; Force Emacs to use gpg-agent with loopback

(provide 'init-editing)
;;; init-editing.el ends here
