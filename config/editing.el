;; config/editing.el - Editing behavior and keybindings
;;; Commentary:
;; Editing enhancements and keybindings
;;; Code:

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; Expand region
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Smartparens
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil))

;; Better movement
(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)))

;; YASnippet - Simple setup with Corfu support
(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-global-mode 1))

;; Community snippets
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; Corfu integration for yasnippet
(use-package cape
  :ensure t
  :after (yasnippet corfu)
  :config
  ;; Add yasnippet to completion backends
  (add-to-list 'completion-at-point-functions #'cape-yasnippet))

;; Simple keybindings
(global-set-key (kbd "C-c s") 'yas-insert-snippet)

(provide 'editing)
;;; editing.el ends here
