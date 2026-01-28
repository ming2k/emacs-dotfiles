;;; init-completion.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Complete completion suite including minibuffer and in-buffer completion

;;; Code:

;;; Built-in Completion Settings
(setq completion-cycle-threshold 3
      tab-always-indent 'complete
      completion-ignore-case t
      read-extended-command-predicate #'command-completion-default-include-p
      enable-recursive-minibuffers t
      resize-mini-windows t
      max-mini-window-height 0.33)

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

;;; Consult - Enhanced search and navigation commands
(use-package consult
  :ensure t)

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

  ;; Disable corfu-auto in the minibuffer: in shell-command (M-!) when only one
  ;; candidate matches, auto-completion commits it and inserts an extra space,
  ;; interrupting typing.
  (add-hook 'minibuffer-setup-hook (lambda () (setq-local corfu-auto nil)))

  ;; Disable corfu-mode in org-mode buffers
  (add-hook 'org-mode-hook (lambda () (corfu-mode -1)))
  :bind (:map corfu-map
              ;; ("TAB"        . corfu-next)
              ;; ("S-TAB"      . corfu-previous)
              ;; ("S-RET"      . corfu-quit)
              ("<return>"     . corfu-insert)
              ("<escape>"     . corfu-quit)
              ("M-n" . corfu-popupinfo-scroll-up)
              ("M-p" . corfu-popupinfo-scroll-down)
              ("C-h" . corfu-popupinfo-documentation)))

;;; Dabbrev - Dynamic abbreviation expansion
;; Built-in completion that expands words based on text in open buffers.
;; M-/ (dabbrev-expand): cycle through expansions from current and other buffers.
;; C-M-/ (dabbrev-completion): show all matching expansions via completion UI.
;; Useful as a lightweight, language-agnostic complement to LSP/Corfu.
(use-package dabbrev
  :ensure nil
  :config
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(provide 'init-completion)
;;; init-completion.el ends here
