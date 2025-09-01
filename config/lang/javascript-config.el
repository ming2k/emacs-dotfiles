;;; config/lang/javascript.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Vanilla JavaScript configuration using only built-in features
;;; Code:

;; JavaScript settings
(setq js-indent-level 2
      js-switch-indent-offset 2)

;; Built-in JavaScript mode
(use-package js
  :ensure nil
  :mode (("\\.js\\'" . js-mode)
         ("\\.jsx\\'" . js-mode)
         ("\\.mjs\\'" . js-mode))
  :hook ((js-mode . js-setup-minor-modes)
         (js-mode . eglot-ensure)
         (js-mode . flymake-mode))
  :config
  (setq js-indent-level 2
        js-switch-indent-offset 2))

;; JavaScript minor modes setup
(defun js-setup-minor-modes ()
  "Enable helpful minor modes for JavaScript."
  (electric-indent-local-mode 1)
  (subword-mode 1)
  (hs-minor-mode 1)
  (flyspell-prog-mode)
  (setq-local comment-auto-fill-only-comments t
              tab-width 2
              indent-tabs-mode nil))

;; Configure JavaScript LSP server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(js-mode . ("typescript-language-server" "--stdio"))))

(provide 'javascript-config)

;;; config/lang/javascript.el ends here