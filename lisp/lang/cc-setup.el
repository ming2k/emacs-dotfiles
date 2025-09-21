;;; config/lang/cc-config.el -*- lexical-binding: t; -*-

(setq-default c-default-style "linux"
              c-basic-offset 4)

;; Tree-sitter C mode (preferred)
(use-package c-ts-mode
  :ensure nil
  :when (treesit-language-available-p 'c)
  :mode "\\.c\\'"
  :hook ((c-ts-mode . cc-setup-minor-modes)
         (c-ts-mode . eglot-ensure)
         (c-ts-mode . flymake-mode)))

;; Tree-sitter C++ mode (preferred)
(use-package c++-ts-mode
  :ensure nil
  :when (treesit-language-available-p 'cpp)
  :mode (("\\.cpp\\'" . c++-ts-mode)
         ("\\.cxx\\'" . c++-ts-mode)
         ("\\.cc\\'" . c++-ts-mode)
         ("\\.hpp\\'" . c++-ts-mode)
         ("\\.hxx\\'" . c++-ts-mode)
         ("\\.h\\'" . c++-ts-mode))
  :hook ((c++-ts-mode . cc-setup-minor-modes)
         (c++-ts-mode . eglot-ensure)
         (c++-ts-mode . flymake-mode)))

;; Fallback to regular c-mode/c++-mode when tree-sitter is not available
(unless (treesit-language-available-p 'c)
  (add-hook 'c-mode-hook #'cc-setup-minor-modes)
  (add-hook 'c-mode-hook #'eglot-ensure)
  (add-hook 'c-mode-hook #'flymake-mode))

(unless (treesit-language-available-p 'cpp)
  (add-hook 'c++-mode-hook #'cc-setup-minor-modes)
  (add-hook 'c++-mode-hook #'eglot-ensure)
  (add-hook 'c++-mode-hook #'flymake-mode))

;; Common setup for both C and C++ modes
(defun cc-setup-minor-modes ()
  "Enable helpful minor modes for C/C++."
  (setq-local c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil))

(provide 'cc-setup)

;;; config/lang/cc-config.el ends here
