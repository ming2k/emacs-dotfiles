;;; init-cmake.el -*- lexical-binding: t; -*-

;; Built-in Tree-sitter CMake mode
(use-package cmake-ts-mode
  :ensure nil
  :mode (("CMakeLists\\.txt\\'" . cmake-ts-mode)
         ("\\.cmake\\'" . cmake-ts-mode)))

;; CMake mode configuration
(add-hook 'cmake-ts-mode-hook
          (lambda ()
            (setq-local cmake-ts-indent-offset 4
                        tab-width 4
                        indent-tabs-mode nil)))

;; Enable eglot for CMake if cmake-language-server is available
(add-hook 'cmake-ts-mode-hook #'eglot-ensure)

(provide 'init-cmake)
