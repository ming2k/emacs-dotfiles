;;; init-cmake.el -*- lexical-binding: t; -*-

;; CMake mode
(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;; CMake mode configuration
(add-hook 'cmake-mode-hook
          (lambda ()
            (setq-local cmake-tab-width 4
                        tab-width 4
                        indent-tabs-mode nil)))

;; Enable eglot for CMake if cmake-language-server is available
(add-hook 'cmake-mode-hook #'eglot-ensure)

(provide 'init-cmake)
