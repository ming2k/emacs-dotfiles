;;; init-dart.el --- Dart and Flutter support -*- lexical-binding: t; -*-

;;; Commentary:
;; Dart and Flutter configuration using dart-mode and flutter.el

;;; Code:

(use-package dart-mode
  :ensure t
  :mode ("\\.dart\\'" . dart-mode)
  :hook (dart-mode . eglot-ensure)
  :config
  (setq dart-format-on-save t))

(use-package flutter
  :ensure t
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-c C-r" . flutter-run-or-hot-reload)
              ("C-c C-l" . flutter-hot-restart)))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(dart-mode . ("dart" "language-server" "--client-id" "emacs.eglot-dart"))))

(provide 'init-dart)
;;; init-dart.el ends here
