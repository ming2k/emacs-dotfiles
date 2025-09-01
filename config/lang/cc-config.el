;;; config/lang/cc-config.el -*- lexical-binding: t; -*-

(setq-default c-default-style "linux"
              c-basic-offset 4)

(add-hook 'c-mode-hook (lambda ()
                         (setq-local c-basic-offset 4
                                     tab-width 4
                                     indent-tabs-mode nil)))
(add-hook 'c++-mode-hook (lambda ()
                           (setq-local c-basic-offset 4
                                       tab-width 4
                                       indent-tabs-mode nil)))
(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)

(provide 'cc-config)

;;; config/lang/cc-config.el ends here
