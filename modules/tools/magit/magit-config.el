;;; modules/tools/magit/magit-config.el -*- lexical-binding: t; -*-

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-dispatch))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package git-gutter
  :ensure t
  :hook ((prog-mode . git-gutter-mode)
         (text-mode . git-gutter-mode))
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-timemachine
  :ensure t
  :bind ("C-x v t" . git-timemachine))

(provide 'magit-config)
;;; modules/tools/magit/magit-config.el ends here