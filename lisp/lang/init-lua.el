;;; init-lua.el --- Lua development configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Basic Lua language support
;;; Code:

(use-package lua-mode
  :ensure t
  :mode (("\\.lua\\'" . lua-mode)
         ("\\.rockspec\\'" . lua-mode))
  :interpreter ("lua" . lua-mode)
  :config
  (setq lua-indent-level 2))

(provide 'init-lua)
;;; init-lua.el ends here