;;; init-lua.el --- Lua development configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Built-in Tree-sitter Lua support
;;; Code:

(use-package lua-ts-mode
  :ensure nil
  :mode (("\\.lua\\'" . lua-ts-mode)
         ("\\.rockspec\\'" . lua-ts-mode))
  :interpreter ("lua" . lua-ts-mode)
  :config
  (setq lua-ts-indent-offset 2))

(provide 'init-lua)
;;; init-lua.el ends here
