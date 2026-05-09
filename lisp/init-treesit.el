;;; init-treesit.el --- Tree-sitter configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Built-in Tree-sitter support.
;;; Code:

(use-package treesit
  :ensure nil
  :custom
  ;; Ask before downloading and building a missing grammar.
  (treesit-auto-install-grammar 'ask)
  (treesit-enabled-modes t)
  (treesit-font-lock-level 4)
  :config
  (add-to-list
   'treesit-language-source-alist
   '(zig "https://github.com/tree-sitter-grammars/tree-sitter-zig")))

(provide 'init-treesit)
;;; init-treesit.el ends here
