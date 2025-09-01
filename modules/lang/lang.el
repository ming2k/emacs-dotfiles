;;; modules/lang/lang.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Smart on-demand loading of language configurations based on available tools
;;; Code:

;; Language configuration with conditional loading based on available tools

;; C/C++ - Load if clangd is available
(when (executable-find "clangd")
  (require 'cc))

;; Python - Load if python3 is available
(when (executable-find "python3")
  (require 'python))

;; Rust - Load if rustc and cargo are available
(when (and (executable-find "rustc") (executable-find "cargo"))
  (require 'rust))

;; JavaScript - Load if node is available
(when (executable-find "node")
  (require 'javascript))

;; TypeScript - Load if typescript-language-server or tsc is available
(when (or (executable-find "typescript-language-server") 
          (executable-find "tsc"))
  (require 'typescript))

;; Go - Load if go is available
(when (executable-find "go")
  (require 'go))

;; Shell scripting - Load if bash is available (should be on most systems)
(when (executable-find "bash")
  (require 'shell))

;; Svelte - Load if svelte-language-server is available or if node is available
(when (or (executable-find "svelte-language-server")
          (executable-find "node"))
  (require 'svelte))

;; Lisp - Always load (built-in Emacs support)
(require 'lisp)

;; JSON - Always load (universal format)
(require 'json)

;; Markdown - Always load (common documentation format)
(require 'markdown)

;; Zig - Load if zig is available
(when (executable-find "zig")
  (require 'zig))

(provide 'lang)
;;; modules/lang/lang.el ends here