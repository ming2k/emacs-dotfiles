;;; init-verilog.el -*- lexical-binding: t; -*-

;; Verilog mode is built into Emacs
(use-package verilog-mode
  :mode (("\\.v\\'" . verilog-mode)
         ("\\.sv\\'" . verilog-mode))  ; SystemVerilog
  :config
  (setq verilog-indent-level 2
        verilog-indent-level-module 2
        verilog-indent-level-declaration 2
        verilog-indent-level-behavioral 2
        verilog-indent-level-directive 2
        verilog-case-indent 2
        verilog-auto-newline nil
        verilog-auto-indent-on-newline t
        verilog-tab-always-indent t
        verilog-auto-endcomments t
        verilog-minimum-comment-distance 10
        verilog-indent-begin-after-if t
        verilog-auto-lineup 'all
        verilog-highlight-p1800-keywords t))

(provide 'init-verilog)
