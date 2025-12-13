;;; eww-setup.el --- EWW (Emacs Web Wowser) configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple EWW browser configuration

;;; Code:

(require 'eww)

;; Use external browser as default
(setq browse-url-browser-function 'browse-url-xdg-open)

;; Set default search engine
(setq eww-search-prefix "https://duckduckgo.com/html/?q=")

;; History settings
(setq eww-history-limit 150)

;; Download directory
(setq eww-download-directory "~/Downloads")

;; Keybindings
(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "o") 'eww)
  (define-key eww-mode-map (kbd "O") 'eww-browse-with-external-browser)
  (define-key eww-mode-map (kbd "f") 'ace-link-eww))

;; Enable cookies
(setq url-cookie-file (expand-file-name "cookies" user-emacs-directory))
(setq url-cookie-save-interval 3600)

(provide 'eww-setup)
;;; eww-setup.el ends here
