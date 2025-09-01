;;; modules/ui/themes/themes-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Theme management and switching configuration
;;; Code:

;; Add themes directory to custom-theme-load-path
(add-to-list 'custom-theme-load-path 
             (file-name-directory (or load-file-name buffer-file-name)))

;; Available themes - change the selected theme here
(defvar my-selected-theme 'gruvbox-dark
  "The theme to load. Available themes: 'gruvbox-dark, 'gruvbox-light, 'dracula")

;; Theme switching function
(defun switch-theme (theme-name)
  "Switch to THEME-NAME theme."
  (interactive
   (list (intern (completing-read "Theme: " '("gruvbox-dark" "gruvbox-light" "dracula")))))
  (load-theme theme-name t))

;; Load the selected theme
(load-theme my-selected-theme t)

;; Keybinding to switch themes quickly
(global-set-key (kbd "C-c t") 'switch-theme)

(provide 'themes-config)

;;; modules/ui/themes/themes-config.el ends here
