;;; lua-setup.el --- Lua development configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Basic Lua language support
;;; Code:

;; Ensure lua-mode is installed
(unless (package-installed-p 'lua-mode)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'lua-mode))

;; Find and add lua-mode package directory to load-path
(let* ((elpa-dir (expand-file-name "elpa" user-emacs-directory))
       (lua-mode-dirs (when (file-directory-p elpa-dir)
                       (directory-files elpa-dir t "lua-mode-[0-9]")))
       (lua-mode-dir (car lua-mode-dirs)))
  (when (and lua-mode-dir (file-directory-p lua-mode-dir))
    (add-to-list 'load-path lua-mode-dir)))

;; Load lua-mode
(require 'lua-mode nil t)

;; Basic lua-mode setup
(when (featurep 'lua-mode)
  ;; Basic configuration
  (setq lua-indent-level 2)

  ;; File associations
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
  (add-to-list 'auto-mode-alist '("\\.rockspec\\'" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

;; Alternative fallback: try to find and load lua-mode.el directly
(unless (featurep 'lua-mode)
  (let* ((elpa-dir (expand-file-name "elpa" user-emacs-directory))
         (lua-mode-dirs (when (file-directory-p elpa-dir)
                         (directory-files elpa-dir t "lua-mode-[0-9]")))
         (lua-mode-el (when lua-mode-dirs
                       (expand-file-name "lua-mode.el" (car lua-mode-dirs)))))
    (when (and lua-mode-el (file-exists-p lua-mode-el))
      (condition-case nil
          (load lua-mode-el)
        (error nil)))))

(provide 'lua-setup)
;;; lua-setup.el ends here