;;; config/system/paths-detailed.el --- Detailed file path configurations -*- lexical-binding: t; -*-
;;; Commentary:
;; Non-essential path configurations that can be loaded after basic setup
;;; Code:

;; Require basic XDG paths
(require 'xdg-paths)

;; Extended path configurations
(setq
 ;; History and state files  
 savehist-file (xdg-emacs-path xdg-state-home "history")
 save-place-file (xdg-emacs-path xdg-state-home "places")
 recentf-save-file (xdg-emacs-path xdg-state-home "recentf")
 
 ;; Bookmarks and abbreviations
 bookmark-default-file (xdg-emacs-path xdg-data-home "bookmarks")
 abbrev-file-name (xdg-emacs-path xdg-data-home "abbrev_defs")
 
 ;; Desktop session
 desktop-path (list (xdg-emacs-path xdg-state-home ""))
 ;;desktop-dirname (xdg-emacs-path xdg-state-home "")
 desktop-base-file-name "desktop"
 desktop-base-lock-name "desktop.lock"
 
 ;; Transient (Magit popups)
 transient-history-file (xdg-emacs-path xdg-state-home "transient/history.el")
 transient-levels-file (xdg-emacs-path xdg-state-home "transient/levels.el")
 transient-values-file (xdg-emacs-path xdg-state-home "transient/values.el")
 
 ;; Shell and network
 eshell-directory-name (xdg-emacs-path xdg-state-home "eshell/")
 url-cache-directory (xdg-emacs-path xdg-cache-home "url/")
 url-configuration-directory (xdg-emacs-path xdg-state-home "url/")
 
 ;; Other tools
 tramp-persistency-file-name (xdg-emacs-path xdg-state-home "tramp")
 temporary-file-directory (xdg-emacs-path xdg-cache-home "tmp/"))

;; Package-specific paths (set when packages are loaded)
(with-eval-after-load 'prescient
  (setq prescient-save-file (xdg-emacs-path xdg-state-home "prescient-save.el")))

(with-eval-after-load 'projectile
  (setq projectile-known-projects-file (xdg-emacs-path xdg-state-home "projectile-bookmarks.eld")))

(with-eval-after-load 'org-roam
  (setq org-roam-directory (xdg-emacs-path xdg-data-home "org-roam/")))

(provide 'paths-detailed)
;;; paths-detailed.el ends here
