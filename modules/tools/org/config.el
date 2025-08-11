;;; modules/tools/org/config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Main org-mode configuration loader - loads core org and org-roam configurations
;;; Code:

;; Load core org-mode configuration
(load-file (expand-file-name "org.el" (file-name-directory load-file-name)))

;; Load org-roam configuration
(load-file (expand-file-name "org-roam.el" (file-name-directory load-file-name)))

(provide 'org-config)
;;; modules/tools/org/config.el ends here