;;; modules/core/project/config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Project management and navigation configuration - vanilla project.el behavior
;;; Code:

;; Use built-in project.el with vanilla behavior
(use-package project
  :ensure nil
  :config
  ;; Show all matches in results list instead of jumping to single file
  (setq project-search-show-all-matches t))

;;; modules/core/project/config.el ends here
