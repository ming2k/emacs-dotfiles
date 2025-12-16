;;; init-project.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Project management configuration

;;; Code:

;; Built-in project.el configuration
(use-package project
  :ensure nil
  :config
  ;; Add .project as a project root marker
  (setq project-vc-extra-root-markers '(".project")))

(provide 'init-project)
;;; init-project.el ends here
