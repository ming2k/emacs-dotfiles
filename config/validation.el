;;; config/validation.el --- Configuration validation and health checks -*- lexical-binding: t; -*-
;;; Commentary:
;; Tools for validating and debugging the Emacs configuration
;;; Code:

(defun config-check-lsp-servers ()
  "Check if required LSP servers are installed."
  (interactive)
  (let ((servers '(("clangd" . "C/C++")
                   ("pylsp" . "Python") 
                   ("rust-analyzer" . "Rust")
                   ("typescript-language-server" . "TypeScript")
                   ("gopls" . "Go")))
        (missing '())
        (found '()))
    (dolist (server servers)
      (if (executable-find (car server))
          (push server found)
        (push server missing)))
    
    (message "=== LSP Server Status ===")
    (when found
      (message "Found: %s" 
               (mapconcat (lambda (s) (format "%s (%s)" (car s) (cdr s))) 
                         found ", ")))
    (when missing
      (message "Missing: %s"
               (mapconcat (lambda (s) (format "%s (%s)" (car s) (cdr s))) 
                         missing ", ")))
    (list :found found :missing missing)))

(defun config-check-packages ()
  "Check if required packages are installed."
  (interactive)
  (let ((required '(use-package vertico corfu marginalia orderless cape consult eglot))
        (missing '())
        (found '()))
    (dolist (pkg required)
      (if (package-installed-p pkg)
          (push pkg found)
        (push pkg missing)))
    
    (message "=== Package Status ===")
    (when found
      (message "Installed: %s" (mapconcat #'symbol-name found ", ")))
    (when missing
      (message "Missing: %s" (mapconcat #'symbol-name missing ", ")))
    (list :found found :missing missing)))

(defun config-startup-time ()
  "Show Emacs startup time."
  (interactive)
  (message "Emacs started in %.2f seconds with %d garbage collections."
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done))

(defun config-health-check ()
  "Run comprehensive configuration health check."
  (interactive)
  (message "=== Configuration Health Check ===")
  (config-startup-time)
  (config-check-packages)
  (config-check-lsp-servers)
  (message "=== Check Complete ==="))

(provide 'validation)
;;; validation.el ends here