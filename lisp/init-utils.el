;;; init-utils.el --- Utility functions -*- lexical-binding: t; -*-

;;; Code:

(defun copy-file-path ()
  "Copy the absolute path of the current file to the clipboard."
  (interactive)
  (if-let ((path (buffer-file-name)))
      (progn
        (kill-new path)
        (message "Copied: %s" path))
    (message "Buffer is not visiting a file")))

(global-set-key (kbd "C-c y p") #'copy-file-path)

(provide 'init-utils)
;;; init-utils.el ends here
