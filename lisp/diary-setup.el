;;; diary-setup.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Diary configuration using daily org files
;;; Code:

(defun diary-todays-file ()
  "Return today's diary file path in format ~/diary/YYYYMMDD.org."
  (expand-file-name (format-time-string "%Y%m%d.org") "~/diary/"))

(use-package diary-lib
  :ensure nil
  :custom
  (diary-file (diary-todays-file))
  (calendar-view-diary-initially-flag t)
  (diary-number-of-entries 7)
  :config
  ;; Ensure diary directory exists
  (let ((diary-dir (expand-file-name "~/diary/")))
    (unless (file-directory-p diary-dir)
      (make-directory diary-dir t)))

  ;; Create today's diary file if it doesn't exist
  (defun diary-ensure-todays-file ()
    "Ensure today's diary file exists."
    (let ((today-file (diary-todays-file)))
      (unless (file-exists-p today-file)
        (with-temp-file today-file
          (insert (format-time-string "#+title: Diary %Y-%m-%d\n#+date: %Y-%m-%d\n\n"))))))

  ;; Create file when opening diary
  (advice-add 'diary :before (lambda (&rest _) (diary-ensure-todays-file))))

(provide 'diary-setup)
;;; diary-setup.el ends here
