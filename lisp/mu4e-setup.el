;;; mu4e-setup.el --- Email configuration with mu4e -*- lexical-binding: t; -*-

;;; Commentary:
;; Email client configuration using mu4e with placeholders for sensitive data

;;; Code:

(use-package mu4e
  :ensure nil  ; mu4e comes with mu package, not from ELPA
  :if (executable-find "mu")
  :config

  ;; Basic mu4e configuration - Follow XDG Base Directory specification
  ;; Note: This sets the root mail directory. Individual accounts should be
  ;; organized as subdirectories: ~/.local/share/mail/account-name/
  (setq mu4e-maildir (expand-file-name "mail" (or (getenv "XDG_DATA_HOME")
                                                    "~/.local/share")))

  ;; SMTP configuration - REPLACE WITH YOUR SETTINGS
  ;; To configure, copy this section to ~/.emacs.d/private/mu4e-private.el
  ;; and fill in your actual values
  (setq user-mail-address "YOUR-EMAIL@example.com"
        user-full-name "YOUR FULL NAME"

        ;; SMTP settings
        smtpmail-smtp-server "smtp.example.com"  ; Your SMTP server
        smtpmail-smtp-service 587  ; or 465 for SSL
        smtpmail-stream-type 'starttls  ; or 'ssl
        smtpmail-smtp-user "YOUR-EMAIL@example.com"  ; Your email username

        ;; Message sending
        message-send-mail-function 'smtpmail-send-it
        send-mail-function 'smtpmail-send-it

        ;; Authentication - supports both .authinfo and .authinfo.gpg
        auth-sources '("~/.authinfo.gpg" "~/.authinfo")

        ;; Don't keep message buffers around
        message-kill-buffer-on-exit t)

  ;; Receiving mail configuration
  ;; mu4e doesn't fetch mail itself - use mbsync/isync, offlineimap, or fetchmail
  ;; Example: Using mbsync (recommended)
  (setq mu4e-get-mail-command "mbsync -a"  ; Replace with your mail sync command
        mu4e-update-interval 300  ; Update every 5 minutes (in seconds)
        mu4e-change-filenames-when-moving t)  ; Avoid sync conflicts

  ;; Display and behavior
  (setq mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-compose-format-flowed t
        mu4e-date-format "%Y-%m-%d"
        mu4e-headers-date-format "%Y-%m-%d"
        mu4e-headers-time-format "%H:%M"
        mu4e-headers-results-limit 1000
        mu4e-headers-skip-duplicates t
        mu4e-attachment-dir (expand-file-name "downloads" "~"))  ; or use XDG_DOWNLOAD_DIR

  ;; Shortcuts for common folders
  ;; NOTE: Update these paths to match your account structure
  ;; Example: If your mail is in ~/.local/share/mail/username/
  ;; then use "/username/INBOX" instead of "/INBOX"
  ;; IMPORTANT: mu4e defaults are /sent, /drafts, /trash (lowercase)
  ;; Check your IMAP server's folder names with: a2 LIST "" "*"
  ;; Override these in private/mu4e-private.el to match your server
  (setq mu4e-maildir-shortcuts
        '((:maildir "/INBOX" :key ?i)
          (:maildir "/sent" :key ?s)
          (:maildir "/drafts" :key ?d)
          (:maildir "/trash" :key ?t)))

  ;; Bookmarks for common searches
  (setq mu4e-bookmarks
        '((:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key ?u)
          (:name "Today's messages" :query "date:today..now" :key ?t)
          (:name "Last 7 days" :query "date:7d..now" :key ?w)
          (:name "Messages with attachments" :query "flag:attach" :key ?a)))

  ;; Load private configuration if it exists
  ;; This file should contain your actual credentials and NOT be committed to git
  ;; IMPORTANT: Load this INSIDE :config to ensure mu4e is loaded first
  (let ((private-config (expand-file-name "private/mu4e-private.el" user-emacs-directory)))
    (when (file-exists-p private-config)
      (load-file private-config)))

  ;; Key bindings
  (global-set-key (kbd "C-c m") 'mu4e))

(provide 'mu4e-setup)
;;; mu4e-setup.el ends here
