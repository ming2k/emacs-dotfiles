;;; mail-setup.el --- Email configuration with mu4e -*- lexical-binding: t; -*-

;;; Commentary:
;; Email client configuration using mu4e with placeholders for sensitive data

;;; Code:

;; mu4e - Mail User Agent for Emacs (email client using mu/mbsync)
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
  ;; To configure, copy this section to ~/.emacs.d/private/mail-private.el
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

        ;; Don't keep message buffers around
        message-kill-buffer-on-exit t)

  ;; Note: auth-sources is set globally in init.el and used by mu4e, sieve-manage, etc.

  ;; Receiving mail configuration
  ;; mu4e doesn't fetch mail itself - use mbsync/isync, offlineimap, or fetchmail
  ;; Example: Using mbsync (recommended)
  (setq mu4e-get-mail-command "mbsync -a"  ; Replace with your mail sync command
        mu4e-update-interval nil  ; Disable automatic updates (manual only)
        mu4e-change-filenames-when-moving t)  ; Avoid sync conflicts

  ;; Headers list sorting
  (setq mu4e-headers-sort-field :date)
  (setq mu4e-headers-sort-direction 'descending)

  ;; Headers list format - 24h time format and compact layout
  (setq mu4e-headers-date-format "%m-%d %H:%M"
        mu4e-headers-fields '((:date . 12)
                              (:flags . 6)
                              (:from . 18)
                              (:subject . 44)))

  ;; Message view fields
  (setq mu4e-view-fields
        '(:subject
          :from
          :to
          :cc
          :date
          :attachments))

  ;; IMPORTANT: mu4e defaults are /sent, /drafts, /trash (lowercase)

  ;; Bookmarks for common searches
  (setq mu4e-bookmarks
        '((:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key ?u)
          (:name "Today's messages" :query "date:today..now" :key ?t)
          (:name "Last 7 days" :query "date:7d..now" :key ?w)
          (:name "Messages with attachments" :query "flag:attach" :key ?a)))

  ;; Configuration for GPG password caching before mail update
  ;; Set in private/mu4e-private.el to customize behavior
  (defvar mu4e-unlock-gpg-file nil
    "When non-nil, decrypt this GPG file before updating mail to cache gpg-agent password.
This prompts for GPG password in minibuffer, caching it in gpg-agent so external
commands like mbsync's 'pass' can use the cached password without prompting.
In theory, any GPG-encrypted file using the same key can be used here.
Example: \"~/.password-store/email/mail.example.com/user.gpg\"")

  ;; Cache GPG password in gpg-agent before updating mail
  (advice-add 'mu4e-update-mail-and-index :before
              (lambda (&rest _args)
                (when mu4e-unlock-gpg-file
                  (condition-case err
                      ;; Visit the GPG file to trigger auto-decryption via epa-file
                      ;; This uses loopback pinentry (minibuffer prompt) and caches password
                      ;; Then immediately kill the buffer without saving
                      (let ((buf (find-file-noselect (expand-file-name mu4e-unlock-gpg-file))))
                        (kill-buffer buf))
                    (error
                     (message "Failed to unlock GPG file: %s" (error-message-string err))
                     (signal (car err) (cdr err)))))))

  ;; Load mu4e private configuration if it exists
  ;; This file should contain your actual credentials and NOT be committed to git
  ;; IMPORTANT: Load this INSIDE :config to ensure mu4e is loaded first
  (let ((private-config (expand-file-name "private/mu4e-private.el" user-emacs-directory)))
    (when (file-exists-p private-config)
      (load-file private-config)))

  ;; Key bindings
  (global-set-key (kbd "C-c m") 'mu4e))

(provide 'init-mail)
;;; mail-setup.el ends here
