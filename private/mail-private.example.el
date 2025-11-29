;;; mail-private.example.el --- Example private mail configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Copy this file to mail-private.el and fill in your actual credentials
;; The mail-private.el file should be added to .gitignore to keep your data private
;;
;; IMPORTANT: This file is loaded AFTER mu4e is loaded (from mail-setup.el),
;; so all mu4e functions and variables are available. You can use
;; make-mu4e-context and other mu4e functions safely here.
;;
;; BEST PRACTICE: Organize mail in account-specific subdirectories:
;; ~/.local/share/mail/username/ or ~/.local/share/mail/work/
;; This keeps multiple accounts cleanly separated.

;;; Code:

;; Personal Information
(setq user-mail-address "your-email@example.com"
      user-full-name "Your Full Name")

;; SMTP Configuration (Sending Mail)
(setq smtpmail-smtp-server "smtp.example.com"  ; e.g., "smtp.gmail.com" for Gmail
      smtpmail-smtp-service 587                ; 587 for STARTTLS, 465 for SSL
      smtpmail-stream-type 'starttls           ; or 'ssl for port 465
      smtpmail-smtp-user "your-email@example.com") ; Must match login in .authinfo/.authinfo.gpg
                                                    ; Some servers need just username part (e.g., "username" instead of "username@domain.com")

;; Common SMTP configurations for popular providers:
;;
;; Gmail:
;;   smtpmail-smtp-server: smtp.gmail.com
;;   smtpmail-smtp-service: 587
;;   smtpmail-stream-type: starttls
;;   smtpmail-smtp-user: your-email@gmail.com
;;   Note: Use App Password if 2FA is enabled
;;   authinfo: machine smtp.gmail.com port 587 login your-email@gmail.com password YOUR_APP_PASSWORD
;;
;; Outlook/Office365:
;;   smtpmail-smtp-server: smtp.office365.com
;;   smtpmail-smtp-service: 587
;;   smtpmail-stream-type: starttls
;;   smtpmail-smtp-user: your-email@outlook.com
;;   authinfo: machine smtp.office365.com port 587 login your-email@outlook.com password YOUR_PASSWORD
;;
;; Yahoo:
;;   smtpmail-smtp-server: smtp.mail.yahoo.com
;;   smtpmail-smtp-service: 587
;;   smtpmail-stream-type: starttls
;;   smtpmail-smtp-user: your-email@yahoo.com
;;   authinfo: machine smtp.mail.yahoo.com port 587 login your-email@yahoo.com password YOUR_APP_PASSWORD
;;
;; ProtonMail Bridge:
;;   smtpmail-smtp-server: 127.0.0.1
;;   smtpmail-smtp-service: 1025
;;   smtpmail-stream-type: starttls
;;   smtpmail-smtp-user: your-email@protonmail.com
;;   authinfo: machine 127.0.0.1 port 1025 login your-email@protonmail.com password BRIDGE_PASSWORD
;;
;; Custom/Self-hosted:
;;   IMPORTANT: Some self-hosted mail servers require just the username part for SMTP login
;;   Example: If your email is me@hihusky.com, the server might need:
;;     smtpmail-smtp-user: me (not me@hihusky.com)
;;     authinfo: machine mail.hihusky.com port 587 login me password YOUR_PASSWORD
;;   Test with: openssl s_client -connect mail.server.com:587 -starttls smtp

;; Mail sync command (if different from default)
;; (setq mu4e-get-mail-command "mbsync -a")

;; ====================================================================
;; SINGLE ACCOUNT EXAMPLE (with subdirectory organization)
;; ====================================================================
;; If your mail structure is: ~/.local/share/mail/hihusky/
;; Uncomment and adjust the following:

;; (setq user-mail-address "hihusky@example.com"
;;       user-full-name "Hi Husky"
;;       smtpmail-smtp-server "smtp.example.com"
;;       smtpmail-smtp-service 587
;;       smtpmail-stream-type 'starttls)

;; Folder configuration - match your IMAP server folder names
;; IMPORTANT: mu4e defaults are: /sent, /drafts, /trash (lowercase)
;; Check your IMAP server's actual folder names with: a2 LIST "" "*"
;; Common variations:
;; - Gmail: "[Gmail]/Sent Mail", "[Gmail]/Drafts", "[Gmail]/Trash"
;; - Outlook/Exchange: "Sent Items", "Drafts", "Deleted Items"
;; - Others: "Sent", "Sent Items", "Sent Messages"
;;
;; Set default folders (adjust folder names to match your provider)
;; (setq mu4e-sent-folder "/hihusky/Sent Items"    ; or "/hihusky/Sent" or "/hihusky/sent"
;;       mu4e-drafts-folder "/hihusky/Drafts"      ; or "/hihusky/drafts"
;;       mu4e-trash-folder "/hihusky/Deleted Items" ; or "/hihusky/Trash" or "/hihusky/trash"
;;       mu4e-refile-folder "/hihusky/Archive")     ; or "/hihusky/INBOX"

;; Update folder shortcuts to include the account subdirectory
;; (setq mu4e-maildir-shortcuts
;;       '((:maildir "/hihusky/INBOX" :key ?i)
;;         (:maildir "/hihusky/Sent Items" :key ?s)  ; Match your server's folder name
;;         (:maildir "/hihusky/Drafts" :key ?d)
;;         (:maildir "/hihusky/Deleted Items" :key ?t)
;;         (:maildir "/hihusky/Junk Mail" :key ?j)))

;; Multiple account contexts (optional)
;; Uncomment and customize if you have multiple email accounts
;; (setq mu4e-contexts
;;       (list
;;        ;; Work account
;;        (make-mu4e-context
;;         :name "Work"
;;         :match-func
;;         (lambda (msg)
;;           (when msg
;;             (string-prefix-p "/work" (mu4e-message-field msg :maildir))))
;;         :vars '((user-mail-address . "work@company.com")
;;                 (user-full-name . "Your Full Name")
;;                 (smtpmail-smtp-server . "smtp.company.com")
;;                 (smtpmail-smtp-service . 587)
;;                 (mu4e-drafts-folder . "/work/Drafts")        ; Match your server
;;                 (mu4e-sent-folder . "/work/Sent Items")      ; Match your server
;;                 (mu4e-refile-folder . "/work/Archive")       ; or "/work/INBOX"
;;                 (mu4e-trash-folder . "/work/Deleted Items"))) ; Match your server
;;        ;; Personal account
;;        (make-mu4e-context
;;         :name "Personal"
;;         :match-func
;;         (lambda (msg)
;;           (when msg
;;             (string-prefix-p "/personal" (mu4e-message-field msg :maildir))))
;;         :vars '((user-mail-address . "personal@example.com")
;;                 (user-full-name . "Your Full Name")
;;                 (smtpmail-smtp-server . "smtp.example.com")
;;                 (smtpmail-smtp-service . 587)
;;                 (mu4e-drafts-folder . "/personal/Drafts")      ; Match your server
;;                 (mu4e-sent-folder . "/personal/Sent Items")    ; Match your server
;;                 (mu4e-refile-folder . "/personal/Archive")     ; or "/personal/INBOX"
;;                 (mu4e-trash-folder . "/personal/Deleted Items"))))) ; Match your server

;;; mail-private.example.el ends here
