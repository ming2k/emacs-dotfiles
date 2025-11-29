;;; erc-setup.el --- IRC client configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; ERC (Emacs IRC Client) configuration

;;; Code:

(use-package erc
  :ensure nil  ; ERC is built into Emacs
  :config
  ;; Personal information - REPLACE WITH YOUR SETTINGS
  ;; Better yet, set these in ~/.emacs.d/private/erc-private.el
  (setq erc-nick "YourNickname"
        erc-user-full-name "Your Full Name")

  ;; Channels to autojoin - CUSTOMIZE THIS
  ;; Format: (("server-name" "port") "#channel1" "#channel2")
  (setq erc-autojoin-channels-alist
        '(;; Example entries - replace with your channels
          ;; ("irc.libera.chat" "#emacs" "#org-mode")
          ;; ("irc.oftc.net" "#debian")
          ))

  ;; Hide join/part/quit messages for better readability
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))

  ;; Timestamp format
  (setq erc-timestamp-format "[%H:%M] "
        erc-timestamp-only-if-changed-flag nil
        erc-insert-timestamp-function 'erc-insert-timestamp-left)

  ;; Enable timestamp module
  (add-to-list 'erc-modules 'timestamp)

  ;; Enable spelling for ERC buffers
  (erc-spelling-mode 1)

  ;; Track channel activity
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                   "324" "329" "332" "333" "353" "477"))

  ;; Don't query before quitting IRC
  (setq erc-kill-queries-on-quit t
        erc-kill-server-buffer-on-quit t)

  ;; Prompt for channel password if needed
  (setq erc-prompt-for-password t)

  ;; Enable autojoin
  (erc-autojoin-mode 1)

  ;; Enable track mode - highlights channels with activity
  (erc-track-mode 1)

  ;; Enable match mode - highlights keywords
  (erc-match-mode 1)

  ;; Enable fill mode - wraps long lines
  (erc-fill-mode 1)

  ;; Ring mode - cycle through ERC buffers
  (erc-ring-mode 1)

  ;; Non-commands scroll buffer
  (erc-netsplit-mode 1)

  ;; Readonly mode for old messages
  (erc-readonly-mode 1)

  ;; Show images inline (requires emacs-w3m or similar)
  ;; (erc-image-mode 1)

  ;; Notifications
  (setq erc-current-nick-highlight-type 'nick)

  ;; Note: Authentication is handled via ~/.authinfo or ~/.authinfo.gpg
  ;; Format: machine irc.example.com login YourNickname password YourPassword
  ;; The auth-sources variable is already set in init.el

  ;; Custom connect function for quick access
  (defun my/erc-connect-libera ()
    "Connect to Libera.Chat IRC server."
    (interactive)
    (erc-tls :server "irc.libera.chat" :port 6697
             :nick erc-nick :full-name erc-user-full-name))

  (defun my/erc-connect-oftc ()
    "Connect to OFTC IRC server."
    (interactive)
    (erc-tls :server "irc.oftc.net" :port 6697
             :nick erc-nick :full-name erc-user-full-name))

  ;; Key bindings
  (global-set-key (kbd "C-c i") 'erc)
  (global-set-key (kbd "C-c C-i") 'my/erc-connect-libera))

;; Optional: Load private ERC configuration if it exists
;; This file should contain your actual credentials and NOT be committed to git
(let ((private-config (expand-file-name "private/erc-private.el" user-emacs-directory)))
  (when (file-exists-p private-config)
    (load-file private-config)))

(provide 'erc-setup)
;;; erc-setup.el ends here
