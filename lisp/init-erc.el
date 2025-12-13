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

  ;; Enable modules
  (setq erc-modules '(autojoin networks services timestamp track match fill ring netsplit readonly spelling))
  (erc-update-modules)

  ;; Timestamp format
  (setq erc-timestamp-format "[%H:%M] "
        erc-timestamp-only-if-changed-flag nil
        erc-insert-timestamp-function 'erc-insert-timestamp-left)

  ;; Track channel activity
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                   "324" "329" "332" "333" "353" "477"))

  ;; Don't query before quitting IRC
  (setq erc-kill-queries-on-quit t
        erc-kill-server-buffer-on-quit t)

  ;; Use authinfo for passwords instead of prompting
  (setq erc-prompt-for-password nil)

  ;; Notifications
  (setq erc-current-nick-highlight-type 'nick)

  ;; Note: Authentication is handled via ~/.authinfo or ~/.authinfo.gpg
  ;; Format: machine irc.example.com login YourNickname password YourPassword
  ;; The auth-sources variable is already set in init.el

  ;; Key binding
  (global-set-key (kbd "C-c i") 'erc))

;; Optional: Load private ERC configuration if it exists
;; This file should contain your actual credentials and NOT be committed to git
(let ((private-config (expand-file-name "private/erc-private.el" user-emacs-directory)))
  (when (file-exists-p private-config)
    (load-file private-config)))

(provide 'init-erc)
;;; erc-setup.el ends here
