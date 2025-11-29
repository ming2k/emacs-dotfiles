;;; erc-private.example.el --- Example ERC configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Copy this file to erc-private.el and fill in your actual IRC settings
;; The erc-private.el file should be added to .gitignore to keep your data private
;;
;; IMPORTANT: This file is loaded AFTER erc is loaded (from erc-setup.el),
;; so all erc functions and variables are available.

;;; Code:

;; ====================================================================
;; PERSONAL INFORMATION
;; ====================================================================
(setq erc-nick "YourNickname"              ; Your IRC nickname
      erc-user-full-name "Your Full Name") ; Your full name

;; ====================================================================
;; AUTO-JOIN CHANNELS
;; ====================================================================
;; Format: (("server" "#channel1" "#channel2"))
(setq erc-autojoin-channels-alist
      '(;; Libera.Chat (most popular for FOSS projects)
        ("Libera.Chat" "#linux")
        ))

;; ====================================================================
;; AUTHENTICATION
;; ====================================================================
;; Passwords are stored in ~/.authinfo or ~/.authinfo.gpg (recommended)
;; The auth-sources variable is already set in init.el
;;
;; Format for ~/.authinfo.gpg:
;; machine irc.libera.chat login YourNickname password YourPassword port 6697
;; machine irc.oftc.net login YourNickname password YourPassword port 6697
;;
;; To create encrypted authinfo:
;; 1. Create ~/.authinfo with the lines above
;; 2. Encrypt: gpg --encrypt --recipient your-email@example.com ~/.authinfo
;; 3. Move: mv ~/.authinfo.gpg ~/
;; 4. Remove plaintext: rm ~/.authinfo

(provide 'erc-private)
;;; erc-private.example.el ends here
