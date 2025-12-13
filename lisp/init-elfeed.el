;;; elfeed-setup.el --- RSS feed reader configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Elfeed configuration for reading RSS/Atom feeds

;;; Code:

(use-package elfeed
  :ensure t
  :config
  ;; Where to store the elfeed database
  (setq elfeed-db-directory
        (expand-file-name "elfeed" user-emacs-directory))

  ;; Show entry content in the same window
  (setq elfeed-show-entry-switch 'display-buffer)

  ;; Truncate long lines for better readability
  (setq elfeed-show-truncate-long-urls t)

  ;; Default search filter - show unread entries
  (setq elfeed-search-filter "@6-months-ago +unread")

  ;; Number of entries to display in search buffer
  (setq elfeed-search-trailing-width 30)

  ;; Customize the date format
  (setq elfeed-search-date-format '("%Y-%m-%d %H:%M" 16 :left))

  ;; RSS/Atom feed list - CUSTOMIZE THIS WITH YOUR FEEDS
  ;; Format: ("feed-url" tag1 tag2 ...)
  ;; Tags help organize and filter feeds
  (setq elfeed-feeds
        '(;; Example feeds - replace with your own
          ;; Tech News
          ("https://planet.emacslife.com/atom.xml" emacs planet)
          ("https://sachachua.com/blog/category/emacs-news/feed/" emacs news)

          ;; Add your feeds here in the format:
          ;; ("https://example.com/feed.xml" tag1 tag2)
          ))

  ;; Auto-cleanup old entries (older than 30 days)
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "30 days ago"
                                :remove 'unread))

  ;; Update feeds on Emacs startup
  (run-at-time nil (* 8 60 60) #'elfeed-update)

  ;; Key bindings
  (global-set-key (kbd "C-c e") 'elfeed)

  ;; Optional: Custom key bindings for elfeed search buffer
  (define-key elfeed-search-mode-map (kbd "U") 'elfeed-update)
  (define-key elfeed-search-mode-map (kbd "r") 'elfeed-search-untag-all-unread)
  (define-key elfeed-search-mode-map (kbd "u") 'elfeed-search-tag-all-unread))

;; Optional: Load private feed list if it exists
;; This allows you to keep personal feeds separate and untracked by git
(let ((private-feeds (expand-file-name "private/elfeed-private.el" user-emacs-directory)))
  (when (file-exists-p private-feeds)
    (load-file private-feeds)))

(provide 'init-elfeed)
;;; elfeed-setup.el ends here
