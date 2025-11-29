;;; elfeed-private.example.el --- Example Elfeed feed list -*- lexical-binding: t; -*-

;;; Commentary:
;; Copy this file to elfeed-private.el and add your actual feed URLs
;; The elfeed-private.el file should be added to .gitignore to keep your feeds private
;;
;; IMPORTANT: This file is loaded AFTER elfeed is loaded (from elfeed-setup.el),
;; so all elfeed functions and variables are available.
;;
;; Format: ("feed-url" tag1 tag2 tag3 ...)
;; - URL: RSS/Atom feed URL
;; - Tags: Optional tags for filtering and organization (use symbols without quotes)

;;; Code:

;; Override the default feed list with your personal feeds
(setq elfeed-feeds
      '(;; ====================================================================
        ;; EMACS COMMUNITY
        ;; ====================================================================
        ("https://planet.emacslife.com/atom.xml" emacs planet)
        ("https://sachachua.com/blog/category/emacs-news/feed/" emacs news)
        ("https://www.reddit.com/r/emacs/.rss" emacs reddit)

        ;; ====================================================================
        ;; TECH NEWS & INDUSTRY
        ;; ====================================================================
        ("https://techcrunch.com/feed/" tech news)
        ("https://www.theverge.com/rss/index.xml" tech news)
        ("https://arstechnica.com/feed/" tech news)

        ;; ====================================================================
        ;; HACKER NEWS & PROGRAMMING
        ;; ====================================================================
        ("https://hnrss.org/frontpage" programming hn)
        ("https://lobste.rs/rss" programming community)

        ;; ====================================================================
        ;; LINUX & OPEN SOURCE
        ;; ====================================================================
        ("https://lwn.net/headlines/rss" linux news)
        ("https://www.phoronix.com/rss.php" linux hardware)

        ;; ====================================================================
        ;; NEWS & WORLD AFFAIRS
        ;; ====================================================================
        ("https://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml" news world)
        ("https://feeds.bbci.co.uk/news/world/rss.xml" news world bbc)

        ;; ====================================================================
        ;; SCIENCE & RESEARCH
        ;; ====================================================================
        ("https://feeds.nature.com/nature/rss/current" science research)
        ("https://www.science.org/rss/news_current.xml" science research)

        ;; ====================================================================
        ;; SECURITY & PRIVACY
        ;; ====================================================================
        ("https://krebsonsecurity.com/feed/" security)
        ("https://www.schneier.com/blog/atom.xml" security privacy)

        ;; ====================================================================
        ;; REDDIT FEEDS
        ;; ====================================================================
        ("https://www.reddit.com/r/programming/.rss" reddit programming)
        ("https://www.reddit.com/r/technology/.rss" reddit tech)

        ;; ====================================================================
        ;; ADD YOUR OWN FEEDS BELOW
        ;; ====================================================================
        ;; ("https://example.com/feed.xml" tag1 tag2)
        ))

;; ====================================================================
;; ADVANCED CONFIGURATION (Optional)
;; ====================================================================

;; Custom search filters for quick access
;; Use with: M-x elfeed, then press 's' and type filter name
;; (setq elfeed-search-filter "@1-week-ago +unread")  ; Default filter

;; Auto-tagging based on feed content (examples)
;; Tag YouTube feeds automatically
;; (add-hook 'elfeed-new-entry-hook
;;           (elfeed-make-tagger :feed-url "youtube\\.com"
;;                               :add '(video youtube)))

;; Tag entries with "important" keyword in title
;; (add-hook 'elfeed-new-entry-hook
;;           (elfeed-make-tagger :entry-title "important\\|breaking\\|critical"
;;                               :add 'important))

;; Remove unread tag from old entries (default is 30 days)
;; Adjust retention period here
;; (add-hook 'elfeed-new-entry-hook
;;           (elfeed-make-tagger :before "15 days ago"  ; Change from default 30
;;                               :remove 'unread))

(provide 'elfeed-private)
;;; elfeed-private.example.el ends here
