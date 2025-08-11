;;; modules/lang/markdown/config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Comprehensive Markdown language support with live preview and editing features
;;; Code:

;; Markdown mode settings
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :config
  ;; Markdown basic settings
  (setq markdown-command "pandoc"
        markdown-command-needs-filename t
        markdown-content-type "application/xhtml+xml"
        markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css")
        markdown-xhtml-header-content "
<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>
<style>
body { box-sizing: border-box; max-width: 740px; width: 100%; margin: 40px auto; padding: 0 10px; }
.markdown-body { font-size: 16px; line-height: 1.6; }
</style>")
  
  ;; Enhanced markdown settings
  (setq markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-gfm-additional-languages '("sh" "bash" "python" "javascript" "elisp")
        markdown-fontify-code-blocks-natively t
        markdown-hide-urls nil
        markdown-hide-markup nil
        markdown-indent-on-enter 'indent-and-new-item
        markdown-nested-imenu-heading-index t
        markdown-split-window-direction 'right)
  
  ;; Auto-fill in markdown
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'markdown-mode-hook 'markdown-setup-completion)
  (add-hook 'markdown-mode-hook 'markdown-setup-keybindings))

;; GitHub Flavored Markdown mode
(use-package markdown-mode
  :ensure nil
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.gfm\\'" . gfm-mode))
  :config
  (setq markdown-command "grip --export -"))

;; Markdown completion setup
(defun markdown-setup-completion ()
  "Setup completion for markdown mode."
  (setq-local completion-at-point-functions
              (list #'dabbrev-capf
                    #'comint-filename-completion)))

;; Markdown keybindings
(defun markdown-setup-keybindings ()
  "Setup markdown mode keybindings."
  (local-set-key (kbd "C-c C-c") 'markdown-preview)
  (local-set-key (kbd "C-c C-p") 'markdown-live-preview-mode)
  (local-set-key (kbd "C-c C-e") 'markdown-export)
  (local-set-key (kbd "C-c C-v") 'markdown-preview-file)
  (local-set-key (kbd "C-c C-w") 'markdown-kill-thing-at-point)
  (local-set-key (kbd "C-c C-n") 'markdown-outline-next)
  (local-set-key (kbd "C-c C-u") 'markdown-outline-previous)
  (local-set-key (kbd "C-c C-f") 'markdown-forward-same-level)
  (local-set-key (kbd "C-c C-b") 'markdown-backward-same-level)
  (local-set-key (kbd "C-c C-t") 'markdown-toc-generate-or-refresh-toc)
  (local-set-key (kbd "C-c C-i") 'markdown-insert-link)
  (local-set-key (kbd "C-c C-l") 'markdown-insert-reference-link))

;; Enhanced markdown preview
(defun markdown-preview ()
  "Preview markdown file in browser."
  (interactive)
  (save-buffer)
  (let ((output-file (concat (file-name-sans-extension (buffer-file-name)) ".html")))
    (shell-command (format "pandoc -f markdown -t html -s -o %s %s" 
                          output-file (buffer-file-name)))
    (browse-url (concat "file://" output-file))))

;; Markdown export function
(defun markdown-export ()
  "Export markdown to HTML."
  (interactive)
  (save-buffer)
  (let ((output-file (concat (file-name-sans-extension (buffer-file-name)) ".html")))
    (if (executable-find "pandoc")
        (progn
          (shell-command (format "pandoc -f markdown -t html -s --highlight-style=github -c github-markdown.css -o %s %s" 
                                output-file (buffer-file-name)))
          (message "Exported to %s" output-file))
      (markdown-export-and-preview))))

;; Table of contents generation
(defun markdown-toc-generate-or-refresh-toc ()
  "Generate or refresh table of contents."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<!-- TOC -->" nil t)
      (let ((toc-start (point))
            (toc-end (if (re-search-forward "^<!-- /TOC -->" nil t)
                        (match-beginning 0)
                      (point))))
        (delete-region toc-start toc-end)
        (markdown-insert-toc)))))

;; Enhanced markdown utilities
(defun markdown-insert-toc ()
  "Insert table of contents at point."
  (interactive)
  (let ((toc-list '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\(#+\\)\\s-+\\(.+\\)$" nil t)
        (let* ((level (length (match-string 1)))
               (title (match-string 2))
               (anchor (downcase (replace-regexp-in-string "[^a-z0-9]+" "-" title))))
          (push (list level title anchor) toc-list))))
    (setq toc-list (nreverse toc-list))
    (dolist (item toc-list)
      (let ((level (car item))
            (title (cadr item))
            (anchor (caddr item)))
        (insert (make-string (* 2 (1- level)) ?\s))
        (insert (format "- [%s](#%s)\n" title anchor))))))

;; Markdown word count
(defun markdown-word-count ()
  "Count words in markdown buffer, excluding markup."
  (interactive)
  (let ((word-count 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (skip-chars-forward "^a-zA-Z0-9")
        (when (looking-at "\\w")
          (forward-word)
          (setq word-count (1+ word-count)))
        (skip-chars-forward "a-zA-Z0-9")))
    (message "Word count: %d" word-count)))

;; Markdown link utilities
(defun markdown-insert-link-dwim ()
  "Insert link with DWIM behavior."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring (region-beginning) (region-end))))
        (delete-region (region-beginning) (region-end))
        (insert (format "[%s](%s)" text (read-string "URL: "))))
    (let ((url (read-string "URL: "))
          (text (read-string "Link text: ")))
      (insert (format "[%s](%s)" text url)))))

;; Markdown table utilities
(defun markdown-table-align ()
  "Align markdown table at point."
  (interactive)
  (when (markdown-table-at-point-p)
    (let ((begin (markdown-table-begin))
          (end (markdown-table-end)))
      (save-excursion
        (goto-char begin)
        (while (< (point) end)
          (markdown-table-align-row)
          (forward-line))))))

;; Live preview mode enhancement
(use-package markdown-preview-mode
  :ensure t
  :after markdown-mode
  :config
  (setq markdown-preview-javascript
        (list "https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"
              "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"))
  (setq markdown-preview-stylesheets
        (list "https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
              "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")))

;; Markdown TOC package for automatic TOC generation
(use-package markdown-toc
  :ensure t
  :after markdown-mode
  :config
  (setq markdown-toc-header-toc-start "<!-- TOC -->"
        markdown-toc-header-toc-end "<!-- /TOC -->"
        markdown-toc-header-toc-title "Table of Contents"
        markdown-toc-indentation-space 2))

;; Additional markdown keybindings
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-s") 'markdown-table-sort-lines)
  (define-key markdown-mode-map (kbd "C-c C-d") 'markdown-word-count)
  (define-key markdown-mode-map (kbd "C-c C-a") 'markdown-table-align)
  (define-key markdown-mode-map (kbd "C-c C-r") 'markdown-insert-link-dwim)
  (define-key markdown-mode-map (kbd "C-c C-o") 'markdown-follow-thing-at-point)
  (define-key markdown-mode-map (kbd "C-c C-x") 'markdown-toggle-markup-hiding))

;; Font-lock enhancements for better syntax highlighting
(with-eval-after-load 'markdown-mode
  (add-to-list 'markdown-code-lang-modes '("elisp" . emacs-lisp-mode))
  (add-to-list 'markdown-code-lang-modes '("bash" . shell-script-mode))
  (add-to-list 'markdown-code-lang-modes '("py" . python-mode))
  (add-to-list 'markdown-code-lang-modes '("js" . javascript-mode)))

(provide 'markdown-config)
;;; modules/lang/markdown/config.el ends here