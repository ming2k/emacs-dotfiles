;;; config/tools/magit-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Comprehensive Git integration with Magit and related tools

;;; Code:

;; Core Magit package
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-dispatch)
         ("C-c g c" . magit-clone)
         ("C-c g s" . magit-status)
         ("C-c g b" . magit-blame-addition)
         ("C-c g l" . magit-log-buffer-file)
         ("C-c g p" . magit-pull)
         ("C-c g P" . magit-push))
  :custom
  ;; Display settings
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-bury-buffer-function #'magit-mode-quit-window)
  
  ;; Performance settings  
  (magit-refresh-status-buffer nil)
  (magit-refresh-verbose t)
  (magit-diff-refine-hunk t)
  
  ;; Git settings
  (magit-save-repository-buffers 'dontask)
  (magit-repository-directories '(("~/projects" . 2)
                                  ("~/.emacs.d" . 0)))
  
  ;; Log settings
  (magit-log-auto-more t)
  (magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  
  :config
  ;; Git commit message settings
  (setq git-commit-summary-max-length 50
        git-commit-fill-column 72)
  
)

;; Enhanced diffs and merge conflicts (optional - comment out if causing issues)
;; (use-package magit-delta
;;   :ensure t
;;   :after magit
;;   :hook (magit-mode . magit-delta-mode))

;; GitHub integration (optional - comment out if not needed)
;; (use-package forge
;;   :ensure t
;;   :after magit
;;   :custom
;;   (forge-database-file (expand-file-name "forge-database.sqlite" user-emacs-directory)))

;; Git gutter for showing changes in buffer
(use-package git-gutter
  :ensure t
  :hook ((prog-mode . git-gutter-mode)
         (text-mode . git-gutter-mode))
  :bind (("C-x v =" . git-gutter:popup-hunk)
         ("C-x v p" . git-gutter:previous-hunk)
         ("C-x v n" . git-gutter:next-hunk)
         ("C-x v r" . git-gutter:revert-hunk)
         ("C-x v s" . git-gutter:stage-hunk))
  :custom
  (git-gutter:update-interval 2)
  (git-gutter:modified-sign "~")
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")
  (git-gutter:hide-gutter t)
  :config
  ;; Add safety check for git-gutter updates
  (defun my-git-gutter-safe-update (orig-fun &rest args)
    "Safely update git-gutter to avoid array errors."
    (when (and (buffer-file-name)
               (vc-backend (buffer-file-name)))
      (condition-case nil
          (apply orig-fun args)
        (error nil))))

  (advice-add 'git-gutter:live-update :around #'my-git-gutter-safe-update))

;; Git time machine for browsing history
(use-package git-timemachine
  :ensure t
  :bind ("C-x v t" . git-timemachine)
  :custom
  (git-timemachine-abbreviation-length 6))

;; Git link for sharing URLs
(use-package git-link
  :ensure t
  :bind (("C-c g u" . git-link)
         ("C-c g U" . git-link-commit)
         ("C-c g h" . git-link-homepage))
  :custom
  (git-link-open-in-browser t))


;; Show TODO/FIXME items in magit status (optional - enable if desired)
;; (use-package magit-todos
;;   :ensure t
;;   :after magit
;;   :config
;;   (magit-todos-mode 1)
;;   :custom
;;   (magit-todos-keywords-list '("TODO" "FIXME" "HACK" "BUG" "NOTE")))

;; Better git configuration
(use-package git-modes
  :ensure t
  :mode (("\\.gitconfig\\'" . gitconfig-mode)
         ("\\.git/config\\'" . gitconfig-mode)
         ("\\.gitmodules\\'" . gitconfig-mode)
         ("\\.gitignore\\'" . gitignore-mode)))

(provide 'magit-setup)
;;; config/tools/magit-config.el ends here