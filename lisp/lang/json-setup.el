;;; json-setup.el --- JSON mode configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; JSON mode configuration with flyspell disabled
;;; Code:

;; JSON mode configuration
(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)
         ("\\.jsonl\\'" . json-mode)
         ("\\.jsonc\\'" . json-mode))
  :hook (json-mode . json-setup-minor-modes)
  :config
  (setq json-reformat:indent-width 2
        json-reformat:pretty-string? nil))

;; JSON minor modes setup (without flyspell)
(defun json-setup-minor-modes ()
  "Enable helpful minor modes for JSON."
  (electric-indent-local-mode 1)
  (hs-minor-mode 1)
  ;; Explicitly disable flyspell for JSON
  (flyspell-mode -1)
  (setq-local tab-width 2
              indent-tabs-mode nil))

(provide 'json-setup)

;;; json-setup.el ends here