;;; config/ui/help.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Help and documentation systems configuration
;;; Code:

;; Which-key - displays available keybindings in popup
(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :config
  ;; Basic which-key settings
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.05
        which-key-sort-order 'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-separator " → "
        which-key-prefix-prefix "+"
        which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL"))
  
  ;; Better popup appearance
  (setq which-key-show-prefix 'echo
        which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-allow-imprecise-window-fit nil
        which-key-max-description-length 32
        which-key-compute-remaps t
        which-key-use-C-h-commands t)
    
  ;; Performance optimization
  (setq which-key-lighter nil)
  
  :bind (("C-h K" . which-key-show-top-level)
         ("C-h M" . which-key-show-major-mode)))

(provide 'which-key-config)
;;; config/ui/help.el ends here
