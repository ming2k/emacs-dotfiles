;;; init-utils.el --- Utility functions -*- lexical-binding: t; -*-

;;; Code:

(defconst my/disabled-builtin-commands
  '(5x5
    animate-birthday-present
    blackbox
    bubbles
    decipher
    doctor
    dunnet
    gomoku
    hanoi
    life
    mpuz
    pong
    snake
    solitaire
    tetris
    todo-show
    zone
    yow
    spook)
  "Built-in commands intentionally disabled in this config.")

(dolist (command my/disabled-builtin-commands)
  (put command 'disabled t))

(mapatoms
 (lambda (sym)
   (when (and (commandp sym)
              (get sym 'byte-obsolete-info))
     (put sym 'disabled t))))

(setq read-extended-command-predicate
      (lambda (command buffer)
        (and (command-completion-default-include-p command buffer)
             (not (get command 'disabled)))))

(defun copy-file-path ()
  "Copy the absolute path of the current file to the clipboard."
  (interactive)
  (if-let ((path (buffer-file-name)))
      (progn
        (kill-new path)
        (message "Copied: %s" path))
    (message "Buffer is not visiting a file")))

(global-set-key (kbd "C-c y p") #'copy-file-path)

(provide 'init-utils)
;;; init-utils.el ends here
