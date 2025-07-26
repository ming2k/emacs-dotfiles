;; wl-clipboard
(defun wl-copy (text)
  "Copy TEXT to wl-clipboard"
  (let ((process (make-process :name "wl-copy"
                               :buffer nil
                               :command '("wl-copy")
                               :connection-type 'pipe)))
    (process-send-string process text)
    (process-send-eof process)))

(defun wl-paste ()
  "Paste from wl-clipboard"
  (with-temp-buffer
    (call-process "wl-paste" nil t nil "--no-newline")
    (buffer-string)))

(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)

(setq select-enable-clipboard t)
(setq select-enable-primary t)

(provide 'linux)
