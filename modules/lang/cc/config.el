;;; modules/lang/cc/config.el -*- lexical-binding: t; -*-

;; Enhanced C/C++ settings
(setq c-default-style "linux"
      c-basic-offset 4
      tab-width 4
      indent-tabs-mode nil)

;; Built-in C mode configuration
(use-package cc-mode
  :ensure nil
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.cxx\\'" . c++-mode)
         ("\\.cc\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.hxx\\'" . c++-mode)
         ("\\.hh\\'" . c++-mode))
  :hook ((c-mode . c-cpp-setup-corfu)
         (c-mode . c-cpp-setup-minor-modes)
         (c-mode . c-cpp-setup-lsp)
         (c-mode . c-cpp-setup-keybindings)
         (c++-mode . c-cpp-setup-corfu)
         (c++-mode . c-cpp-setup-minor-modes)
         (c++-mode . c-cpp-setup-lsp)
         (c++-mode . c-cpp-setup-keybindings))
  :config
  (setq c-default-style "linux"
        c-basic-offset 4
        tab-width 4
        indent-tabs-mode nil))

;; Enhanced C/C++ completion setup
(defun c-cpp-setup-corfu ()
  "Setup corfu completion for C/C++ with LSP priority."
  (setq-local corfu-auto-delay 0.0
              corfu-auto-prefix 1
              completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'cape-dabbrev
                     #'cape-file
                     #'cape-keyword))))

;; C/C++ minor modes setup
(defun c-cpp-setup-minor-modes ()
  "Enable helpful minor modes for C/C++."
  (electric-pair-local-mode 1)
  (electric-indent-local-mode 1)
  (subword-mode 1)
  (hs-minor-mode 1)
  (flyspell-prog-mode)
  (setq-local tab-width 4
              indent-tabs-mode nil))

;; Simple C/C++ LSP setup with basic header resolution
(defun c-cpp-setup-lsp ()
  "Setup LSP for C/C++ if available."
  (when (executable-find "clangd")
    (c-cpp-setup-include-paths)
    (eglot-ensure)))

;; C/C++ keybindings setup
(defun c-cpp-setup-keybindings ()
  "Setup C/C++ mode keybindings."
  (local-set-key (kbd "C-c C-c") 'c-cpp-compile)
  (local-set-key (kbd "C-c C-r") 'c-cpp-run)
  (local-set-key (kbd "C-c C-f") 'c-cpp-format-buffer)
  (local-set-key (kbd "C-c C-d") 'eglot-find-declaration)
  (local-set-key (kbd "C-c C-i") 'eglot-find-implementation)
  (local-set-key (kbd "C-c C-s") 'c-cpp-switch-header-source))

;; Simplified clangd configuration
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode) . ("clangd"
                                      "--background-index"
                                      "--header-insertion=iwyu"
                                      "--completion-style=detailed"))))

;; Simple include path setup
(defun c-cpp-setup-include-paths ()
  "Setup basic include paths for current project."
  (let* ((current-dir (if buffer-file-name
                          (file-name-directory buffer-file-name)
                        default-directory))
         (project-root (or (locate-dominating-file current-dir ".git")
                          (locate-dominating-file current-dir "Makefile")
                          (locate-dominating-file current-dir "CMakeLists.txt")
                          current-dir))
         (include-dir (expand-file-name "include" project-root))
         (src-dir (expand-file-name "src" project-root)))
    
    ;; Set flycheck include paths (if using)
    (when (boundp 'flycheck-gcc-include-path)
      (setq-local flycheck-gcc-include-path 
                  (list project-root include-dir src-dir)))
    (when (boundp 'flycheck-clang-include-path)
      (setq-local flycheck-clang-include-path 
                  (list project-root include-dir src-dir)))))

;; C/C++ utility functions
(defun c-cpp-compile ()
  "Compile current C/C++ file or project."
  (interactive)
  (save-buffer)
  (let* ((file (buffer-file-name))
         (makefile (locate-dominating-file default-directory "Makefile"))
         (cmake (locate-dominating-file default-directory "CMakeLists.txt"))
         (project-root (or makefile cmake default-directory)))
    (cond
     (makefile
      (let ((default-directory makefile))
        (compile "make")))
     (cmake
      (let ((default-directory cmake))
        (compile "cmake --build build 2>/dev/null || (mkdir -p build && cd build && cmake .. && make)")))
     ((string-match "\\.cpp\\|cc\\|cxx\\'" file)
      (let* ((include-flags (c-cpp-get-simple-include-flags project-root))
             (cmd (format "g++ -std=c++17 -Wall -g %s -o %s %s"
                         include-flags
                         (file-name-sans-extension file)
                         file)))
        (compile cmd)))
     (t
      (let* ((include-flags (c-cpp-get-simple-include-flags project-root))
             (cmd (format "gcc -std=c11 -Wall -g %s -o %s %s"
                         include-flags
                         (file-name-sans-extension file)
                         file)))
        (compile cmd))))))

(defun c-cpp-get-simple-include-flags (project-root)
  "Get simple include flags for compilation."
  (let ((dirs (list "." "include" "src")))
    (mapconcat (lambda (dir)
                 (let ((full-path (expand-file-name dir project-root)))
                   (when (file-directory-p full-path)
                     (format "-I%s" full-path))))
               dirs " ")))

(defun c-cpp-run ()
  "Run compiled C/C++ executable."
  (interactive)
  (let ((executable (file-name-sans-extension (buffer-file-name))))
    (if (file-executable-p executable)
        (compile (format "./%s" (file-name-nondirectory executable)))
      (message "No executable found. Compile first with C-c C-c"))))

(defun c-cpp-format-buffer ()
  "Format buffer with clang-format if available."
  (interactive)
  (cond
   ((executable-find "clang-format")
    (let ((original-point (point)))
      (shell-command-on-region
       (point-min) (point-max)
       "clang-format"
       (current-buffer) t)
      (goto-char original-point)))
   ((fboundp 'eglot-format-buffer)
    (eglot-format-buffer))
   (t (message "No C/C++ formatter found. Install clang-format"))))

(defun c-cpp-switch-header-source ()
  "Switch between header and source file using clangd."
  (interactive)
  (if (eglot-current-server)
      (eglot-execute-command (eglot-current-server) "clangd.switchSourceHeader" 
                             (vector (eglot--path-to-uri (buffer-file-name))))
    (message "clangd not running. Start LSP first.")))