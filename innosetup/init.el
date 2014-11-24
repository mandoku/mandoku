;; This is the emacs init file installed by the Mandoku Installer
;; You can add to this file or edit the file as you like.

;; The initialization code for mandoku is in the following file; if
;; you remove the following code, you will loose all mandoku functionality.
(with-temp-buffer
  (insert-file-contents "~/.emacs.d/mandoku.cfg")
;   (concat (file-name-directory (or load-file-name (buffer-file-name)))  "mandoku.cfg"))
  (goto-char (point-min))
  (re-search-forward "^basedir=\\(.*\\)" nil t)
  (setq mandoku-base-dir (concat (match-string 1) "\\"))
  (goto-char (point-min))
  (re-search-forward "^appdir=\\(.*\\)" nil t)
  (setq mandoku-app-dir (concat (match-string 1) "\\"))
  )
(add-to-list 'exec-path (expand-file-name (concat mandoku-app-dir "bin\\git\\bin")))
(add-to-list 'exec-path (expand-file-name (concat mandoku-app-dir "bin\\emacs-24.3\\bin")))
(add-to-list 'exec-path (expand-file-name (concat mandoku-app-dir "python\\App")))
(setq mandoku-git-program (expand-file-name (concat mandoku-app-dir "bin\\git\\bin\\git.exe")))
(setq mandoku-python-program (expand-file-name (concat mandoku-app-dir "python\\App\\python.exe")))

(load (expand-file-name "~/.emacs.d/md-init.el"))

(require 'mandoku-link)
(mandoku-initialize)
