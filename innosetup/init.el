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
  )

(load (expand-file-name "~/.emacs.d/md-init.el"))

(require 'mandoku-link)
(mandoku-initialize)
