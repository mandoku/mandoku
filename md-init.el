;;; md-init.el --- Where all the magic begins
;;
;; Part of the Mandoku Starter Kit
;;
;; This is the first thing to get loaded.
;;
;(setq debug-on-error t)
(prefer-coding-system 'utf-8)
;; remember this directory
(defconst starter-kit-dir (file-name-directory (or load-file-name (buffer-file-name)))
    "directory where the starterkit is installed")

(defvar starter-kit-user-dir (expand-file-name "user" starter-kit-dir)
  "user directory for personal code")

(defvar mandoku-lisp (expand-file-name "lisp" starter-kit-dir)
  "directory for mandoku lisp code")

(setq mandoku-base-dir (concat (mapconcat 'identity (butlast (split-string starter-kit-dir "/") 2) "/") "/"))

(add-to-list 'load-path starter-kit-dir)
(add-to-list 'load-path starter-kit-user-dir)
(add-to-list 'load-path mandoku-lisp)

;; set proxy on windows
(ignore-errors (if (eq window-system 'w32)

(if (string-match "WaveLAN" (shell-command-to-string "netsh wlan show interface | find /i \" SSID\""))
    (setq url-proxy-services '(("no_proxy" . "localhost")
			   ("http" . "proxy.kuins.net:8080"))))
))

;; install init on mac

(ignore-errors (if (eq window-system 'mac)
    (with-current-buffer (find-file-noselect user-init-file)
      (goto-char (point-min))
      (if (not (search-forward "md-init" nil t))
	  (progn
	    (goto-char (point-max))
	    (insert "(load \"" starter-kit-dir "md-init.el\")")))
      (save-buffer)
      (kill-buffer))
    
))
	  

(require 'install-packages)
(require 'md-kit)
;;; end md-init

