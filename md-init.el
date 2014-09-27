;;; md-init.el --- Where all the magic begins
;;
;; Part of the Mandoku Starter Kit
;;
;; This is the first thing to get loaded.
;;
(setq debug-on-error t)
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

(setq ssid (if (eq window-system 'w32)
		      (shell-command-to-string "netsh wlan show interface | find /i \" SSID\"")
	     (shell-command-to-string "/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -I | awk '/ SSID/ {print substr($0, index($0, $2))}'")))

(ignore-errors 
  (if (string-match "WaveLAN" ssid)
      (setq url-proxy-services '(("no_proxy" . "localhost")
			   ("http" . "proxy.kuins.net:8080")))
      (setq url-proxy-services nil))
)


;; install init on mac

(ignore-errors (if (eq window-system 'mac)
		   (let ((init-file (if user-init-file
					user-init-file
				      (expand-file-name "~/.emacs.d/init.el"))))
    (with-current-buffer (find-file-noselect init-file)
      (goto-char (point-min))
      (if (not (search-forward "md-init" nil t))
	  (progn
	    (goto-char (point-max))
	    (insert "(load \"" starter-kit-dir "md-init.el\")")))
      (save-buffer)
      (kill-buffer))
)))
	  

(require 'install-packages)
(require 'md-kit)
;;; end md-init

