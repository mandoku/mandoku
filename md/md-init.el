;;; md-init.el --- Where all the magic begins
;;
;; Part of the Mandoku Starter Kit
;;
;; This is the first thing to get loaded.
;;
;(setq debug-on-error t)
;; hopefully this will solve the problem with *temp* buffer
(prefer-coding-system 'utf-8)
;; remember the directory below this
(defconst starter-kit-dir (file-name-directory (substring (file-name-directory (or load-file-name (buffer-file-name))) 0 -1))
    "directory where the starterkit is installed")

(defvar starter-kit-user-dir (expand-file-name "user" starter-kit-dir)
  "user directory for personal code")
(unless (file-exists-p starter-kit-user-dir)
  (make-directory starter-kit-user-dir))

(defvar starter-kit-lisp (expand-file-name "md" starter-kit-dir)
  "directory for starter-kit lisp code")


(add-to-list 'load-path starter-kit-lisp)
(add-to-list 'load-path starter-kit-user-dir)

;; try to set proxy
(setq ssid (if (eq window-system 'w32)
		      (shell-command-to-string "netsh wlan show interface | find /i \" SSID\"")
	     (shell-command-to-string "/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -I | awk '/ SSID/ {print substr($0, index($0, $2))}'")))

(ignore-errors 
  (if (string-match "WaveLAN" ssid)
      (setq url-proxy-services '(("no_proxy" . "localhost")
			   ("http" . "proxy.kuins.net:8080")))
      (setq url-proxy-services nil))
)


(require 'install-packages)
(require 'md-kit)
;;; end md-init

