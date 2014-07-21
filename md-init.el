;;; md-init.el --- Where all the magic begins
;;
;; Part of the Mandoku Starter Kit
;;
;; This is the first thing to get loaded.
;;

;; remember this directory
(defconst starter-kit-dir (file-name-directory (or load-file-name (buffer-file-name)))
    "directory where the starterkit is installed")

(defvar starter-kit-user-dir (expand-file-name "user" starter-kit-dir)
  "user directory for personal code")

(defvar mandoku-lisp (expand-file-name "lisp" starter-kit-dir)
  "directory for mandoku lisp code")

(add-to-list 'load-path starter-kit-dir)
(add-to-list 'load-path starter-kit-user-dir)
(add-to-list 'load-path mandoku-lisp)


;; proxy on windows
(if (eq window-system 'w32)
    (eval-after-load "url"
      '(progn
	 (require 'w32-registry)
	 (defadvice url-retrieve (before
				  w32-set-proxy-dynamically
				  activate)
	   "Before retrieving a URL, query the IE Proxy settings, and use them."
	   (let ((proxy (w32reg-get-ie-proxy-config)))
	     (setq url-using-proxy proxy
		   url-proxy-services proxy))))))


;; check for proxy
(require 'timer)
(defun mdinit-set-proxy ()
  "Try to connect, if not, set proxy"
  (interactive)
  (setq url-proxy-services nil)
  (with-timeout (5 
		 (setq url-proxy-services '(("no_proxy" . "localhost")
                           ("http" . "proxy.kuins.net:8080"))))
    
    (url-retrieve-synchronously "http://www.google.com"))
  (message "Set proxy services. %S " url-proxy-services)
  )
    

(mdinit-set-proxy)

;; install init on mac

(if (eq window-system 'mac)
    (with-current-buffer (find-file user-init-file)
      (goto-char (point-min))
      (if (not (search-forward "md-init" nil t))
	  (progn
	    (goto-char (point-max))
	    (insert "(load \"" starter-kit-dir "md-init.el\")")))))
	  

(require 'install-packages)
(require 'md-kit)
;;; end md-init

