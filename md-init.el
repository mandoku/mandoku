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

(require 'install-packages)
(require 'md-kit)
;;; end init

