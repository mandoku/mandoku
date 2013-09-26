;; default settings for mandoku
;; change filename to default.el and copy to site-lisp or any other directory Emacs loads by default
(if (eq window-system 'w32)
;; we try to get the driveletter and base directory, which should be the same as the emacs executable
    (progn
      (setq tmp (split-string (car load-path) "/"))
      (setq mandoku-base-dir (concat (car tmp) "/" (car (cdr tmp))"/" ))))
  

;; we look in some popular destinations for mandoku
(ignore-errors
  (setq mandoku-base-dir
	(car
	 (or (bound-and-true-p mandoku-base-dir)
	     (file-attributes (concat (file-name-as-directory "/Users/Shared/") "krp"))
	     (file-attributes (concat (file-name-as-directory "/Users/Shared/") "mandoku"))
	     (file-attributes (concat (expand-file-name "~/") "krp"))
	     (file-attributes (concat (expand-file-name "~/") "mandoku"))
	     (file-attributes (concat (expand-file-name "~/") "db"))))))
;; if we do not have it yet, we make one within the emacs dir
(let ((mandoku-root
       (file-name-as-directory
	(or (bound-and-true-p mandoku-base-dir)
	    (concat (file-name-as-directory user-emacs-directory) "mandoku")))))

  (setq mandoku-base-dir mandoku-root)
  (add-to-list 'load-path (concat mandoku-base-dir "mandoku/lisp"))
  (add-to-list 'load-path user-emacs-directory)
  (add-to-list 'load-path mandoku-base-dir)


    (unless (ignore-errors (require 'mandoku))
      ;; it seems this works not under windows...
      (with-temp-buffer (url-insert-file-contents 
       "https://raw.github.com/cwittern/mandoku/master/lisp/mandoku-install.el")
       (eval-region (point-min) (point-max))			
       )
    (mandoku-install mandoku-base-dir))
  ;; if this does not exist, install it to here!!
)

 
(or (ignore-errors (load "mandoku-local-init"))
    (load (concat (file-name-as-directory user-emacs-directory) "mandoku-local-init")))




(message "Default setup for Mandoku finished")

;; default.el ends here
