;; default settings for mandoku
;; change filename to default.el and copy to site-lisp or any other directory Emacs loads by default
;; default settings for mandoku
(require 'org)
(if (eq window-system 'w32)
;; we try to get the driveletter and base directory, which should be the same as the emacs executable
    (let (( tmp (split-string (car load-path) "/")))
      (setq mandoku-base-dir (concat (car tmp) "/" (car (cdr tmp))"/" ))
      (add-to-list 'exec-path (concat mandoku-base-dir "/bin/Git/bin")) 
      (add-to-list 'exec-path (concat mandoku-base-dir "/bin/gnuwin32/bin")) 
      (add-to-list 'exec-path (concat mandoku-base-dir "/bin/Git/bin/cmd")) )
  (ignore-errors
    ;; otherwise (mac or linux):
    ;; we look in some popular destinations for mandoku
    (let ((fn1 (concat (file-name-as-directory "/Users/Shared/") "krp"))
	  (fn2 (concat (file-name-as-directory "/Users/Shared/") "mandoku"))
	  (fn3 (concat (expand-file-name "~/") "krp"))
	  (fn4 (concat (expand-file-name "~/") "mandoku"))
	  (fn5 (concat (expand-file-name "~/") "db")))
      (setq mandoku-base-dir
	    (file-name-as-directory (cond 
				     ((file-exists-p fn1) fn1)
				     ((file-exists-p fn2) fn2)
				     ((file-exists-p fn3) fn3)
				     ((file-exists-p fn4) fn4)
				     ((file-exists-p fn5) fn5))))))
)

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
    (unless 
	(ignore-errors
	  (load (concat (file-name-as-directory user-emacs-directory) "mandoku-local-init")))
      (load "mandoku-install")
      (mandoku-setup-local-init-file)
      (load "mandoku-local-init")
      ))

(or (ignore-errors (org-babel-load-file (expand-file-name "settings.org" mandoku-meta-dir))))


(setq inhibit-splash-screen t)
(prefer-coding-system 'utf-8)

(message "Default setup for Mandoku finished")

;; default.el ends here
