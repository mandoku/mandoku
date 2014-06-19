;; default settings for mandoku
;; change filename to default.el and copy to site-lisp or any other directory Emacs loads by default
;; default settings for mandoku
(if (eq window-system 'w32)
;; we try to get the driveletter and base directory, which should be the same as the emacs executable
    (let (( tmp (split-string (car load-path) "/")))
      (setq mandoku-base-dir (concat (car tmp) "/" (car (cdr tmp))"/" ))
      (add-to-list 'exec-path (concat mandoku-base-dir "/bin/Git/bin")) 
      (add-to-list 'exec-path (concat mandoku-base-dir "/bin/gnuwin32/bin")) 
      (add-to-list 'exec-path (concat mandoku-base-dir "/bin/Git/bin/cmd")) 
      (setenv "HOME" (getenv "USERPROFILE")))
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
