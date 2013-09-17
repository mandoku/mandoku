;;; mandoku-install.el
;; inspired by el-get
(setq mandoku-base-dir nil)
(let ((mandoku-root
       (file-name-as-directory
	(or (bound-and-true-p mandoku-base-dir)
	    (concat (file-name-as-directory user-emacs-directory) "mandoku")))))

  (setq mandoku-base-dir mandoku-root)

  (when (file-directory-p mandoku-root)
    (add-to-list 'load-path (concat mandoku-root "mandoku/lisp")))

  ;; try to require mandoku, failure means we have to install it
  (unless (require 'mandoku nil t)
    (unless (file-directory-p mandoku-root)
      (make-directory mandoku-root t))
    
    (let* ((package   "mandoku")
	   (buf       (switch-to-buffer "*mandoku bootstrap*"))
	   (pdir      (file-name-as-directory (concat mandoku-root package "/lisp")))
	   (git       (or (executable-find "git")
			  (error "Unable to find `git'")))
	   (url       (or (bound-and-true-p mandoku-git-install-url)
			  "http://github.com/cwittern/mandoku.git"))
	   (default-directory mandoku-root)
	   (process-connection-type nil)   ; pipe, no pty (--no-progress)

	   ;; First clone mandoku
	   (status
	    (call-process
	     git nil `(,buf t) t "--no-pager" "clone" "-v" url package)))

      (unless (zerop status)
	(error "Couldn't clone mandoku from the Git repository: %s" url))

      ;; switch branch if we have to
      (let* ((branch (cond
                      ;; Check if a specific branch is requested
                      ((bound-and-true-p mandoku-install-branch))
                      ;; Check if master branch is requested
                      ((boundp 'mandoku-master-branch) "master")
                      ;; As a last resort, use the master branch
                      ("master")))
             (remote-branch (format "origin/%s" branch))
	     (default-directory mandoku-root)
             (bstatus
               (if (string-equal branch "master")
                 0
                 (call-process git nil (list buf t) t "checkout" "-t" remote-branch))))
        (unless (zerop bstatus)
          (error "Couldn't `git checkout -t %s`" branch)))

      (add-to-list 'load-path pdir)
      (load package)

      (unless (file-directory-p mandoku-meta-dir)
	(make-directory mandoku-meta-dir t))
      (unless (file-directory-p mandoku-sys-dir)
	(make-directory mandoku-sys-dir t))
      (unless (file-directory-p mandoku-temp-dir)
	(make-directory mandoku-temp-dir t))

;;
      (let* ((pdir (file-name-as-directory mandoku-meta-dir))
	    (url       (or (bound-and-true-p mandoku-catalog-clone-url)
			  "http://github.com/cwittern/ZB"))
	    (default-directory pdir)
	    
	   ;; Now clone the catalogs
	   (status
	    (call-process
	     git nil `(,buf t) t "--no-pager" "clone" "-v" url)))
        (unless (zerop status)
	  (error "Couldn't clone mandoku catalogs from the Git repository: %s" url)))

      (load "mandoku-init")
      (mandoku-update-subcoll-list)
      (mandoku-update-title-lists)
      (mandoku-read-titletables)
      (mandoku-read-lookup-list)
      (with-current-buffer buf
	(goto-char (point-max))
	(insert "\nCongrats, mandoku is installed and ready to serve!")))))


;; (defun mandoku-post-install (package)
;;   "install the other necessary files, make the system files etc."
;;   (let*  (
;; 	   (buf       (switch-to-buffer "*mandoku bootstrap*"))
;; 	   (pdir      (file-name-as-directory (concat mandoku-root package)))
;; 	   (git       (or (executable-find "git")
;; 			  (error "Unable to find `git'")))
;; 	   (url       (or (bound-and-true-p mandoku-git-install-url)
;; 			  "http://github.com/cwittern/mandoku.git"))
;; 	   (default-directory mandoku-root)
;; 	   (process-connection-type nil)   ; pipe, no pty (--no-progress)

