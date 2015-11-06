;; mandoku-github.el   -*- coding: utf-8 -*-
;; created [2015-11-07T11:41:45+0900] chris
;; mandoku library for handling communication with github
;; relies on gh.el and github-clone.el

(require 'github-clone)


(defun mandoku-get-remote-text (&optional txtid)
  "This checks if a text is available in a repo and then clones
it into the appropriate place If a txtid is given, it will use
that txtid, otherwise it tries to derive one from the current
filename.
We should check if the file exists before cloning!"
  (interactive)
  (let* ((buf (current-buffer))
	 (fn (file-name-sans-extension (file-name-nondirectory (buffer-file-name ))))
	 (ext (file-name-extension (file-name-nondirectory (buffer-file-name ))))
;	 (txtid (downcase (car (split-string  fn "_" ))))
	 (tmpid (car (split-string  fn "_" )))
	 (mandoku-gh-user "kanripo")
	 (mandoku-gh-server "github.com")
	 (txtid (if txtid
		    txtid
	   (if (string-match "[a-z]"  tmpid (- (length tmpid) 1))
		     (substring tmpid 0 (- (length tmpid) 1))
		   tmpid)))
	 (repid (car (split-string txtid "\\([0-9]\\)")))
	 (groupid (substring txtid 0 (+ (length repid) 2)))
	 (clone-url (if mandoku-git-use-http
	 		(concat "https://" mandoku-gh-server "/")
	 	      (concat "git@" mandoku-gh-server ":")))
	 (txturl (concat clone-url (mandoku-get-user) "/" txtid ".git"))
	 (targetdir (concat mandoku-text-dir groupid "/")))
    (mkdir targetdir t)
;    (github-clone (concat mandoku-gh-rep "/" txtid) targetdir)
    (mandoku-clone (concat targetdir txtid)  txturl)
;    (kill-buffer buf)
;    (find-file (concat targetdir txtid "/" fn "." ext)))
))

 ; (shell-command-to-string (concat "cd " default-directory "  && " git " clone " txturl ))) 

;; this will be implemented once the gitlab API change is in place
(defun mandoku-gitlab-create-project ()
  "Create a gitlab project for the current text"
  (interactive)
  (let*  ((tit (mandoku-get-title))
	  (txtid (mandoku-get-textid))
	  (branch (mandoku-get-current-branch))
	  (url "False") rem cont)
    (if (equal branch "master")
	(setq cont (y-or-n-p "You are still on the master branch. It is recommended to create a different branch first. Do you want to continue?"))
      (setq cont t)
      )
    (if (and cont (y-or-n-p "This will create a project on gitlab and push a copy there. Do you want to continue?"))
	  (setq url (substring (shell-command-to-string (concat mandoku-python-program " " mandoku-sys-dir "python/makerep.py " txtid " " tit )) 0 -1)))
    (if (equal (substring url 0 4) "git@")
	(progn
	  ; we create a remote for this repository
	  (shell-command-to-string (concat mandoku-git-program " remote add " mandoku-gitlab-remote-name " " url))
	  ; and now push to the repository .. maybe do this asyncroneosly..
	  (shell-command-to-string (concat mandoku-git-program " push -u " mandoku-gitlab-remote-name " " branch))
	  )
      (if (and cont (not (equal url "False")))
	  ; the remote dir already exists, still need to add it as remote here:
	  (progn
	    (shell-command-to-string
	     (concat mandoku-git-program " remote add " mandoku-gitlab-remote-name " "
		     (cadr (split-string url " "))))
					; now we add the remote default branch to this .git/config
	    (shell-command-to-string
	     (concat mandoku-git-program " fetch " mandoku-gitlab-remote-name  ))
	    (shell-command-to-string
	     (concat mandoku-git-program " checkout " (car (split-string url " ")) ))
	    
	    )
      ))
    
;  (message "%s" url)
  ))

(defun mandoku-fork (txtid)
  "Fork a repository to the current user. At the moment, this requires curl"
  (let ((res
  (shell-command-to-string
   (concat "curl -s -u "
	   mandoku-user-token
	   ":x-oauth-basic -X POST https://api."
	   ;; unlikely this will work for a different server, but hey..
	   mandoku-gh-server
	   "/repos/"
	   ;; this is the gh user of the org, usually kanripo!
	   mandoku-gh-user
	   "/"
	   txtid
	   "/forks"))))
    (if (string-match (concat (mandoku-get-user) "/" txtid) res)
	(message (concat txtid " has been cloned successfully"))
    )
  )
  )

(defun mandoku-url-to-txtid (url)
;  (if mandoku-git-use-http
      (substring (car (last (split-string url "/"))) 0 -4)
;  (substring (cadr (split-string url "/")) 0 -4))
  )



(defun mandoku-clone (targetdir url)
  (let* ((default-directory targetdir)
	 (process-connection-type nil)   ; pipe, no pty (--no-progress)
	 (curbuf    (current-buffer))
	 (buf       (get-buffer-create "*mandoku bootstrap*"))
	 (txtid     (mandoku-url-to-txtid url)))
    (with-current-buffer (find-file-noselect mandoku-log-file)
      (goto-char (point-max))
      (insert (format-time-string "[%Y-%m-%dT%T%z] INFO " (current-time)))
      (insert (format "Starting to download %s %s\n"
		      (replace-regexp-in-string "http://[^@]+@" "http://" url)
		      (mandoku-textid-to-title txtid)))
      (save-buffer)
      )
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format-time-string "[%Y-%m-%dT%T%z]\n" (current-time)))
      (set-process-sentinel (start-process-shell-command (concat ""
								 (replace-regexp-in-string "http://[^@]+@" "http://" url)) buf
							 (concat mandoku-git-program  " clone " url " -v " targetdir))
			    'mandoku-clone-sentinel)
      )
    (set-buffer curbuf)
    (message "Downloading %s %s" txtid (mandoku-textid-to-title txtid))
  ))

(defun mandoku-clone-sentinel (proc msg)
    (if (string-match "finished" msg)
      ;; TODO: check write to log, write to index queue etc.
	(let ((txtid  (mandoku-url-to-txtid (format "%s" proc)))) 
	  ;; make the log entry
	  (with-current-buffer (find-file-noselect mandoku-log-file)
	    (goto-char (point-max))
	    (insert (format-time-string "[%Y-%m-%dT%T%z] INFO " (current-time)))
	    (insert (format "Finished downloading %s %s\n" proc (mandoku-textid-to-title txtid)))
	    (save-buffer))
	  ;; dont need to do this for the wiki repo
	  (unless (string-match "wiki" (format "%s" proc))
	    (progn
	      ;; this has to be the local catalog file
	      (with-current-buffer (find-file-noselect mandoku-local-catalog)
		(goto-char (point-max))
		(insert (format "*** [[mandoku:%s][%s]] %s\n" txtid txtid (mandoku-textid-to-title txtid)))
		(save-buffer))
	      ;; add it to the index queue
	      (with-current-buffer (find-file-noselect mandoku-index-queue)
		(goto-char (point-max))
		(insert (format "%s %s\n" txtid (mandoku-textid-to-title txtid)))
		(save-buffer)))))
      )
    (message "Download %s %s" proc msg)
    )

(defun mandoku-download-add-text-to-queue ()
  "Adds the text at point or on the current line to dl queue"
  (interactive)
  (let ((txtid (mandoku-get-textid)))
    (if (not txtid)
	(setq txtid (read-string "Enter ID number of text to download: ")))
    (if (mandoku-text-local-p txtid)
	(message "%s %s already downloaded!" txtid (mandoku-textid-to-title txtid))
      (with-current-buffer (find-file-noselect mandoku-download-queue)
	(goto-char (point-max))
	;; we first clone the text if we have a user token
	;; this assumes we have internet and can acccess github...
	(if (< 0 (length mandoku-user-token))
	    (mandoku-fork txtid))
	(insert (format "%s %s\n" txtid (mandoku-textid-to-title txtid)))
	(save-buffer)
	(kill-buffer)
	(message "%s %s added to download list" txtid (mandoku-textid-to-title txtid))))
    ))

(defun mandoku-download-show-queue ()
  (interactive)
  (find-file mandoku-download-queue)
  (goto-char (point-min)))

(defun mandoku-download-process-queue ()
  "Work through the queue and download the texts if necessary."
  (interactive)
  (let (txtid)
    (with-current-buffer (find-file-noselect mandoku-download-queue)
      (goto-char (point-min))
      (while (not (eobp))
	(setq txtid (mandoku-get-textid))
	(if (and txtid (not (mandoku-text-local-p txtid)))
	    (mandoku-get-remote-text txtid))
	(delete-region (point-at-bol)
		  (progn (forward-line) (point)))
;	(next-line)
	)
      (save-buffer)
      (message "Finished processing the texts in the download list %s" mandoku-download-queue)
      )))


(defun mandoku-clone-repo (repo directory)
  (let* ((name (oref repo :name))
         (target (if (file-exists-p directory)
                     (expand-file-name name directory)
                   directory))
         (repo-url (eieio-oref repo github-clone-url-slot)))
    (message "Cloning %s into %s from \"%s\"" name target repo-url)
    (if (not (= 0 (shell-command (format "git clone %s %s" repo-url target)
                                 "*github-clone output*")))
        (error "Failed to clone repo \"%s\" to directory \"%s\"" repo-url target))
    (when (not (string-equal (oref (oref repo :owner) :login)
                                  (github-clone-user-name)))
      (github-clone-fork-repo repo))))

(provide 'mandoku-github)
;; mandoku-github.el ends here
