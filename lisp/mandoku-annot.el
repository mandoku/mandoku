;; annotating for mandoku files
;; cwittern@gmail.com [2014-06-06T13:01:38+0900]

(require 'mandoku)

(defvar mandoku-annot-dir (expand-file-name  (concat mandoku-base-dir "notes/")))

(defvar mandoku-annot-regex "^\\([^:\n ]+\\) *\\([^:]+\\)::\\(.*?\\)$") 

;;;###autoload
(defun mandoku-annot-annotate (beg end &optional skip-pinyin)
  "Annotate the selected term on the following line."
  (interactive "r")
  (let* ((term (mandoku-remove-markup (buffer-substring-no-properties beg end)))
	 (pinyin (if (fboundp 'helm-charinfo-get-pinyin )
		     (concat " [" (helm-charinfo-get-pinyin term) "] ")
		   ""))
	 (case-fold-search t))
    (forward-line)
    (beginning-of-line)
    (if (looking-at ":zhu:")
	(progn
	  (re-search-forward ":end:")
	  (beginning-of-line)
	  (insert term 
		  pinyin
		  "\n" )
	  (previous-line))
      (progn
	(insert ":zhu:\n \n:end:\n")
	(previous-line 2)
	(beginning-of-line)
	(insert term pinyin)))
    (beginning-of-line)
    (deactivate-mark)))


(defun mandoku-annot-write-to-file (filename &optional n)
  "Write to file, optional n indicates a 'notes first' format."
  (interactive)
  (let (beg end tl pos notes
	    (commit (magit-rev-verify "HEAD"))
	    (fn (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
	    (outb (find-file-noselect filename)))
    (goto-char (point-min))
    (with-current-buffer outb
      (erase-buffer)
      (goto-char (point-min))
      (insert (format-time-string ";;[%Y-%m-%dT%T%z]\n" (current-time)))
      (insert "* Notes for " fn "\n"))
    (while (search-forward mandoku-annot-start nil t)
      (setq beg (point))
      (setq tl (mandoku-annot-targetline))
      (setq pos (mandoku-charcount-at-point-internal))
      (search-forward mandoku-annot-end)
      (setq end (point))
      (setq notes (mandoku-annot-collect beg end))
      (if (not n)
	  (with-current-buffer outb
	    (insert "** " (car tl) "\n:PROPERTIES:\n:COMMIT: "
		    commit
		    "\n:LOCATION: mandoku:"
		    (format "%s:%s\n" (car (cadr tl)) (car (cdr (cdr (cadr tl)))))
		    ":CHARPOS: " (number-to-string pos)
		    "\n:END:\n"
		    )
	    (dolist (note notes)
	      (insert note "\n")))
	(with-current-buffer outb
	  (dolist (note notes)
	    (insert "** " (mandoku-cut-string (car (split-string note ))) "\n:PROPERTIES:\n:COMMIT: "
		    commit
		    "\n:TARGET: " (car tl)
		  "\n:LOCATION: mandoku:"
		  (format "%s:%s\n" (car (cadr tl)) (car (cdr (cdr (cadr tl)))))
		  ":CHARPOS: " (number-to-string pos)
		  "\n:END:\n"
		  )
	    (insert note "\n")))
	  
      ;; now store the annotations or do something
      ))))

(defun mandoku-annot-find ()
  (interactive)
  (let (beg end tl bline pos)
    (while (search-forward mandoku-annot-start)
      (setq beg (point))
      (setq tl (mandoku-annot-targetline))
      (setq bline (save-excursion
                    (forward-line -2)
                    (point)))
      (setq pos (mandoku-position-at-point-internal bline))
      (search-forward mandoku-annot-end)
      (setq end (point))
      (setq txx (mandoku-annot-collect beg end)
      ;; now store the annotations or do something
      ))))

(defun mandoku-annot-targetline (&optional beg)
  "Line that is target for the annotation."
  (save-excursion
    (when beg (goto-char beg))
    (unless (looking-at mandoku-annot-drawer)
      (search-backward mandoku-annot-drawer))
    (forward-line -1)
    (list
     (thing-at-point 'line t)
     (mandoku-position-at-point-internal))))


(defun mandoku-annot-collect (beg end)
  "This is called with beg as beginning and end as the end of an
annotation drawer. It will collect all annotations between beg
and end. A new annotation begins in column 0 of a new line and at
that position has to be a character from the line that is target
of the annotation."
  (let ((tl (mandoku-annot-targetline beg))
	(buffer-invisibility-spec nil)
	notes)
    (save-excursion
      (goto-char beg)
      (forward-line 1)
      (while (< (point) end)
	(skip-chars-forward " ")
	(when (or (looking-at ":")
		  (string-match (char-to-string (char-after)) (car tl)))
	  (push (string-trim
		 (buffer-substring-no-properties beg (- (point) 1))) notes)
	  (setq beg (point)))
	(forward-line 1)
	))
    (reverse notes)))
  
(defun mandoku-location-put (&rest stuff)
  "Add properties to the capture property list `mandoku-location-plist'"
  (while stuff
    (setq mandoku-location-plist (plist-put mandoku-location-plist
				       (pop stuff) (pop stuff)))))


(defun mandoku-annot-scan (&optional annot-dir)
  (interactive)
  (let ((fn (file-name-sans-extension (file-name-nondirectory (buffer-file-name ))))
	type p olp start rest f1 key)
    (save-excursion 
;      (goto-char (point-min))
      (while (re-search-forward mandoku-annot-regex nil t)
	(setq key nil)
	(setq start (match-beginning 0))
	(setq f1 (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
	(setq type (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
	(setq rest (split-string (buffer-substring-no-properties (+ 1 (match-beginning 3)) (match-end 3)) "@"))
	(setq mandoku-location-plist nil )
	(dolist (r rest)
	  (when (string-match "key" r) 
	    (setq key (cadr (split-string r "="))))
	(mandoku-location-put 
	 :context f1
	 :location (concat (mandoku-position-with-char
			    (save-excursion
			      (goto-char start)
			      (ignore-errors
			      (re-search-backward 
			       (replace-regexp-in-string "\\(.\\)" "\\1¶?" f1)
			       ))))
			   "+" (number-to-string (length f1) ))
	 :title (mandoku-get-title)
	 :textid (car (split-string fn "_"))
	 :filename fn
	 :rest rest
	 :key key
	 :olp (mandoku-get-outline-path p)
	 :type type)
	(mandoku-link-store-link)
	(mandoku-annot-insert)
	)))))      
    

(defun mandoku-annot-insert ()
  (let* ((type (plist-get mandoku-location-plist :type))
	 (annot-file (concat mandoku-annot-dir  type ".txt"))
	 (hd (plist-get mandoku-location-plist :context))
	 (rest (plist-get mandoku-location-plist :rest))
	 (key (plist-get mandoku-location-plist :key))
	 (fn (plist-get mandoku-location-plist :filename))
	 (lf (or key hd))
	 )
    (unless (file-exists-p mandoku-annot-dir)
      (mkdir mandoku-annot-dir t))
    (with-current-buffer (find-file annot-file)
      (org-mode)
      (goto-char (point-min))
      (unless (re-search-forward lf
;	   (format org-complex-heading-regexp-format (regexp-quote lf))
	   nil t)
	;; if we found an entry, we still might have a different name -> all names also in location-entry
	(goto-char (point-max))
	(insert (concat "\n** " hd "\n:PROPERTIES:\n" 
			(if key (concat ":ID: " key "\n" ) "")
			":END:\n" )))
      (org-end-of-subtree)
      (insert (concat "\n*** [[mandoku:krp:"
		      (plist-get org-store-link-plist :textid)
		      ":"
		      (plist-get mandoku-location-plist :location)
		      "::" hd "]["
		      hd "]] ("
		      (plist-get mandoku-location-plist :title)
		      ","
		      (format "%d" (string-to-number (cadr (split-string fn "_"))))
		      ": "
		      (mapconcat 'identity (plist-get mandoku-location-plist :olp) " / ")
		      ") "
		      "\n"))
	(insert (concat ":PROPERTIES:\n:LOCATION: " 
			(plist-get mandoku-location-plist :location)
			"\n:TEXTID: "
			(plist-get org-store-link-plist :textid)
			"\n:END:\n" )))
      
      (save-buffer)
))


(provide 'mandoku-annot)
