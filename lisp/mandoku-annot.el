;; annotating for mandoku files
;; cwittern@gmail.com [2014-06-06T13:01:38+0900]

(require 'mandoku)

(defvar mandoku-annot-dir (expand-file-name  (concat mandoku-base-dir "notes/")))

(defvar mandoku-annot-regex "^\\([^:\n ]+\\) *\\([^:]+\\)::\\(.*?\\)$") 

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
	 (lf (or key hd))
	 )
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
