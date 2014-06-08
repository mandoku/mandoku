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
  (let (type p olp start rest f1 key)
    (save-excursion 
      (goto-char (point-min))
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
			      (search-backward f1)))
			   "+" (number-to-string (length f1) ))
	 :rest rest
	 :key key
	 :olp (mandoku-get-outline-path p)
	 :type type)
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
      (if (re-search-forward
	   (format org-complex-heading-regexp-format (regexp-quote lf))
	   nil t)
	  (insert "\n** ")
	(insert (concat "\n* " hd "\n:PROPERTIES:\n))

      
	 
))))

(provide 'mandoku-annot)
