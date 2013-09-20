;; 
;; custom links to mandoku files from org
;; [2010-02-13T14:55:27+0900]  cwittern@gmail.com


(require 'org)
(require 'mandoku)

(org-add-link-type "mandoku" 'org-mandoku-open)
(add-hook 'org-store-link-functions 'org-mandoku-store-link)

(defun org-mandoku-open (link)
  "Open the text (and optionally go to indicated  position) in LINK.
LINK will consist of a <textid> recognized by mandoku."
  ;; need to get the file name and then call mandoku-execute-file-search
  (let* ((coll (car (split-string link ":")))
	 (textid (car (cdr (split-string link ":"))))
	 (page (concat (if textid 
			   (replace-in-string (car (cdr (cdr (split-string link ":")))) "_" ":" )
			 "")))
	 (src (car (cdr (split-string link "::"))))
	 (fname (concat (if (> (length textid ) 0 )
			    (concat textid "_" (car (split-string page "-")) ".txt")
			  coll)))
	 (filename (concat  "/" 
			    (if (equal coll "krp")
				(concat (substring textid 0 4) "/" textid "/" fname)
			      (if (> (length textid ) 0 )
				  (funcall (intern (concat "mandoku-" coll "-textid-to-file")) textid page)
				(concat (substring coll 0 4) "/" (substring coll 0 8) "/" coll)))
		   )))
;    (message (format "%s" page))
    (if (equal coll "meta")
	  ;; this does a headline only search in meta; we need to have the ID on the headline for this to work
	  (org-open-file filename  t nil (concat "#" textid)) 
					;      (message (format "%s" (concat mandoku-meta-dir  textid ".org" )))
      (if (file-exists-p (concat mandoku-text-dir "/" filename))
	  (org-open-file (concat mandoku-text-dir "/" filename) t nil 
			 (or src page))
			 ;; (if src 
			 ;;     (concat page "::" src)
			 ;;   page))
	(if (file-exists-p (concat mandoku-temp-dir fname))
	    (org-open-file (concat mandoku-temp-dir fname) t nil 
			   (if src 
			       (concat page "::" src)
			     page))
	  (mandoku-open-remote-file filename src page)
	  )
	  ))))



(defun org-mandoku-store-link ()
  "if we are in mandoku-view-mode, or visiting a mandoku file, then store link"
  (when (eq major-mode 'mandoku-view-mode)
  ;; this is a mandoku file, make link
    (save-excursion
      (let* ((coll (mandoku-get-coll (buffer-file-name)))
	     (location (mandoku-position-at-point))
	     (link (concat "mandoku:" coll ":" location))
	     (description (mandoku-cit-format  location)))
	(org-store-link-props
	 :type "mandoku"
	 :link link
	 :description description)))))

(provide 'org-mandoku)

;;; org-mandoku.el ends here


