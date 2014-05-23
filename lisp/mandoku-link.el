;; linking for mandoku files
;; cwittern@gmail.com [2014-05-20T19:57:42+0900]

(require 'mandoku)

(org-add-link-type "mandoku" 'mandoku-link-open)
(add-hook 'org-store-link-functions 'mandoku-link-store-link)

(defvar mandoku-store-link-plist nil
  "Plist with info about the most recent link created with `mandoku-store-link'.")


(defun mandoku-link-open (link)
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



(defun mandoku-link-store-link ()
  "if we are in mandoku-view-mode, or visiting a mandoku file, then store link"
  (when (eq major-mode 'mandoku-view-mode)
  ;; this is a mandoku file, make link
    (save-excursion
      (let* ((fn (file-name-sans-extension (file-name-nondirectory (buffer-file-name ))))
	     (location (car (cdr (cdr (mandoku-position-at-point-internal)))))
	     (charpos (mandoku-charcount-at-point-internal))
	     (textid (car (split-string fn "_")))
	     (title (mandoku-get-title))
	     (juan (mandoku-get-juan))
	     (br (mapcar 'mandoku-chomp (mandoku-get-branches)))
	     (outline (mapconcat 'identity (mandoku-get-outline-path) " / "))
	     (link (concat "mandoku:" fn ":"  location))
	     (region (if (org-region-active-p)
		     (buffer-substring (region-beginning) (region-end))))
	     (description (mandoku-cit-format  location)))
	(org-store-link-props
	 :type "mandoku"
	 :link link
	 :textid textid
	 :edition br
	 :region region
	 :title title
	 :description description)))))


(defun mandoku-insert-link (&optional plink)
  "insert the most recent link"
  (let* ((link (or plink
		   mandoku-store-link-plist
		   org-store-link-plist))
	 (region (plist-get link :region))
	 (edition (plist-get link :edition))
	 (title (plist-get link :title))
	 )
    (insert (concat "** " title "\n:PROPERTIES:\n:edition: " edition "\n:END:\n\n" region )))
	 
)

(provide 'mandoku-link)

