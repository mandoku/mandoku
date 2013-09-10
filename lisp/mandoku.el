;; mandoku.el   -*- coding: utf-8 -*-
;; created [2001-03-13T20:32:32+0800]  (as smart.el)
;; renamed and refactored [2010-01-08T17:01:43+0900]
(require 'org)
(defvar mandoku-base-dir (expand-file-name  "/Users/Shared/md/"))
(defvar mandoku-do-remote nil)

(defvar mandoku-text-dir (expand-file-name (concat mandoku-base-dir "text/")))
(defvar mandoku-image-dir nil)
(defvar mandoku-index-dir nil)
(defvar mandoku-meta-dir (expand-file-name  (concat mandoku-base-dir "meta/")))
(defvar mandoku-temp-dir (expand-file-name  (concat mandoku-base-dir "temp/")))
(defvar mandoku-sys-dir (expand-file-name  (concat mandoku-base-dir "system/")))

(defvar mandoku-string-limit 10)


;; ** Textfilters
;; we have one default textfilter, which always exists and can be dynamically treated. 
(defvar mandoku-default-textfilter (make-hash-table :test 'equal) )
(setplist 'mandoku-default-textfilter '(:name "Default" :active t))
;; more textfilters can be added to the list
(defvar mandoku-textfilter-list (list 'mandoku-default-textfilter))
;; switch the whole filter mechanism on or off.
(defvar mandoku-use-textfilter nil)
;; control, which collections are used.
;; this could be a list? currently only one subcoll allowed, but it could be a regex understood by the shell ZB6[rq]
(defvar mandoku-search-limit-to-coll nil)
;; ** Catalogs
(defvar mandoku-catalogs-alist nil)



(defvar mandoku-file-type ".txt")
;;we skip: 》《 
(defvar mandoku-punct-regex-post "\\([^
]\\)\\([　-〇〉」』】〗〙〛〕-㄀︀-￯)]+\\)")
(defvar mandoku-punct-regex-pre "\\([^
]\\)\\([(〈「『【〖〘〚〔]+\\)")

(defvar mandoku-kanji-regex "\\([㐀-鿿𠀀-𪛟]+\\)")

(defvar mandoku-regex "<[^>]*>\\|[　-㄀＀-￯\n¶]+\\|\t[^\n]+\n")

(defun mandoku-update-subcoll-list ()
  ;; dont really need this outer loop at the moment...
  (dolist (x mandoku-repositories-alist)
    (let ((scfile (concat mandoku-sys-dir "subcolls.txt")))
      (with-current-buffer (find-file-noselect scfile t)
	(erase-buffer)
	(insert (format-time-string ";;[%Y-%m-%dT%T%z]\n" (current-time)))
	(dolist (y mandoku-catalogs-alist)
	  (let ((tlist 
		 (with-current-buffer (find-file-noselect (cdr y))
		   (org-map-entries 'mandoku-get-header-item "+LEVEL=2"))))
	    (with-current-buffer (file-name-nondirectory scfile)
	      (dolist (z tlist)
		(insert (concat (car z) "\t" (car (last z)) "\n")))
	      (save-buffer))))
	      (kill-buffer (file-name-nondirectory scfile) )))))

	  
(defun mandoku-update-title-lists ()
  (dolist (x mandoku-catalogs-alist)
    ;; ("ZB6 佛部" . "/Users/chris/projects/meta/zb-cbeta.org")
    (message (concat  "Reading catalog file for: "  (car x)))
    (let* ((titlefile (concat mandoku-sys-dir (car (split-string (car x))) "-titles.txt"))
	   (volfile (concat mandoku-sys-dir (car (split-string (car x))) "-volumes.txt"))
	   (lookupfile (concat mandoku-sys-dir (car (split-string (car x))) "-lookup.txt"))
	  (catfile (cdr x))
	  (tlist 
	   (with-current-buffer (find-file-noselect catfile)
	     (org-map-entries 'mandoku-get-header-item "+LEVEL=3"))))
      (message (format "%s" (concat "Updating file: " titlefile)))
      (with-current-buffer (find-file-noselect titlefile t)
	(erase-buffer)
	(insert (format-time-string ";;[%Y-%m-%dT%T%z]\n" (current-time)))
	(dolist (y tlist)
	  (insert (concat (car y) "\t" (car (last y)) "\n")))
	(save-buffer)
	(kill-buffer))
      (message (concat "Updating file: " volfile))
      (with-current-buffer (find-file-noselect volfile t)
	(erase-buffer)
	(insert (format-time-string ";;[%Y-%m-%dT%T%z]\n" (current-time)))
	(dolist (y tlist)
	  ;; if there is a CBETA number, it is in the middle: we want the first part before "n"
	  (if (< 2 (length y))
	      (insert (concat (car y) "\t"  (car (split-string (car (cdr y)) "n"))  "\n"))))
	(save-buffer)
	(kill-buffer))
      (with-current-buffer (find-file-noselect lookupfile t)
	(erase-buffer)
	(insert (format-time-string ";;[%Y-%m-%dT%T%z]\n" (current-time)))
	(dolist (y tlist)
	  ;; if there is a CBETA number, it is in the middle: we want the first part before "n"
	  (if (< 2 (length y))
	      (insert (concat (car (cdr y)) "\t"  (car y)  "\n"))))
	(save-buffer)
	(kill-buffer))
      (message "Done!")
;;      (kill-buffer catfile)
  )))

(defun mandoku-get-header-item ()
  (let ((end (save-excursion(end-of-line) (point)))
	(begol (save-excursion (beginning-of-line) (search-forward " ") )))
    (split-string 
     (replace-regexp-in-string org-bracket-link-regexp "\\3" 
			       (buffer-substring-no-properties begol end)))))

(defun mandoku-read-lookup-list () 
  "read the titles table"
  (setq mandoku-lookup (make-hash-table :test 'equal))
  (dolist (x mandoku-catalogs-alist)
    (when (file-exists-p (concat mandoku-sys-dir (car (split-string (car x))) "-lookup.txt"))
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8)
              textid)
          (insert-file-contents (concat mandoku-sys-dir (car (split-string (car x))) "-lookup.txt"))
          (goto-char (point-min))
          (while (re-search-forward "^\\([a-z0-9]+\\)	\\([^	
]+\\)" nil t)
	     (puthash (match-string 1) (match-string 2) mandoku-lookup)))))))


(defun mandoku-read-titletables () 
  "read the titles table"
  (setq mandoku-subcolls (make-hash-table :test 'equal))
  (when (file-exists-p (concat mandoku-sys-dir  "subcolls.txt"))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8)
	    textid)
	(insert-file-contents (concat mandoku-sys-dir "subcolls.txt"))
	(goto-char (point-min))
	(while (re-search-forward "^\\([a-z0-9]+\\)	\\([^	
]+\\)" nil t)
	  (puthash (match-string 1) (match-string 2) mandoku-subcolls)))))

  (setq mandoku-titles (make-hash-table :test 'equal))
  (dolist (x mandoku-catalogs-alist)
    (when (file-exists-p (concat mandoku-sys-dir (car (split-string (car x))) "-titles.txt"))
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8)
              textid)
          (insert-file-contents (concat mandoku-sys-dir (car (split-string (car x))) "-titles.txt"))
          (goto-char (point-min))
          (while (re-search-forward "^\\([a-z0-9]+\\)	\\([^	
]+\\)" nil t)
	     (puthash (match-string 1) (match-string 2) mandoku-titles)))))))



(defun char-to-ucs (char)
  char
)
(defun mandoku-char-to-ucs (char)
  (format "%04x" (char-to-ucs char))
)

(defun mandoku-what (char)
  (interactive (list (char-after)))
	(message (mandoku-char-to-ucs char))
)

(defun mandoku-next-three-chars ()
  (save-excursion
    (list
     (char-after)
     (progn (mandoku-forward-one-char) (char-after))
     (progn (mandoku-forward-one-char) (char-after))
     (progn (mandoku-forward-one-char) (char-after))
     (progn (mandoku-forward-one-char) (char-after))
     (progn (mandoku-forward-one-char) (char-after))
)))


(defun mandoku-forward-one-char ()
	"this function moves forward one character, ignoring punctuation and markup
One character is either a character or one entity expression"
	(interactive)
	(save-match-data
	(if (looking-at "&[^;]*;")
	    (forward-char (- (match-end 0) (match-beginning 0)))
	  (forward-char 1)
	)
	;; this skips over newlines, punctuation and markup.
	;; Need to expand punctuation regex [2001-03-15T12:30:09+0800]
	;; this should now skip over most ideogrph punct
	(while (looking-at mandoku-regex)
		(forward-char (- (match-end 0) (match-beginning 0)))))
)

(defun mandoku-forward-n-characters (num)
	(while (> num 0)
		(setq num (- num 1))
		(message (number-to-string num))
		(mandoku-forward-one-char))
)



(defun mandoku-grep-internal (search-string)
  (interactive "s")
  (let ((coding-system-for-read 'utf-8)
	(coding-system-for-write 'utf-8)
	(index-buffer (get-buffer-create "*temp-mandoku*"))
	(the-buf (current-buffer))
	(result-buffer (get-buffer-create "*Mandoku Index*"))
	(org-startup-folded t)
	(mandoku-count 0))
    (progn
      (set-buffer index-buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (mandoku-search-internal search-string index-buffer)
      ;; setup the buffer for the index results
      (set-buffer result-buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      ;; switch to index-buffer and get the results
      (mandoku-read-index-buffer index-buffer result-buffer search-string)
      )))

(defun mandoku-search-internal (search-string index-buffer)
  (if mandoku-do-remote 
      (mandoku-search-remote search-string index-buffer)
    (mandoku-search-local search-string index-buffer)
))

(defun mandoku-search-local (search-string index-buffer)
;; find /tmp/index/SDZ0001.txt -name "97.idx.*" | xargs zgrep "^靈寳"
  (let ((coding-system-for-read 'utf-8)
	(coding-system-for-write 'utf-8)
	(search-char (string-to-char search-string)))
      (shell-command
		    (concat "bzgrep -H " "^"
		     (substring search-string 1 )
		     " "
		     mandoku-index-dir
		     (substring (format "%04x" search-char) 0 2)
		     "/"
		     (format "%04x" search-char)
		     (if mandoku-search-limit-to-coll
			 (concat "." mandoku-search-limit-to-coll)
		       "")
		     "*.idx* | cut -d : -f 2-")
		    index-buffer nil
		    )
))

(defun mandoku-read-index-buffer (index-buffer result-buffer search-string)
  (let (
	(mandoku-count 0)
	(mandoku-filtered-count 0)
      	(search-char (string-to-char search-string)))

      (switch-to-buffer-other-window index-buffer t)
;;xx      (set-buffer index-buffer)
;; first: sort the result (after the filename)
      (sort-lines nil (point-min) (point-max))
      (goto-char (point-min))
      (while (re-search-forward
	      (concat
	       ;; match-string 1: collection
	       ;; match-string 2: match
	       ;; match-string 3: pre
	       ;; match-string 4: location
	       ;; the following are optional:
	       ;; match-string 5: dummy
	       ;; match-string 6: addinfo
	       ;; match-string 1: dummy
	       ;; match-string 2: collection
	       ;; match-string 3: match
	       ;; match-string 4: pre
	       ;; match-string 5: location
	       ;; the following are optional:
	       ;; match-string 6: dummy
	       ;; match-string 7: addinfo
	       "^\\([^,]*\\),\\([^\t]*\\)\t\\([^\t \n]*\\)\\(\t[^\n\t ]*\\)?$"
	) nil t )
	(let* (
	       ;;if no subcoll, need to switch the match assignments.
	      (pre (match-string 2))
	      (post (match-string 1))
	      (location (split-string (match-string 3) ":" ))
	      (extra (match-string 8))
	      )
	  (let* ((txtid (car location))
		 (pag (car (cdr location)))
		 (line (car (cdr (cdr location))))
		 (page (if (string-match "[-_]"  pag)
			   (concat (substring pag 0 (- (length pag) 1))
				   (mandoku-num-to-section (substring pag (- (length pag) 1))) line)
			 (concat
			  (format "%4.4d" (string-to-number (substring pag 0 (- (length pag) 1))))
			  (mandoku-num-to-section (substring pag (- (length pag) 1)))
			  line)))
		 (vol (mandoku-textid-to-vol txtid))
		 (tit (mandoku-textid-to-title txtid)))
	    (set-buffer result-buffer)
	    (unless (mandoku-apply-filter txtid)
	    (setq mandoku-filtered-count (+ mandoku-filtered-count 1))
	    (insert "** [[mandoku:krp:" 
		    txtid
		    ":"
		    page
		    "::"
		    search-string
		    "]["
;		    txtid
;		    " "
		    (if vol
			(concat vol ", ")
		      "")
		    page
		    "]]"
		    "\t"
		    pre
;		    "\t"
		    search-char
		    post
		    "  [[mandoku:meta:"
		    txtid
		    ":10][《" txtid " "
		    (format "%s" tit)
		    "》]]\n"
		    )
;; additional properties
	    (insert ":PROPERTIES:\n:COLL: krp"
		    "\n:ID: " txtid
		    "\n:PAGE: " txtid ":" page
		    "\n:PRE: "  (concat (nreverse (string-to-list pre)))
		    "\n:POST: "
		    search-char
		    post
		    "\n:END:\n"
		    ))
	    (set-buffer index-buffer)
	    (setq mandoku-count (+ mandoku-count 1))
	    )))
      (switch-to-buffer-other-window result-buffer t)
      (goto-char (point-min))
;      (insert (format "There were %d matches for your search of %s:\n"
;       mandoku-count search-string))
      (if (equal mandoku-use-textfilter t)
	  (insert (format "Active Filter: %s , Matches: %d (Press 't' to temporarily disable the filter)\n" 
			  (mapconcat 'mandoku-active-filter mandoku-textfilter-list "")
			  mandoku-filtered-count))
	)
      (insert (format "Location\tMatch               Source\n* %s (%d/%d)\n"  search-string mandoku-filtered-count mandoku-count))
      (mandoku-index-mode)
 ;     (org-overview)
      (hide-sublevels 2)
      (replace-buffer-in-windows index-buffer)
;      (kill-buffer index-buffer)
))

(defun mandoku-textid-to-vol (txtid) nil)
(defun mandoku-textid-to-title (txtid) 
;  (list txtid (gethash txtid mandoku-titles)))
  (gethash txtid mandoku-titles))

(defun mandoku-meta-textid-to-file (txtid page)
  (let ((repid (car (split-string txtid "[0-9]"))))
;    (concat mandoku-meta-dir repid "/" (substring txtid 0 (+ (length repid) 2)) ".org")))
    (concat mandoku-meta-dir repid "/" (substring txtid 0 (+ (length repid) 1)) ".org")))


(defun mandoku-get-outline-path ()
  "this includes the first upward heading"
  (save-excursion
    (let ((olp ))
	  (outline-previous-visible-heading 1)
	  (when (looking-at org-complex-heading-regexp)
	    (push (org-trim
		   (replace-regexp-in-string
		    ;; Remove statistical/checkboxes cookies
		    "\\[[0-9]+%\\]\\|\\[[0-9]+/[0-9]+\\]\\|¶" ""
		    (org-match-string-no-properties 4)))
		  olp))
	  (while (org-up-heading-safe)
	    (when (looking-at org-complex-heading-regexp)
	      (push (mandoku-cut-string (org-trim
		     (replace-regexp-in-string
		      ;; Remove statistical/checkboxes cookies
		      "\\[[0-9]+%\\]\\|\\[[0-9]+/[0-9]+\\]\\|¶" ""
		      (org-match-string-no-properties 4))))
		    olp)))
	  olp)))


(defun mandoku-cut-string (s)
  (if (< mandoku-string-limit (length s)  )
      (substring s 0 mandoku-string-limit)
    s))

;; (defun mandoku-read-index-buffer (index-buffer result-buffer search-string)
;;   (let (
;; 	(mandoku-count 0)
;; 	(mandoku-filtered-count 0)
;;       	(search-char (string-to-char search-string)))

;;       (switch-to-buffer-other-window index-buffer t)
;; ;;xx      (set-buffer index-buffer)
;; ;; first: sort the result (after the filename)
;;       (sort-regexp-fields nil "^[^:]*:\\(.*\\)$" "\\1" (point-min) (point-max))

;;       (goto-char (point-min))
;;       (while (re-search-forward
;; 	      (concat
;; 	       ;; match-string 1: collection
;; 	       ;; match-string 2: match
;; 	       ;; match-string 3: pre
;; 	       ;; match-string 4: location
;; 	       ;; the following are optional:
;; 	       ;; match-string 5: dummy
;; 	       ;; match-string 6: addinfo
;; 	       ;; match-string 1: dummy
;; 	       ;; match-string 2: collection
;; 	       ;; match-string 3: match
;; 	       ;; match-string 4: pre
;; 	       ;; match-string 5: location
;; 	       ;; the following are optional:
;; 	       ;; match-string 6: dummy
;; 	       ;; match-string 7: addinfo

;; ;;       "^[^.]*.\\([^.]*\\)?.\\(.*\\).idx[^:]*:\\([^,]*\\),\\([^\t]*\\)\t\\([^\t \n]*\\)\\(\t?\\([^\n\t ]*\\)\\)$"

;; ;;       "^[^.]*.\\([^.]*\\)?.?\\(.*\\).idx[^:]*:\\([^,]*\\),\\([^\t]*\\)\t\\([^\t \n]*\\)\\(\t[^\n\t ]*\\)?$"
;; 	       "^\\([^.]*.\\([^.]*\\)?.?\\(.*\\).idx[^:]*\\)?:?\\([^,]*\\),\\([^\t]*\\)\t\\([^\t \n]*\\)\\(\t[^\n\t ]*\\)?$"
;; 	) nil t )
;; 	(let* (
;; 	       ;;if no subcoll, need to switch the match assignments.
;; 	      (subcoll (if (equal "" (match-string 3))
;; 			   (match-string 3)
;; 			 (match-string 2)))
;; 	      (coll  (if (equal "" (match-string 3))
;; 			   (match-string 2)
;; 			 (match-string 3)))
;; 	      (pre (match-string 5))
;; 	      (post (match-string 4))
;; 	      (location (funcall (intern (concat "mandoku-" coll "-parse-location")) (match-string 6)))
;; 	      ;(vol (format "%02d" (string-to-number (match-string 7))))
;; 	      ;(page (match-string 4))
;; 	      ;(sec (match-string 5))
;; 	      (line (match-string 7))
;; 	      (extra (match-string 8))
;; ;;	      (markup (match-string 8))
;; 	      )
;; 	  (let* ((vol (car location))
;; 		 (pag (car (cdr location)))
;; 		 (line (car (cdr (cdr location))))
;; 		 (page (if (string-match "[-_]"  pag)
;; 			   (concat (substring pag 0 (- (length pag) 1))
;; 				   (mandoku-num-to-section (substring pag (- (length pag) 1))) line)
;; 			 (concat
;; 			  (format "%4.4d" (string-to-number (substring pag 0 (- (length pag) 1))))
;; 			  (mandoku-num-to-section (substring pag (- (length pag) 1)))
;; 			  line)))
;; 		 (tx (if (string-match "_"  (car (cdr location)))
;; 			 ;; if the length is five, we have a location with the textnum at the end, otherwise it starts with a vol and we have to get the textid from there
;; 		       (funcall (intern (concat "mandoku-" coll "-textid-to-title"))
;; 			(if subcoll
;; 			    (concat (upcase subcoll) (car location))
;; 			  vol)
;; 		       (concat page ""))

;; ;;
;; 		       (funcall (intern (concat "mandoku-" coll "-textid-to-title"))
;; 			(if subcoll
;; 			    (concat subcoll vol )
;; 			  vol)
;; 		       (concat page ""))))
;; 		 ;; (text (funcall (intern (concat "mandoku-" coll "-vol-page-to-file"))
;; 		 ;;       subcoll
;; 		 ;;       (string-to-number vol)
;; 		 ;;       (string-to-number pag)))
;; 		 )
;; 	    (set-buffer result-buffer)
;; 	    (unless (mandoku-apply-filter (car tx))
;; 	    (setq mandoku-filtered-count (+ mandoku-filtered-count 1))
;; 	    (insert "** [[mandoku:" coll ":" 
;; ;		    (if (not (equal (substring subcoll 0 2) "ZB"))
;; ;			 subcoll)
;; 		    vol
;; 		    ":"
;; 		    page
;; 		    "::"
;; 		    search-string
;; 		    "]["
;; ;		    (if (not (equal (substring subcoll 0 2) "ZB"))
;; ;			(upcase subcoll))
;; 		    vol
;; 		    ", "
;; 		    page
;; 		    "]]"
;; 		    "\t"
;; 		    pre
;; ;		    "\t"
;; 		    search-char
;; 		    post
;; 		    "  [[mandoku:meta:"
;; 		    coll
;; 		    ":"
;; 		    (car tx)
;; 		    "][《"
;; 		    (format "%s" (car (cdr tx)))
;; 		    "》]]\n"
;; 		    )
;; ;; additional properties
;; 	    (insert ":PROPERTIES:\n:COLL: "
;; 		    coll
;; 		    "\n:ID: " (car tx)
;; 		    "\n:PRE: "  (concat (nreverse (string-to-list pre)))
;; 		    "\n:POST: "
;; 		    search-char
;; 		    post
;; 		    "\n:END:\n"
;; 		    ))
;; 	    (set-buffer index-buffer)
;; 	    (setq mandoku-count (+ mandoku-count 1))
;; 	    )))
;;       (switch-to-buffer-other-window result-buffer t)
;;       (goto-char (point-min))
;; ;      (insert (format "There were %d matches for your search of %s:\n"
;; ;       mandoku-count search-string))
;;       (if (equal mandoku-use-textfilter t)
;; 	  (insert (format "Active Filter: %s , Matches: %d (Press 't' to temporarily disable the filter)\n" 
;; 			  (mapconcat 'mandoku-active-filter mandoku-textfilter-list "")
;; 			  mandoku-filtered-count))
;; 	)
;;       (insert (format "Location\tMatch               Source\n* %s (%d/%d)\n"  search-string mandoku-filtered-count mandoku-count))
;;       (mandoku-index-mode)
;;  ;     (org-overview)
;;       (hide-sublevels 2)
;;       (replace-buffer-in-windows index-buffer)
;; ;      (kill-buffer index-buffer)
;; ))


(defun manoku-index-no-filter ()
  "Temporarily displays the search result without applying a filter"
  (interactive)
  (save-match-data 
  (let ((mandoku-use-textfilter nil)
	(index-buffer (get-buffer "*temp-mandoku*"))
	(result-buffer (current-buffer))
	(search-string (progn
			 (goto-char (point-min))
			 (re-search-forward "^\* \\([^ ]*\\) (")
			 (match-string 1))))
    (set-buffer result-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (mandoku-read-index-buffer index-buffer result-buffer search-string))))

(defun mandoku-apply-filter (textid)
  "Apply a filter to the search results."
    (if (equal mandoku-use-textfilter t)
	(let ((test t))
	(dolist (f mandoku-textfilter-list)
	  (if (get f :active)
	      (if (gethash textid (symbol-value f))
		  (setq test nil))))
	test)
    ))

(defun mandoku-active-filter (f)
;; rewrite as mapping function
    (if (get f :active)
	(if (get f :filename)
	    (format "[[file:%s][%s]] " 
		    (get f :filename) 
		    (get f :name))
	  (get f :name)
	  )
      nil))

(defun mandoku-read-textfilter (filename )
  "Reads a new textfilter and adds it to the list of textfilters"
  (when (file-exists-p filename)
    (let ((fn (file-name-sans-extension (file-name-nondirectory filename))))
      (eval (read (concat "(setq tab-" (file-name-sans-extension (file-name-nondirectory filename)) " (make-hash-table :test 'equal))")))
      (eval (read (concat "(put 'tab-" fn " :name \042" fn "\042)")))
      (eval (read (concat "(put 'tab-" fn " :filename \042" filename "\042)")))
      (eval (read (concat "(put 'tab-" fn " :active  t )")))
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8)
              textid)
          (insert-file-contents filename)
          (goto-char (point-min))
          (while (re-search-forward "^\\([a-z0-9]+\\)\s+\\([^\s\n]+\\)" nil t)
	    (eval (read (concat "(puthash (match-string 1) (match-string 2) tab-" fn ")"))))))
      (eval (read (concat "(add-to-list 'mandoku-textfilter-list 'tab-" fn ")"))))))
      




(defun mandoku-make-textfilter ()
  "Creates a new textfilter and adds it to the list of textfilters"
)

(defun mandoku-grep (beg end)
  (interactive "r")
  (mandoku-grep-internal (buffer-substring-no-properties beg end)))

;;;###autoload
(defun mandoku-grep-n3 (search-for)
  (interactive
  (let ((search-for (mapconcat 'char-to-string (mandoku-next-three-chars) "")))
    (list (read-string "Search for: " search-for))))
    (mandoku-grep-internal search-for)
)


(defun mandoku-num-to-section (num)
  "Converts the number codes used in the index to the conventionally used values abc"
  (format "%c" (+ (string-to-number num) 96)))

(defun mandoku-section-to-num (sec)
  "Converts the number codes used in the index to the conventionally used values abc"
  (- (string-to-char sec) 96))


(defun mandoku-parse-pno (s)
" parse a pagenumber s format is pagenumber:line or maybe 462a12"
    (let
	((page (if (posix-string-match ":" s)
		   (car (split-string s ":"))
		 (if (posix-string-match "[a-z]" s)
		     (substring s 0 (+ 1 (length (car (split-string s "[a-z]")))))
		   s)))
	 (line (if (posix-string-match "[a-z:]" s)
		   (string-to-int (car (cdr  (split-string s "[a-z:]"))))
		 0)))
     (string-to-int (format "%s%s%2.2d" (substring page 0 (- (length page) 1) ) (mandoku-section-to-num (substring page (- (length page) 1) ))  line))
      ))

(defun mandoku-execute-file-search (s)
"Go to the line indicated by s format is pagenumber:line or maybe 462a12"
  (when (or (eq major-mode 'mandoku-view-mode) (eq major-mode 'org-mode))
    (let* (
	   (page
	    (if (posix-string-match "[a-h]" s)
		     (substring s 0 (+ 1 (length (car (split-string s "[a-o]")))))
	      (if (posix-string-match "l" s)
		   (car (split-string s "l"))
		s)))
	   (line (if (posix-string-match "[a-o]" s)
		     (string-to-int (car (cdr  (split-string (car (split-string s "::")) "[a-o]"))))
		 0))
	   (search (if (posix-string-match "::" s)
		       (car (cdr (split-string s "::")))
		     nil)))
    (goto-char (point-min))
    (re-search-forward page nil t)
    (while (< -1 line)
      (re-search-forward "¶" nil t)
      (+ (point) 1)
      (setq line (- line 1)))
    (beginning-of-line-text)
    (if search
	(progn
	  (hi-lock-mode t)
	  ;; FIXME: need to construct a true regex here!
	  (highlight-regexp
	   (mapconcat 'char-to-string
		      (string-to-list search) (concat "\\(" mandoku-regex "\\)?")))
	  ;; (message 	   (mapconcat 'char-to-string
	  ;; 	      (string-to-list search) (concat "\\(" mandoku-regex "\\)?")))
	  )
    ))
  ;; return t to indicate that the search is done.
    t))
(defun mandoku-position-at-point ()
  (interactive)
  (message (mandoku-position-at-point-internal)))
(defun mandoku-position-at-point-internal ()
  (save-excursion
    (let ((p (point)))
      (re-search-backward "<pb:" nil t)
      (re-search-forward "\\([^_]*\\)_\\([^_>]*\\)>" nil t)
      (setq textid (match-string 1))
      (setq page (match-string 2))
      (setq line -1)
      (while (and
	      (< (point) p )
	      (re-search-forward "¶" (point-max) t))
	(setq line (+ line 1)))
      (format "%s%s, p%s%2.2d" textid (if (mandoku-get-vol) (mandoku-get-vol) "") (car (cdr (split-string page "-"))) line))))

(defun mandoku-open-image-at-page ()
  (interactive)
  (let* (
	 (coll (mandoku-get-coll buffer-file-name))
	 (path (concat mandoku-image-dir coll "/"
	   (funcall (intern
		     (concat "mandoku-" coll "-page-to-image"))
		    (mandoku-position-at-point-internal)   ))))
  (find-file-other-window path )))


(defun mandoku-position-at-point-internal ()
  (interactive)
  (save-excursion
    (let ((p (point)))
      (re-search-backward "<pb:" nil t)
      (re-search-forward "\\([^_:]*\\)_\\([^_]*\\)_\\([^_]*\\)>" nil t)
      (setq textid (match-string 1))
      (setq page (match-string 3))
      (setq line 0)
      (while (and
	      (< (point) p )
	      (re-search-forward "¶" (point-max) t))
	(setq line (+ line 1)))
      (concat textid ":" page (int-to-string line)))))

(defun mandoku-get-coll (filename)
"find the collection of the file"
(car (cdr (cdr (cdr (cdr (cdr (split-string filename "/")))))))
)

(defun mandoku-cit-format (location)
;; FIXME imlement citation formats for mandoku
  (format "%s %s" (mandoku-get-title)  location)
)


(defun mandoku-textid-to-filename (coll textid page)
"given a textid, a collection id and a page, return the file that contains this page"
(funcall (intern (concat "mandoku-" coll "-textid-to-file")) textid page))




;; mandoku-view-mode

(defvar mandoku-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" 'view-mode)
    (define-key map "a" 'redict-get-line)
         map)
  "Keymap for mandoku-view mode"
)


(define-derived-mode mandoku-view-mode org-mode "mandoku-view"
  "a mode to view mandoku files
  \\{mandoku-mode-map}"
  (setq case-fold-search nil)
  (setq header-line-format (mandoku-header-line))
  (set (make-local-variable 'org-startup-folded) 'showeverything)
  (set (make-local-variable 'tab-with) 30)
  (mandoku-hide-p-markers)
  (add-to-invisibility-spec 'mandoku)
;  (view-mode)
)

(defun mandoku-header-line ()
  (let* ((fn (file-name-sans-extension (file-name-nondirectory (buffer-file-name ))))
	 (textid (car (split-string fn "_"))))
    (list 
     (concat " " textid " " (mandoku-get-title)  ", " (mandoku-get-juan) " -  ")
     '(:eval  (mapconcat 'identity (mandoku-get-outline-path) " / "))
     " "
     '(:eval (mandoku-position-at-point-internal))
     )
     ))


;(setq mandoku-hide-p-re "\\(?:<[^>]*>\\)\\|¶\n\\|¶")
;(setq mandoku-hide-p-re "\\(?:<[^>]*>\\)\\|¶")
(setq mandoku-hide-p-re "\\(<pb\\)\\([^_]+_[^_]+_\\)\\([^>]+>\\)\\|¶\\|&\\([^;]+\\);")
(defun mandoku-hide-p-markers ()
  "add overlay 'mandoku to hide/show special characters "
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward mandoku-hide-p-re nil t)
	(if (match-beginning 2)
	    (overlay-put (make-overlay (- (match-beginning 2) 2) (match-end 2)) 'invisible 'mandoku)
	  (if (match-beginning 1)
	      (overlay-put (make-overlay (match-beginning 1) (match-end 1)) 'invisible 'mandoku)
	    (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'invisible 'mandoku)))
))))



(define-key mandoku-mode-map
             "C-ce" 'view-mode)




(add-hook 'org-execute-file-search-functions 'mandoku-execute-file-search)

;; formatting

;; (defun mandoku-format-file (file)
;; (interactive)
;; (with-current-buffer
;; (goto-char (point-min))
;; (while (re-search-forward "。\\([^¶\n\t]\\)" nil t)
;;   (replace-match "。
;; " (match-data))
;; ))

(defun mandoku-index-sort-pre ()
"sort the result index by the preceding string, this has been saved in the property PRE"
(interactive)
(save-excursion
  (mark-whole-buffer)
  (setq buffer-read-only nil)
  (org-sort-entries t ?r nil nil "PRE")
  (hide-sublevels 2)
)
)


(defun mandoku-closest-elm-in-seq (n seq)
  "returns the closest element which is larger or equal to n in sequence seq "
   (let ((pair (loop with elm = n with last-elm
                  for i in seq
                  if (eq i elm) return (list i)
                  else if (and last-elm (< last-elm elm) (> i elm)) return (list last-elm i)
                  do (setq last-elm i))))
     (if (> (length pair) 1)
         (if (< (- n (car pair)) (- (cadr pair) n))
             (car pair) (cadr pair))
         (car pair))))


(defun mandoku-format-on-punc ( rep)
  "Formats the text from point to the end, splitting at punctuation and other splitting points."
;  (interactive "s")
  (save-match-data
    (while (re-search-forward mandoku-punct-regex-post nil t)
      (if (or (looking-at "¶?[
]") (org-at-heading-p) (org-at-comment-p))
	  nil
	(replace-match (concat (match-string 1)  (match-string 2) rep)))
      (if (looking-at "¶?[	]")
	  (forward-line 1)
	(forward-char 1))
      )))

(defun mandoku-pre-format-on-punc (rep)
  "hallo"
  (save-match-data
    (while (re-search-forward mandoku-punct-regex-pre nil t)
      (if (or (org-at-heading-p) (org-at-comment-p))
	  nil
	(replace-match (concat (match-string 1) rep (match-string 2)))
	(forward-char 1)
	))))

(defun mandoku-format-with-p ()
  "Formats the whole file, adding the line marker to the end of the line"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (looking-at "#")
      (forward-line 1))
    (mandoku-format-on-punc "¶
")
    (goto-char (point-min))
    (while (looking-at "#")
      (forward-line 1))
    (mandoku-pre-format-on-punc "¶
")))

(defun mandoku-format ()
  "Formats the whole file"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (looking-at "#")
      (forward-line 1))
    (mandoku-format-on-punc "
")
    (goto-char (point-min))
    (while (looking-at "#")
      (forward-line 1))
    (mandoku-pre-format-on-punc "
")
))

(defun mandoku-format-add-p-numbers ()
  "For texts without page numbers, add paragraph numbers as a substitute"
  (interactive)
  (save-excursion
    (let ((cnt 0)
	  ;; this assumes a naming convention txtid_<nnn>.txt
	  (txtfn (car (split-string (file-name-nondirectory (buffer-file-name)) "\\.")))
	  (be (mandoku-get-baseedition)))
    (goto-char (point-min))
    (forward-paragraph 1)
    (while (not (eobp))
      (setq cnt (+ cnt 1))
      (insert (concat "<pb:" be "_" txtfn "-" (int-to-string cnt) "a>
"))
      (forward-paragraph 1)))))

(defun mandoku-annotate (beg end)
  (interactive "r")
;  (save-excursion
  (let ((term (replace-regexp-in-string "\\(?:<[^>]*>\\)?¶?" ""
					(buffer-substring-no-properties beg end) )))
    (forward-line)
    (beginning-of-line)

    (if (looking-at ":zhu:")
	(progn
	  (re-search-forward ":END:")
	  (beginning-of-line)
	  (insert term " [" (chw-text-get-pinyin term) "] \n" )
	  (previous-line))
      (progn
	(insert ":zhu:\n \n:END:\n")
	(previous-line 2)
	(beginning-of-line)
	(insert term " [" (chw-text-get-pinyin term) "]" )))

    (beginning-of-line)
    (deactivate-mark)
;    (lookup-word)
))


(defun mandoku-img-to-text (arg)
  "when looking at an image, try to find the corresponding text location"
  (interactive "P")
  (let* ((pb (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
         (this (split-string pb "_")))
	  (if (file-exists-p (concat mandoku-text-dir "dzjy/" (elt this 1) "/" (elt this 1 ) ".txt"))
	      (find-file-other-window (concat mandoku-text-dir "dzjy/" (elt this 1) "/" (elt this 1 ) ".txt"))
	    (find-file-other-window (concat mandoku-text-dir "dzjy-can/" (elt this 1) "/" (elt this 1 ) ".txt")))
	  (goto-char (point-min))
	  (message pb)
	  (search-forward (concat "<pb:" pb))))

(defun mandoku-string-remove-all-properties (string)
  (condition-case ()
      (let ((s string))
	(set-text-properties 0 (length string) nil string)
	s)
    (error string)))


(defun mandoku-get-baseedition ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward ": BASEEDITION \\(.*\\)" (point-max) t)
      (mandoku-string-remove-all-properties (match-string 1)))))



(defun mandoku-get-title ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+TITLE: \\(.*\\)" (point-max) t)
      (car (last (split-string (mandoku-string-remove-all-properties  (match-string 1)) " ")))  )))
      
;;the mode for mandoku-index
(defvar mandoku-index-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" 'view-mode)
    (define-key map " " 'View-scroll-page-forward)
    (define-key map "t" 'manoku-index-no-filter)
    (define-key map "s" 'mandoku-index-sort-pre)
         map)
  "Keymap for mandoku-index mode"
)

(define-derived-mode mandoku-index-mode org-mode "mandoku-index-mode"
  "a mode to view Mandoku index search results
  \\{mandoku-index-mode-map}"
  (setq case-fold-search nil)
  (set (make-local-variable 'tab-with) 24)
  (set (make-local-variable 'org-startup-folded) 'overview)
;  (toggle-read-only 1)
;  (view-mode)
)



;; (defun mandoku-read-titletable (filename tablename) 
;;   "reads a titles table"
;;   (when (file-exists-p filename)
;;     (if (> (hash-table-count tablename) 0)
;;       (setq tablename (make-hash-table :test 'equal))
;;       (put 'tablename :filename filename)
;;       (with-temp-buffer
;;         (let ((coding-system-for-read 'utf-8)
;;               textid)
;;           (insert-file-contents filename)
;;           (goto-char (point-min))
;;           (while (re-search-forward "^\\([a-z0-9]+\\)\s+\\([^\s\n]+\\)" nil t)
;; 	    (puthash (match-string 1) (match-string 2) tablename)))))))

;;[2012-02-28T08:26:29+0900]
(defun mandoku-get-heading (&optional n)
  (interactive "p")
 (car (split-string (car (org-get-outline-path)) "\t" )))

(defun mandoku-display-heading (&optional n)
  (interactive "p")
(message (mandoku-get-heading)))

(defun mandoku-get-juan ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+PROPERTY: JUAN \\(.*\\)" (point-max) t)
      (mandoku-string-remove-all-properties (match-string 1)))))

(defun mandoku-get-vol ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+PROPERTY: VOL \\(.*\\)" (point-max) t)
      (mandoku-string-remove-all-properties (match-string 1)))))


(defun mandoku-page-at-point ()
  (interactive)
  (save-excursion
    (let ((p (point)))
      (re-search-backward "<pb:" nil t)
      (re-search-forward "\\([^_]*\\)_\\([^_]*\\)>" nil t)
      (setq textid (match-string 1))
      (setq page (match-string 2))
      (setq line 0)
      (while (and
	      (< (point) p )
	      (re-search-forward "¶" (point-max) t))
	(setq line (+ line 1)))
      (format "%s%2.2d" page line))))


(defun mandoku-get-subtree ()
  (interactive)
  (org-copy-subtree) 
  (kill-append (concat "\n(巻" (mandoku-get-juan) ", " (mandoku-get-heading) ", p" (mandoku-page-at-point) ")") nil ) ) 

(defun mandoku-get-line (&optional left)
;  (interactive)
;  (message 
   (car (split-string (buffer-substring-no-properties (point-at-bol) (point-at-eol)) "	")))


(easy-menu-define mandoku-md-menu mandoku-mode-map "Mandoku menu"
  '("Mandoku"
    ("Browse"
     ["Show Catalog" mandoku-show-catalog t]
     )
    ["Search" mandoku-search t]
    ("Versions"
     ["Switch versions" mandoku-switch-version nil]
     ["Master" mandoku-switch-to-master nil]
     ["New version" mandoku-new-version nil]
     )
    ("Maintenance"
     ["Update installed texts" mandoku-update nil]
     ["Add repository" mandoku-setting nil]
     )
))     

(provide 'mandoku)

;; end of file mandoku.el
