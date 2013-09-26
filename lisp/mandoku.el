;; mandoku.el   -*- coding: utf-8 -*-
;; created [2001-03-13T20:32:32+0800]  (as smart.el)
;; renamed and refactored [2010-01-08T17:01:43+0900]
(require 'org)
;; the root of the mandoku hierarchy
(defvar mandoku-base-dir (expand-file-name  "~/krp/"))
(defvar mandoku-do-remote nil)

(defvar mandoku-lisp-dir nil)
;(defvar mandoku-subdirs (list "text" "images" "index" "meta" "temp" "system" "work"))
(defvar mandoku-text-dir (expand-file-name (concat mandoku-base-dir "text/")))
(defvar mandoku-image-dir nil)
(defvar mandoku-index-dir nil)
(defvar mandoku-meta-dir (expand-file-name  (concat mandoku-base-dir "meta/")))
(defvar mandoku-temp-dir (expand-file-name  (concat mandoku-base-dir "temp/")))
(defvar mandoku-sys-dir (expand-file-name  (concat mandoku-base-dir "system/")))

(defvar mandoku-string-limit 10)
(defvar mandoku-index-display-limit 200)
;; Defined somewhere in this file, but used before definition.
(defvar mandoku-md-menu)

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

(defvar mandoku-initialized nil)


(defvar mandoku-file-type ".txt")
;;we skip: 》《 
(defvar mandoku-punct-regex-post "\\([^
]\\)\\([　-〇〉」』】〗〙〛〕-㄀︀-￯)]+\\)")
(defvar mandoku-punct-regex-pre "\\([^
]\\)\\([(〈「『【〖〘〚〔]+\\)")

(defvar mandoku-kanji-regex "\\([㐀-鿿𠀀-𪛟]+\\)")

(defvar mandoku-regex "<[^>]*>\\|[　-㄀＀-￯\n¶]+\\|\t[^\n]+\n")

;; Add this since it appears to miss in emacs-2x
(or (fboundp 'replace-in-string)
    (defun replace-in-string (target old new)
      (replace-regexp-in-string old new  target)))

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
		   (org-map-entries 'mandoku-get-header-item "+LEVEL<=2"))))
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


(defun mandoku-grep (beg end)
  (interactive "r")
  (mandoku-grep-internal (buffer-substring-no-properties beg end)))

;;;###autoload
(defun mandoku-show-catalog ()
  (interactive)
  (unless mandoku-initialized
    (load "mandoku-init"))
  (find-file mandoku-catalog)
)

;;;###autoload
(defun mandoku-search-text (search-for)
  (interactive 
   (let ((search-for (mapconcat 'char-to-string (mandoku-next-three-chars) "")))
     (list (read-string "Search for: " search-for))))
  (unless mandoku-initialized
    (progn (load "mandoku-init")))
  (mandoku-grep-internal (mandoku-cut-string search-for))
  )

(defun mandoku-next-three-chars ()
  (save-excursion
    (mandoku-remove-nil-recursively
     (list
     (char-after)
     (progn (mandoku-forward-one-char) (char-after))
     (progn (mandoku-forward-one-char) (char-after))
     (progn (mandoku-forward-one-char) (char-after))
     (progn (mandoku-forward-one-char) (char-after))
     (progn (mandoku-forward-one-char) (char-after))
))))


(defun mandoku-forward-one-char ()
	"this function moves forward one character, ignoring punctuation and markup
One character is either a character or one entity expression"
;	(interactive)
	(ignore-errors
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
))

(defun mandoku-forward-n-characters (num)
	(while (> num 0)
		(setq num (- num 1))
		(message (number-to-string num))
		(mandoku-forward-one-char))
)

(defun mandoku-index-get-search-string ()
  "Get the search-string from the Index Buffer"
  (save-excursion
    (goto-char (point-min))
    (search-forward "
* " nil t)
    (car (split-string  (org-get-heading))))
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
      (set (make-local-variable 'mandoku-search-string) search-string)
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


(defun mandoku-tabulate-index-buffer (index-buffer)
  (switch-to-buffer-other-window index-buffer t)
  (let ((tabhash (make-hash-table :test 'equal))
	(m))
    (goto-char (point-min))
    (while (re-search-forward "^\\([^	]+\\)	\\([^	
]+\\)" nil t)
      (setq m (substring (match-string 2) 0 4))
      (if (gethash m tabhash)
	  (puthash m (+ (gethash m tabhash) 1) tabhash)
	(puthash m 1 tabhash)))
    tabhash))
;    (setq myList (mandoku-hash-to-list tabhash))))

    ;; (set-buffer result-buffer)
    ;; (dolist (x   
    ;; 	     (sort myList (lambda (a b) (string< (car a) (car b)))))
    ;;   (insert (format "* %s\t%s\t%d\n" (car x) (gethash (car x) mandoku-subcolls) (car (cdr x)))))))

(defun mandoku-index-insert-tablist (hashtable index-buffer)
  (let ((myList (mandoku-hash-to-list hashtable)))
    (set-buffer result-buffer)
    (dolist (x   
	     (sort myList (lambda (a b) (string< (car a) (car b)))))
      (insert (format "* %s\t%s\t%d\n\n" (car x) (gethash (car x) mandoku-subcolls) (car (cdr x)))))))

(defun mandoku-hash-to-list (hashtable)
  "Return a list that represent the HASHTABLE."
  (let (myList)
    (maphash (lambda (kk vv) (setq myList (cons (list kk vv) myList))) hashtable)
    myList
  )
)    
(defun mandoku-sum-hash (hashtable)
  "Return the sum of the HASHTABLE's value" 
  (let ((cnt 0))
    (maphash (lambda (kk vv) (setq cnt (+ cnt vv))) hashtable)
    cnt))

(defun mandoku-index-insert-result (search-string index-buffer result-buffer  &optional filter)
  (let ((mandoku-use-textfilter nil)
      	(search-char (string-to-char search-string))
	(mandoku-filtered-count 0))
      (progn
    (switch-to-buffer-other-window index-buffer t)
;;xx      (set-buffer index-buffer)
;; first: sort the result (after the filename)
    (setq buffer-file-name nil)
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
	       (concat "^\\([^,]*\\),\\([^\t]*\\)\t" filter  "\\([^\t \n]*\\)\\(\t[^\n\t ]*\\)?$")
	) nil t )
	(let* (
	       ;;if no subcoll, need to switch the match assignments.
	      (pre (match-string 2))
	      (post (match-string 1))
	      (location (split-string (match-string 3) ":" ))
	      (extra (match-string 8))
	      )
	  (let* ((txtid (concat filter (car location)))
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
;	    (setq mandoku-count (+ mandoku-count 1))
))))))    

(defun mandoku-read-index-buffer (index-buffer result-buffer search-string)
  (let* (
	(mandoku-count 0)
	(mandoku-filtered-count 0)
      	(search-char (string-to-char search-string))
	(tab (mandoku-tabulate-index-buffer index-buffer))
	(cnt (mandoku-sum-hash tab)))
    (if (and (not (= 0 mandoku-index-display-limit)) (> cnt mandoku-index-display-limit))
;    (if nil
	(mandoku-index-insert-tablist tab result-buffer)
      (mandoku-index-insert-result search-string index-buffer result-buffer ""))
      (switch-to-buffer-other-window result-buffer t)
      (goto-char (point-min))
;      (insert (format "There were %d matches for your search of %s:\n"
;       mandoku-count search-string))
      (if (equal mandoku-use-textfilter t)
	  (insert (format "Active Filter: %s , Matches: %d (Press 't' to temporarily disable the filter)\n" 
			  (mapconcat 'mandoku-active-filter mandoku-textfilter-list "")
			  mandoku-filtered-count))
	)
      (insert (format "Location\tMatch\tSource\n* %s (%d/%d)\n"  search-string mandoku-filtered-count cnt))
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

(defun mandoku-meta-textid-to-file (txtid &optional page)
  (let ((repid (car (split-string txtid "[0-9]"))))
;    (concat mandoku-meta-dir repid "/" (substring txtid 0 (+ (length repid) 2)) ".org")))
    (concat mandoku-meta-dir repid "/" (substring txtid 0 (+ (length repid) 1)) ".txt")))


(defun mandoku-get-outline-path ()
  "this includes the first upward heading"
  (if (org-before-first-heading-p)
      (list "")
    (save-excursion
    (let ((olp ))
	  (outline-previous-visible-heading 1)
	  (when (looking-at org-complex-heading-regexp)
	    (push (org-trim
		   (replace-regexp-in-string org-bracket-link-regexp "\\3"
		   (replace-regexp-in-string
		    ;; Remove statistical/checkboxes cookies
		    "\\[[0-9]+%\\]\\|\\[[0-9]+/[0-9]+\\]\\|¶" ""
		    (org-match-string-no-properties 4))))
		  olp))
	  (while (org-up-heading-safe)
	    (when (looking-at org-complex-heading-regexp)
	      (push (mandoku-cut-string 
		     (org-trim
		      (replace-regexp-in-string org-bracket-link-regexp "\\3"
		     (replace-regexp-in-string
		      ;; Remove statistical/checkboxes cookies
		      "\\[[0-9]+%\\]\\|\\[[0-9]+/[0-9]+\\]\\|¶" ""
		      (org-match-string-no-properties 4)))))
		    olp)))
	  olp))))


(defun mandoku-cut-string (s)
  (if (< mandoku-string-limit (length s)  )
      (substring s 0 mandoku-string-limit)
    s))


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
      (if 
	  (re-search-backward "<pb:" nil t)
	  (progn
	    (re-search-forward "\\([^_]*\\)_\\([^_>]*\\)>" nil t)
	    (setq textid (match-string 1))
	    (setq page (match-string 2))
	    (setq line -1)
	    (while (and
		    (< (point) p )
		    (re-search-forward "¶" (point-max) t))
	      (setq line (+ line 1)))
	    (format "%s%s, p%s%2.2d" textid (if (mandoku-get-vol) (mandoku-get-vol) "") (car (cdr (split-string page "-"))) line))
	" -- ")
      )))

(defun mandoku-open-image-at-page ()
  (interactive)
  (let* (
	 (coll (mandoku-get-coll buffer-file-name))
	 (path (concat mandoku-image-dir coll "/"
	   (funcall (intern
		     (concat "mandoku-" coll "-page-to-image"))
		    (mandoku-position-at-point-internal)   ))))
  (find-file-other-window path )))


;; (defun mandoku-position-at-point-internal ()
;;   (interactive)
;;   (save-excursion
;;     (let ((p (point)))
;;       (re-search-backward "<pb:" nil t)
;;       (re-search-forward "\\([^_:]*\\)_\\([^_]*\\)_\\([^_]*\\)>" nil t)
;;       (setq textid (match-string 1))
;;       (setq page (match-string 3))
;;       (setq line 0)
;;       (while (and
;; 	      (< (point) p )
;; 	      (re-search-forward "¶" (point-max) t))
;; 	(setq line (+ line 1)))
;;       (concat textid ":" page (int-to-string line)))))

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

(defvar mandoku-view-mode-map
  (let ((map (make-sparse-keymap)))
;    (define-key map "e" 'view-mode)
;    (define-key map "a" 'redict-get-line)
         map)
  "Keymap for mandoku-view mode"
)


(define-derived-mode mandoku-view-mode org-mode "mandoku-view"
  "a mode to view mandoku files
  \\{mandoku-view-mode-map}"
  (setq case-fold-search nil)
  (setq header-line-format (mandoku-header-line))
  (set (make-local-variable 'org-startup-folded) 'showeverything)
  (set (make-local-variable 'tab-with) 30)
  (mandoku-hide-p-markers)
  (add-to-invisibility-spec 'mandoku)
;  (easy-menu-remove-item org-mode-map (list "Org") org-org-menu)
;  (easy-menu-remove org-tbl-menu)
  (local-unset-key [menu-bar Org])
  (local-unset-key [menu-bar Tbl])
  (easy-menu-add mandoku-md-menu mandoku-view-mode-map)
;  (view-mode)
)

(defun mandoku-toggle-visibility ()
  (interactive)
  (if (member 'mandoku buffer-invisibility-spec)
      (remove-from-invisibility-spec 'mandoku)
    (add-to-invisibility-spec 'mandoku))
  (if (member 'mandoku buffer-invisibility-spec)
      (easy-menu-change
	 '("Mandoku") "Markers"
	 (list  ["Show" mandoku-toggle-visibility t]))
    (easy-menu-change
	 '("Mandoku") "Markers"
	 (list ["Hide" mandoku-toggle-visibility t])))
  (redraw-display)
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



(define-key mandoku-view-mode-map
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
;  (set-text-properties 0 (length string) nil string))
  (condition-case ()
      (let ((s string))
	(set-text-properties 0 (length s) nil s) s)
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
  (set-variable 'tab-with 24 t)
;  (set (make-local-variable 'tab-with) 24)
  (set (make-local-variable 'org-startup-folded) 'overview)
;  (toggle-read-only 1)
;  (view-mode)
)


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

;; (easy-menu-define mandoku-md-menu org-mode-map "Mandoku menu"
;;   '("BK-MDA"
;;     ["Test" (lambda () (interactive) (insert "test!")) t]
;;     ))
  
(easy-menu-define mandoku-md-menu mandoku-view-mode-map "Mandoku menu"
  '("Mandoku"
    ("Markers"
     ["Show" mandoku-toggle-visibility t])
    ("Browse"
     ["Show Catalog" mandoku-show-catalog t]
     )
    ("Search"
     ["Texts" mandoku-search-text t]
     ["Titles" mandoku-search-titles t]
     ["Dictionary" mandoku-dict-mlookup t]
     )
    ("Versions"
     ["Switch versions" mandoku-switch-version nil]
     ["Master" mandoku-switch-to-master nil]
     ["New version" mandoku-new-version nil]
     )
    ("Maintenance"
     ["Update mandoku" mandoku-update t]
     ["Update installed texts" mandoku-update-texts nil]
     
     ["Add repository" mandoku-setting nil]
     )
))     

;; tab

(defun mandoku-index-tab-change (state)
  (interactive)
  (cond 
   ((and (eq major-mode 'mandoku-index-mode)
	     (memq state '(children subtree)))
    (save-excursion
      (let ((hw 	(car (split-string  (org-get-heading)))))
	(forward-line)
	(if (looking-at "\n")
	    (mandoku-index-insert-result (mandoku-index-get-search-string) (current-buffer) "*temp-buffer*" hw))
	)))
   ((and (eq major-mode 'mandoku-index-mode)
	     (memq state '(overview folded)))
      (hi-lock-mode 0))))

;(add-hook 'org-cycle-hook 'mandoku-index-tab-change) 



(defun mandoku-get-catalog-entries(file search &rest type)
;; have not yet defined search types, this will be parallel to org-agenda-entry-types
;;  (setq type (or type mandoku-search-types))
  (let* ((org-startup-folded nil)
	 (org-startup-align-all-tables nil)
	 (buffer (if (file-exists-p file)
		     (org-get-agenda-file-buffer file)
		   (error "No such file %s" file)))
	 arg results rtn deadline-results)
    (if (not buffer)
	;; If file does not exist, make sure an error message ends up in diary
	(list (format "Mandoku search error: No such catalog-file %s" file))
      (with-current-buffer buffer
	(unless (derived-mode-p 'org-mode)
	  (error "Catalog file %s is not in `org-mode'" file))
	(setq mandoku-cat-buffer (or mandoku-cat-buffer buffer)))
)))

(defun mandoku-remove-nil-recursively (x)
  (if (listp x)
    (mapcar #'mandoku-remove-nil-recursively
            (remove nil x))
    x))
;; catalog etc

(defun mandoku-list-titles(filter)
    (let ((buf (get-buffer-create "*Mandoku Titles*")))
      (with-current-buffer buf
	(mandoku-title-list-mode)
	(set (make-local-variable 'package-menu--new-package-list)
	     new-packages)
	(package-menu--generate nil t))
      ;; The package menu buffer has keybindings.  If the user types
      ;; `M-x list-packages', that suggests it should become current.
      (switch-to-buffer buf)))
;;;###autoload
(defun mandoku-search-titles(s)
  (interactive "sMandoku | Search for title containing: ")
  (let* ((files (mapcar 'cdr mandoku-catalogs-alist ))
	 (buf (get-buffer-create "*Mandoku Titles*"))
	 (type "title")
	 rtn)
    (setq rtn (mandoku-remove-nil-recursively (org-map-entries 'mandoku-get-catalog-entry "+LEVEL=3" files)))
    (with-current-buffer buf
      (mandoku-title-list-mode)
      (setq tabulated-list-entries (mapcar 'mandoku-title-entry rtn))
      (tabulated-list-print) 
      (switch-to-buffer-other-window buf))
;    (setq results (append results rtn))
;    results))
    ))

(defun mandoku-title-entry (entry)
  "Fromat the entry for  `tabulated-list-entries'.
ent has the form ((serial-number title) author dynasty (sn-parent parent) )"
  (let* (
	 (sn (caar entry))
	 (title (car (cdr (car entry))))
	 (resp (or (nth 1 entry) ""))
	 (dyn (or (nth 2 entry) ""))
	 (lei (concat (substring (car (nth 3 entry)) 2) (car (cdr (nth 3 entry))))))
    (list (cons sn nil)
	  (vector lei sn title dyn resp ))
))  

(defun mandoku-search-resp(s)
  (let* ((files (mapcar 'cdr mandoku-catalogs-alist ))
	 results rtn)
    (setq rtn (mandoku-remove-nil-recursively (org-map-entries 'mandoku-get-catalog-entry "+LEVEL=3" files)))
    (setq results (append results rtn))
    results))

(defun mandoku-get-catalog-entry ()
  "let bind the search-string as var s"
  (let* ((begol (save-excursion (beginning-of-line) (search-forward " ") ))
;	 (parent (save-excursion (org-up-heading-safe) (mandoku-get-header-item )))
	 (rtn (mandoku-get-header-item)))

    (if (equal type "title")
	(if (string-match s (car (cdr rtn)))
	    (list 
	     rtn 
	     (or (org-entry-get begol "RESP" ) "")
	     (or (org-entry-get begol "DYNASTY" )   "")
	     (save-excursion (org-up-heading-safe) (mandoku-get-header-item ))
	     ))
      (if (equal type "resp")
	  (if (string-match s (org-entry-get begol "RESP" ))
	    (list rtn (org-entry-get begol "RESP" ) (org-entry-get begol "DYNASTY" ) ))
	(if (equal type "dyn")
	  (if (string-match s (org-entry-get begol "DYNASTY" ))
	    (list rtn (org-entry-get begol "RESP" ) (org-entry-get begol "DYNASTY" ) ))
      )))))

;; this works
;; (setq r (mandoku-remove-nil-recursively (let ((s "周易"))
;;   (org-map-entries 'mandoku-get-catalog-entry "+DYNASTY=\"宋\"" files))))

(define-derived-mode mandoku-title-list-mode tabulated-list-mode "Title List"
  "Major mode for browsing a list of titles.
Letters do not insert themselves; instead, they are commands.
\\<mandoku-title-list-mode-map>
\\{mandoku-title-list-mode-map}"
  (setq tabulated-list-format [("Bu" 8 nil)
			       ("Number" 12 t)
			       ("Title" 35 t)
			       ("Dynasty"  10 mandoku-title-menu--dyn-predicate)
			       ("Author" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Title" nil))
  (tabulated-list-init-header))

(defun mandoku-title-menu--dyn-predicate (A B)
  (let ((dA (aref (cadr A) 3))
	(dB (aref (cadr B) 3)))
  (string< dA dB)))

(defvar mandoku-title-list-mode-map 
  (let ((map (make-sparse-keymap))
;	(menu-map (make-sparse-keymap "Catalog")))
	)
;    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "t" 'mandoku-title-list-goto-text)
    (define-key map "c" 'mandoku-title-list-goto-catalog)
    (define-key map "i" 'mandoku-title-list-goto-catalog)
;    (define-key map "[RET]" 'mandoku-title-list-goto-text)
    map)
  "Local keymap for `mandoku-title-list-mode' buffers.")

(define-key mandoku-title-list-mode-map  "t" 'mandoku-title-list-goto-text)
(define-key mandoku-title-list-mode-map "c" 'mandoku-title-list-goto-catalog)
(define-key mandoku-title-list-mode-map "i" 'mandoku-title-list-goto-catalog)


(defun mandoku-title-list-goto-text ()
  (interactive)
  (let* ((id (tabulated-list-get-id))
	 (entry (and id (assq id tabulated-list-entries))))
    (if entry
;; this is where I need to implement the jump
	(org-mandoku-open (concat  (aref (cadr entry) 1) ".org"))
      "")))

(defun mandoku-title-list-goto-catalog ()
  (interactive)
  (let* ((id (tabulated-list-get-id))
	 (entry (and id (assq id tabulated-list-entries))))
    (if entry
;; this is where I need to implement the jump
	(org-mandoku-open (concat "meta:" (aref (cadr entry) 1) ":10"))
      "")))


;; maintenance

(defun mandoku-update()
  (interactive)
  (let* ((package   "mandoku")
	 (buf       (switch-to-buffer "*mandoku bootstrap*"))
	 (git       (or (executable-find "git")
			(error "Unable to find `git'")))
	 (default-directory (concat mandoku-base-dir package "/lisp"))
	 (process-connection-type nil)   ; pipe, no pty (--no-progress)

	   ;; First clone mandoku
	 (status
	  (call-process
	   git nil `(,buf t) t "pull" "origin" "-v" )))

	(unless (zerop status)
	  (error "Couldn't update %s from the remote Git repository." (concat mandoku-base-dir package)))
	(let ((byte-compile-warnings nil)
	      ;; Byte-compile runs emacs-lisp-mode-hook; disable it
	      emacs-lisp-mode-hook)
	  (byte-recompile-directory default-directory 0))))


(provide 'mandoku)

;; end of file mandoku.el
