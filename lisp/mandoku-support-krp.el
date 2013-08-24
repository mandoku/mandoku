;;support file for the DIV texts.
;; currently there are four subsections s1 s2 s3 s4, they are accessed through different functions
(setq mandoku-krp-titles nil)
(setq mandoku-krp-titletable (expand-file-name (concat mandoku-meta-dir "div-titles.txt")))

(defun mandoku-krp-page-to-image (locid)
  "given a location id, returns the path of the image"
  (let* ((textid (car (split-string locid ":" )))
	 (vol (substring textid 0 3))
	 (page (car (cdr (split-string locid ":" )))))
    (concat (substring vol 0 1) "/" vol "/" (substring page 0 2) "/"
	    vol "-" (substring page 0 4) ".tif")))




(defun mandoku-krp-vol-page (vol page)
(interactive "sPlease enter the volume, including a letter (T X J H W), eg T51: \nsPlease enter the page with optional line number like 439a12: ")
(org-mandoku-open (concat "div: " vol ":"  page)))

(defun mandoku-krp-parse-location (loc)
"parse the location"
(split-string loc "[:_]"))


(defun mandoku-krp-textid-to-title (textid page)
  "returns the title of the text"
  ;; read the table if necessary
  (mandoku-krp-read-titletable)
  (list textid (gethash textid mandoku-krp-titles))
  )

(defun mandoku-krp-textid-to-file (textid page)
  "Textids of the form QTW and page strings like 0060-142 are used to find the right file"
  (let* ((sp (split-string page ":"))
	 (sec (if (string-match "-" (car sp))
		  (car (split-string (car sp) "-"))
		"001"))
	 (subcoll (if (string-match "ZB" textid)
		      (substring textid 0 4)
		    "tls")))
    (concat subcoll "/" textid "/" textid "_" sec ".txt")))


;; (defun mandoku-krp-vol-page-to-file (subcoll textnum sec)
;;   "converts the text id and page to a file name.  Page is numeric with the ending abc as 123"
;;   (let ((txtid (concat (upcase subcoll) "n" (format "%4.4d" textnum)))) 
;;     (concat (downcase subcoll) "/" txtid "/" txtid "-" (format "%3.3d" sec) ".txt")
;;     ))


(defun mandoku-krp-vol-page-to-title  (subcoll vol page)
  (list subcoll (concat vol ":" page))
)


(defun mandoku-krp-read-titletable () 
"read the titles table"
  (when (file-exists-p mandoku-krp-titletable)
    (unless mandoku-krp-titles
      (setq mandoku-krp-titles (make-hash-table :test 'equal))
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8)
              textid)
          (insert-file-contents mandoku-krp-titletable)
          (goto-char (point-min))
          (while (re-search-forward "^\\([a-z0-9]+\\)	\\([^	]+\\)" nil t)
	     (puthash (match-string 1) (match-string 2) mandoku-krp-titles)))))))
