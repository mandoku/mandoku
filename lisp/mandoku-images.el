;; mandoku-images.el   -*- coding: utf-8 -*-
;; created [2014-02-28T09:23:45+0900]
;; remote image

(defun mandoku-open-remote-image (filename src page)
  (let* ((buffer (car (last (split-string filename "/"))))
	 (rep (car (split-string buffer "[0-9]")))
	 (rep-url (car (cdr (assoc rep mandoku-repositories-alist ))))
	 )
    (with-current-buffer (get-buffer-create buffer)
      (url-insert-file-contents (concat rep-url "/getfile?filename=" filename )
			      (lambda (status) (switch-to-buffer buffer))))
    (switch-to-buffer buffer)
    (setq buffer-file-name (concat mandoku-temp-dir buffer))
    (unless (file-directory-p mandoku-temp-dir)
      (make-directory mandoku-temp-dir t))
    (save-buffer)
    (mandoku-view-mode)
    (mandoku-execute-file-search 
	 (if src 
	     (concat page "::" src)
	   page))
    ))





;; specialized functions for specific editions:

(defun mandoku-t-page-to-image (locid)
  "given a location id, returns the path of the image in the Taisho"
  (let ((textid (nth 0 locid))
	(vol (concat (nth 1 locid) (mandoku-get-vol)))
	(pg (car (cdr (split-string (nth 2 locid) "-")))))
    (concat "cbeta/" (nth 1 locid ) "/" vol "/" (substring pg 0 2) "/"
	    vol "-" (substring pg 0 4) ".tif")))

(defun mandoku-x-page-to-image (locid)
  "given a location id, returns the path of the image in the Xuzangjing"
  (let ((textid (nth 0 locid))
	(vol (concat (nth 1 locid) (mandoku-get-vol)))
	(pg (car (cdr (split-string (nth 2 locid) "-")))))
    (concat "cbeta/" (nth 1 locid ) "/" vol "/" (substring pg 0 2) "/"
	    vol "-" (substring pg 0 4) ".tif")))


(defun mandoku-tkd-page-to-image (locid)
  "given a location id, returns the path of the image in the Tripitaka Koreana Dongguk reprint"
  (let (
	(vol (car (split-string (nth 2 locid) "-")))
	(pg (car (cdr (split-string (nth 2 locid) "-")))))
    (concat "TK/" vol "-tif/" (substring pg 0 2) "00-" (substring pg 0 2) "99/" vol "_" (substring pg 0 4) "_"  (upcase (substring pg -1)) ".TIF")))




;; end of file mandoku-images.el
