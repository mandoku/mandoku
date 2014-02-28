;; mandoku-images.el   -*- coding: utf-8 -*-
;; created [2014-02-28T09:23:45+0900]
;; this file holds specialized functions for specific editions. 

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



;; end of file mandoku-images.el
