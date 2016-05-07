;;; helm-charinfo.el --- A helm source for character information
;; -*- coding: utf-8 -*-
;; created [2016-04-15T12:59:30+0900]
;;
;; Copyright (c) 2016 Christian Wittern
;;
;; Author: Christian Wittern <cwittern@gmail.com>
;; URL: https://github.com/cwittern/helm-charinfo
;; Version: 0.01
;; Keywords: convenience
;; Package-Requires: ((emacs "24") (helm "1.7.0") (cl-lib "0.5"))
;; This file is not part of GNU Emacs.

;;; Code:

(require 'helm)

(defcustom helm-charinfo-follow-delay 1
  "Delay before Dictionary summary pops up."
  :type 'number
  :group 'helm-charinfo)
;; todo: get the readings file if not available:
;; http://www.unicode.org/Public/UCD/latest/ucd/Unihan.zip
(defcustom helm-charinfo-unihan-readings
  (car
   (remove nil
	   (cl-mapcar  (lambda (x) (car (file-expand-wildcards x)))
		       (list "/Users/*/src/Unihan/Unihan_Readings.txt" ;;my system :-(
			     (concat mandoku-sys-dir "Unihan_Readings.txt")
			     ;;TODO Add suitable paths for other operating system
			     ))))
  "Location of unihan files."
  :type 'string
  :group 'helm-charinfo)

(defvar helm-charinfo-chartab nil)
(defvar helm-charinfo-selected nil)
(defun helm-charinfo-do-chartab ()
  "Read the Unihan Readings into helm-charinfo-chartab"
  (setq helm-charinfo-chartab (make-hash-table :test 'equal))
  (when (file-exists-p helm-charinfo-unihan-readings)
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8)
	    textid)
	(insert-file-contents helm-charinfo-unihan-readings) ;
	(goto-char (point-min))
	(while (re-search-forward "^\\(U[^
]+\\)\t\\([^\t
]+\\)\t\\([^\t
]+\\)$" nil t)
	  (let ((type (match-string 2))
		(def (match-string 3))
		(unicode (match-string 1))
		tchar)
	    ;; U+5364	kHanyuPinyin	10093.130:xī,lǔ 74609.020:lǔ,xī
	    ;; there could be duplicates? remove them
	    (setq tchar (gethash unicode helm-charinfo-chartab))
	    (if (string= type "kMandarin")
		(puthash unicode (plist-put tchar :kMandarin def)  helm-charinfo-chartab)
	      (if (string= type "kHanyuPinyin")
		  (puthash unicode (plist-put tchar :kHanyuPinyin
					      (split-string (car (split-string (cadr (split-string def ":")) " "))",")
					      )  helm-charinfo-chartab)
		(when (string= type "kDefinition")
		  (puthash unicode (plist-put tchar :kDefinition def)  helm-charinfo-chartab))))
	    ))))))
  
  

(defun helm-charinfo-get-candidates ()
  (unless helm-charinfo-chartab
    (helm-charinfo-do-chartab)
    )
  (let (l x d)
    (maphash (lambda (k v)
	       (setq d (or (plist-get v :kDefinition) ""))
	       (if (dolist (x (plist-get v :kHanyuPinyin))
		     (push (format "%c %-10s %-10s %s" (string-to-number (substring k 2) 16)  x  k d) l ))
		   (if (plist-get v :kMandarin)
		       (push (format "%c %-10s %-10s %s" (string-to-number (substring k 2) 16)  (plist-get v :kMandarin) k d ) l )
		 )))
	     helm-charinfo-chartab)
					;(sort l 'string-lessp)
    l
    ))

(defvar helm-charinfo-source
  (helm-build-sync-source "Charinfo"
    :candidates #'helm-charinfo-get-candidates
    :action '(("Select" . (lambda (candidate)
			    (setq helm-charinfo-selected candidate)))
	       )
    :requires-pattern 1))
  
(defun helm-charinfo (&optional c)
  (interactive)
  (helm :sources 'helm-charinfo-source
	:buffer "*helm dictionary*"
	:input (or c (thing-at-point 'word))))
  
(provide 'helm-charinfo)


