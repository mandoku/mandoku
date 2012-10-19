;; 
;; custom links to mandoku files from org
;; [2010-02-13T14:55:27+0900]  cwittern@gmail.com
;; mandoku:cbeta:T52:0850a3::與道冥
;; [2012-10-19T20:11:32+0900]  created for kanrep from mandoku example
;; kanrep:filename:0850a3::與道冥

(require 'org)
(require 'mandoku)

(org-add-link-type "kanrep" 'org-kanrep-open)
(add-hook 'org-store-link-functions 'org-kanrep-store-link)
(defvar kanrep-base-dir (expand-file-name  "/Users/Shared/md/"))
(defun org-kanrep-open (link)
  "Open the text (and optionally go to indicated  position) in LINK."

  ;; need to get the file name and then call mandoku-execute-file-search
  (let ((filename (car (split-string link ":")))
      (page (car (cdr (split-string link ":"))))
;      (page (replace-in-string (car (cdr (cdr (split-string link ":")))) "_" ":" ))
      (src (car (cdr (split-string link "::")))))
  (message (format "%s" page))
    (org-open-file (concat kanrep-base-dir filename ".txt") t nil 
   (if src 
       (concat page "::" src)
     page)
    )))



(defun org-kanrep-store-link ()
  "if we are in mandoku-view-mode, or visiting a mandoku file, then store link"
  (when (eq major-mode 'mandoku-view-mode)
  ;; this is a mandoku file, make link
    (save-excursion
      (let* ((coll (mandoku-get-coll (buffer-file-name)))
	     (location (mandoku-position-at-point))
	     (link (concat "kanrep:" coll ":" location))
	     (description (mandoku-cit-format  location)))
	(org-store-link-props
	 :type "mandoku"
	 :link link
	 :description description)))))

(provide 'org-kanrep)

;;; org-kanrep.el ends here


