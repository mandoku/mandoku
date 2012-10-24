;; 
;; custom links to mandoku files from org
;; [2010-02-13T14:55:27+0900]  cwittern@gmail.com
;; mandoku:cbeta:T52:0850a3::與道冥
;; [2012-10-19T20:11:32+0900]  created for kanripo from mandoku example
;; kanripo:filename:0850a3::與道冥

(require 'org)
(require 'mandoku)

(org-add-link-type "kanripo" 'org-kanripo-open)
(add-hook 'org-store-link-functions 'org-kanripo-store-link)
(defvar kanripo-base-dir (expand-file-name  "/Users/Shared/md/"))
(defun org-kanripo-open (link)
  "Open the text (and optionally go to indicated  position) in LINK."

  ;; need to get the file name and then call mandoku-execute-file-search
  (let ((filename (car (split-string link "%")))
      (page (car (cdr (split-string link "%"))))
;      (page (replace-in-string (car (cdr (cdr (split-string link ":")))) "_" ":" ))
      (src (car (cdr (split-string link "::")))))
  (message (format "%s" page))
    (org-open-file (concat kanripo-base-dir filename ".txt") t nil 
   (if src 
       (concat page "::" src)
     page)
    )))



(defun org-kanripo-store-link ()
  "if we are in mandoku-view-mode, or visiting a mandoku file, then store link"
  (when (eq major-mode 'mandoku-view-mode)
  ;; this is a mandoku file, make link
    (save-excursion
      (let* ((coll (mandoku-get-coll (buffer-file-name)))
	     (location (mandoku-position-at-point))
	     (link (concat "kanripo:" coll ":" location))
	     (description (mandoku-cit-format  location)))
	(org-store-link-props
	 :type "kanripo"
	 :link link
	 :description description)))))

(provide 'org-kanripo)

;;; org-kanripo.el ends here


