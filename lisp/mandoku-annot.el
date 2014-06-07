;; annotating for mandoku files
;; cwittern@gmail.com [2014-06-06T13:01:38+0900]

(require 'mandoku)

(defvar mandoku-annot-dir (expand-file-name  (concat mandoku-base-dir "notes/")))

(defvar mandoku-annot-regex "^\\([^:\n ]+\\) *\\([^:]+\\)::\\(.*?\\)$") 

(defun mandoku-location-put (&rest stuff)
  "Add properties to the capture property list `mandoku-location-plist'"
  (while stuff
    (setq mandoku-location-plist (plist-put mandoku-location-plist
				       (pop stuff) (pop stuff)))))


(defun mandoku-annot-scan (&optional annot-dir)
  (interactive)
  (let (f1 type p olp)
    (save-excursion 
      (goto-char (point-min))
      (while (re-search-forward mandoku-annot-regex nil t)
	(setq f1 (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
	(setq mandoku-location-plist :context f1)
	(mandoku-location-put 
	 :location (concat (mandoku-position-with-char  
			    (search-backward f1))
			   "+" (number-to-string (length f1) ))
	 :olp (mandoku-get-outline-path p)
	 :type (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
	(mandoku-annot-insert)
	))))      
    

(defun mandoku-annot-insert ()
)

(provide 'mandoku-annot)
