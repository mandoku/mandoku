;; annotating for mandoku files
;; cwittern@gmail.com [2014-06-06T13:01:38+0900]

(require 'mandoku)

(defvar mandoku-annot-dir (expand-file-name  (concat mandoku-base-dir "notes/")))


(defun mandoku-annot-scan ()
(interactive)
(save-excursion 
  (goto-char (point-min))
  (while (re-search-forward "" nil t)
    


(provide 'mandoku-annot)
