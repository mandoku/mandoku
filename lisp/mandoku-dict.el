;; access to remote dictionary on mdserv
;; cwittern@gmail.com [2013-09-05T21:27:20+0900]
;; dict format:
;; loc-x = location keys
;; def-x = definition keys
;; pron-x = pronounciation keys
;; all keys will be sorted lexically
(require 'mandoku)

(defvar mandoku-dict-regex "<[^>]*>\\|[　-㄀＀-￯\n¶]+\\|\t[^\n]+\n")
(defvar mandoku-dict-url "http://www.kanripo.org/api/v1.0" )

(defvar mandoku-dict-img-dir nil) 


(defun mandoku-dict-procline (inp)
  "parse the string and repetitevely call the dictionary"
  (let ((result-buffer (get-buffer-create "*Dict Result*"))
	(the-buf (current-buffer))
	(pos    (mandoku-position-at-point)))
    (set-buffer result-buffer)
    (setq buffer-file-name nil)
    (read-only-mode 0)
    (erase-buffer)
    (insert "* " (if pos pos "")  "\n")
    (url-insert-file-contents (concat mandoku-dict-url "/procline?query=" (replace-regexp-in-string "&" "$" inp)))
    (mandoku-dict-mode)
    (hide-sublevels 2)
    (goto-char (point-min))
    (switch-to-buffer-other-window result-buffer t)))

;;;###autoload
(defun mandoku-dict-mlookup (arg)
  (interactive "P")
  (let* (
	 (regionp (org-region-active-p))
	 (beg (if regionp (region-beginning) (point)))
	 (end (if regionp (region-end))))
    (if regionp
	(mandoku-dict-procline (buffer-substring-no-properties beg end))
      (mandoku-dict-procline (mandoku-get-line))
)))


(defun mandoku-dict-get-next-line ()
  "display the entries for the next line in the Dict Buffer"
  (interactive)
  (if (equal (buffer-name) "*Dict Result*")
      (other-window 1))
  (forward-line 1)
  (mandoku-dict-procline (mandoku-get-line))
)

(defun mandoku-dict-get-prev-line ()
  "display the entries for the next line in the Dict Buffer"
  (interactive)
  (if (equal (buffer-name) "*Dict Result*")
      (other-window 1))
  (forward-line -1)
  (mandoku-dict-procline (mandoku-get-line))
)

(defun mandoku-dict-repeat-line ()
  "display the entries for the next line in the Dict Buffer"
  (interactive)
  (if (equal (buffer-name) "*Dict Result*")
      (other-window 1))
  (mandoku-dict-procline (mandoku-get-line)))



;; mandoku-dict-view-mode

(defvar mandoku-dict-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" 'view-mode)
    (define-key map " " 'View-scroll-page-forward)
    (define-key map "r" 'mandoku-dict-repeat-line)
    (define-key map "n" 'mandoku-dict-get-next-line)
    (define-key map "p" 'mandoku-dict-get-prev-line)
    (define-key map "p" 'mandoku-dict-get-prev-line)
         map)
  "Keymap for mandoku-dict-view mode"
)

(define-derived-mode mandoku-dict-mode org-mode "mandoku-dict-mode"
  "a mode to view dictionary files
  \\{mandoku-dict-mode-map}"
  (setq case-fold-search nil)
  (set (make-local-variable 'org-startup-folded) 'overview)
  (read-only-mode 0)
;  (view-mode)
)


;(global-set-key (kbd "C-c g") 'mandoku-dict-get-line)
(global-set-key (kbd "M-a") 'mandoku-dict-get-line) ; was backward-sentence.
(global-set-key (kbd "M-s a") 'mandoku-dict-get-line)
;(global-set-key (kbd "M-s n") 'mandoku-dict-get-next-line)
;(global-set-key (kbd "M-s p") 'mandoku-dict-get-prev-line)
;(global-set-key (kbd "M-s r") 'mandoku-dict-repeat-line)

(define-key mandoku-dict-mode-map
             "n" 'mandoku-dict-get-next-line)
(defun mandoku-dict-highlight (state)
  (interactive)
  (cond 
   ((and (eq major-mode 'mandoku-dict-mode)
	     (memq state '(children subtree)))
    (save-excursion
    (let ((hw (progn 
		(while (> (org-current-level) 2)
		  (outline-previous-heading))
		(car (split-string  (org-get-heading))))))
      (hi-lock-mode 1)
      (highlight-regexp hw))))
   ((and (eq major-mode 'mandoku-dict-mode)
	     (memq state '(overview folded)))
      (hi-lock-mode 0))))

(add-hook 'org-cycle-hook 'mandoku-dict-highlight) 

;; add link type dic
(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters "dic"
                 :follow #'org-dic-open
                 )
  (org-add-link-type "dic" 'org-dic-open))

; (org-add-link-type "dic" 'org-dic-open)
;(add-hook 'org-store-link-functions 'org-dic-store-link)

(defun org-dic-open (link)
  "Display the dictionary page, either local or remotely"
  (if mandoku-dict-img-dir
      (org-open-file (concat mandoku-dict-img-dir  link))
;    (browse-url (concat mandoku-dict-url "/static/dic/" link))))
    (browse-url (concat mandoku-dict-url "/dicpage/" link))))
	 
(provide 'mandoku-dict)


;;; mandoku-dict.el ends here
