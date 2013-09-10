;; access to remote dictionary on mdserv
;; cwittern@gmail.com [2013-09-05T21:27:20+0900]
;; dict format:
;; loc-x = location keys
;; def-x = definition keys
;; pron-x = pronounciation keys
;; all keys will be sorted lexically
(require 'org)
(require 'cl)
(require 'assoc)

(defvar mandoku-dict-regex "<[^>]*>\\|[　-㄀＀-￯\n¶]+\\|\t[^\n]+\n")
(defvar mandoku-dict-url (concat "http://dic.kanripo.org" "/dic?query="))
;; pron-kanwa-01 for kanwa!
;(defvar mandoku-dict-pron "pron-pinyin-01")
;(defvar mandoku-dict-prefdic "def-abc-01-01")
(defvar mandoku-dict-img-dir nil) "/Users/Shared/md/images/dic/")


(defun mandoku-dict-getline (inp)
  "parse the string and repetitevely call the dictionary"
  (let ((result-buffer (get-buffer-create "*Dict Result*"))
	(the-buf (current-buffer))
	(pos    (mandoku-position-at-point)))
    (set-buffer result-buffer)
    (toggle-read-only 0)
    (erase-buffer)
    (insert "* " (if pos pos "")  "\n")
    (with-current-buffer (get-buffer-create result-buffer)
      (url-insert-file-contents (concat mandoku-remote-url "/procline?query=" inp)
			      (lambda (status) (switch-to-buffer result-buffer))))
;  (switch-to-buffer result-buffer)
  (mandoku-dict-mode)
  (hide-sublevels 2)
  (goto-char (point-min))
  (switch-to-buffer-other-window result-buffer t)))

(defun mandoku-dict-get-region (beg end)
  (interactive "r")
  (let ((s (buffer-substring-no-properties beg end)))
    (mandoku-dict-getline s)))




(defun mandoku-dict-maybe-substring (s len)
  (if (> (length s) len)
      (concat (substring s 0 len) " … " )
    s))


(defun mandoku-dict-loc-entry (f)
  "f is a two-element list, with the first of the form 'loc-daikanwa-01' and the second of
the form V07-p08115-129"
  (let ((dict (car (cdr (split-string (car  f) "-"))))
	(loc  (cdr f)))
    (if (equal dict "daikanwa")
	;; p07-08115.djvu
	(format "[[%sdkw/p%s-%s.djvu][%s : %s]]"
		mandoku-dict-img-dir
		(substring (car (split-string loc "-")) 1 )
		(substring (car (cdr (split-string loc "-"))) 1)
		dict
		loc)
    (if (equal dict "hydzd")
	;; hydzd : // hydzd-p00001 V1-p0031-04
	(format "[[%shydzd/hydzd-p%s.djvu][%s : %s]]"
		mandoku-dict-img-dir
		(substring (car (cdr (split-string loc "-"))) 1)
		dict
		loc)
      (if (equal dict "koga")
	  ;;koga-p0001.djvu
	  (format "[[%skoga/koga-p%4.4d.djvu][%s : %s]]"
		  mandoku-dict-img-dir
		(string-to-int loc)
		dict
		loc)
	(if (equal dict "naka")
	    ;; naka-p1241.djvu
	    (format "[[%snaka/naka-p%4.4d.djvu][%s : %s]]"
		    mandoku-dict-img-dir
		    (string-to-int (substring loc 0 (- (length loc) 1 )))
		    dict
		    loc)
	  (if (equal dict "zgd")
	      (format "[[%szgd/zgd-p%4.4d.djvu][%s : %s]]"
		      mandoku-dict-img-dir
		      (string-to-int (substring loc 0 (- (length loc) 1 )))
		      dict
		      loc)
	  (if (equal dict "zhongwen")
	      (format "[[%szhwdcd/%5.5d.djvu][%s : %s]]"
		      mandoku-dict-img-dir
		      (string-to-int (substring (car (cdr (split-string loc "-"))) 1 ))
		      dict
		      loc)
	  (if (equal dict "je")
	      (format "[[%sjeb/jeb-p%4.4d.djvu][%s : %s]]"
		      mandoku-dict-img-dir
		      (string-to-int (car (cdr (split-string loc "/"))))
		      dict
		      loc)
	  (if (equal dict "yo")
	      (format "[[%syokoi/yokoi-p%4.4d.djvu][%s : %s]]"
		      mandoku-dict-img-dir
		      (string-to-int (car (split-string loc "/")))
		      dict
		      loc)
	  (if (equal dict "ina")
	      (format "[[%sina/ina-p%4.4d.djvu][%s : %s]]"
		      mandoku-dict-img-dir
		      (string-to-int (car (split-string loc "/")))
		      dict
		      loc)
	  (if (equal dict "bcs")
	      (format "[[%sbcs/bcs-p%4.4d.djvu][%s : %s]]"
		      mandoku-dict-img-dir
		      (string-to-int (car (split-string loc "/")))
		      dict
		      loc)
	  (if (equal dict "bsk")
	      (let ((vol  (format "vol%2.2d" (- (string-to-char loc) 9311)))
		    (page (format "p%4.4d" (string-to-number (substring loc 1 -1)))))
		(format "[[%sbsk/%s/bsk-%s-%s.djvu][%s : %s]]" mandoku-dict-img-dir vol vol page dict loc))

	  (if (equal dict "mz")
	      (let ((vol (string-to-int (substring (car (split-string loc "p")) 1)))
		    (page (string-to-int (substring (car (split-string loc ",")) 3 -1) )))
	      (format "[[%smz/vol%2.2d/mz-v%2.2d-p%4.4d.djvu][%s : %s]]"
		      mandoku-dict-img-dir
		      vol 
		      vol
		      page
		      dict
		      loc))
;	    (if (not (equal loc "dummy"))
;		""
	      (format "%s : %s" dict loc)))))))))))))))
  

(defun mandoku-dict-get-next-line ()
  "display the entries for the next line in the Dict Buffer"
  (interactive)
  (if (equal (buffer-name) "*Dict Result*")
      (other-window 1))
  (forward-line 1)
  (mandoku-dict-get-line)
)
(defun mandoku-dict-get-prev-line ()
  "display the entries for the next line in the Dict Buffer"
  (interactive)
  (if (equal (buffer-name) "*Dict Result*")
      (other-window 1))
  (forward-line -1)
  (mandoku-dict-get-line)
)
(defun mandoku-dict-repeat-line ()
  "display the entries for the next line in the Dict Buffer"
  (interactive)
  (if (equal (buffer-name) "*Dict Result*")
      (other-window 1))
  (mandoku-dict-get-line))


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
  (toggle-read-only 1)
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
	 
(provide 'mandoku-dict)


;;; mandoku-dict.el ends here
