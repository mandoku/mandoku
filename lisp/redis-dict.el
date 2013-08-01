;; redis based dictionary access (to ci4 etc)
;; cwittern@gmail.com [2011-01-08T10:52:58+0900]
;; dict format:
;; loc-x = location keys
;; def-x = definition keys
;; pron-x = pronounciation keys
;; all keys will be sorted lexically
(require 'redis)
(require 'org)
(require 'cl)
(require 'assoc)
;; this is the same as mandoku-regex, for the moment copying it to avoid dependencies.
;;(setq res (redis-cmd-hgetall "本文"))

(defvar redict-regex "<[^>]*>\\|[　-㄀＀-￯\n¶]+\\|\t[^\n]+\n")
;; pron-kanwa-01 for kanwa!
(defvar redict-pron "pron-pinyin-01")
(defvar redict-prefdic "def-abc-01-01")
(defvar redict-dict-img-dir "/Users/Shared/md/images/dic/")

;;(let (( redis-buffer "*redis-dict-buffer*"))
(setq redis (redis-open-connection redis-buffer))
(set-process-coding-system redis 'utf-8 'utf-8)

(defun redict-procline (inp)
  "parse the string and repetitevely call the dictionary"
  (let ((v '())
	(l  (replace-regexp-in-string redict-regex "" inp))
	(res 1))
    (loop for i from 0 to  (- (length l) 1) do
	  (setq j i)
	  (while (and res (< j (length l)))
	    (setq j (+ j 1))
	    (setq s (substring l i j))
	    (setq res (redis-cmd-hgetall s))
	    (if res
		(aput 'v s res)))
	  (setq res 1))
    v))

(defun redict-get-line ()
  (interactive)
  (redict-display-result
   (redict-procline (mandoku-get-line))
   (redict-position-at-point)))
  ;; (save-excursion
  ;;   (end-of-line)
  ;;   (setq end (point))
  ;;   (beginning-of-line)
  ;;   (redict-display-result
  ;;    (redict-procline (buffer-substring-no-properties (point) end))
  ;;    (redict-position-at-point))))

(defun redict-display-result (res pos)
  (let ((result-buffer (get-buffer-create "*Dict Result*"))
		(the-buf (current-buffer)))
    (set-buffer result-buffer)
    (toggle-read-only -1)
    (erase-buffer)
    (insert "* " (if pos pos "")  "\n")
    (dolist (e (reverse res))
      ;; this displays the heading line
;;      (let ((hd (concat (car e) " ")))
      ;; lets ignore the head for now
      (let ((hd ""))
	(insert
	 (concat "** "
		 (car e) " ("
		 (format "%s" (- (length e) 1))
		 "): "
		 (cdr (assoc redict-pron e))
		 (if (assoc redict-prefdic e)
		     (concat " / "
			     (redict-maybe-substring (cdr (assoc redict-prefdic e)) 40)
			     " / "))
		 "\n" ))
      ;; this displays the entries
	(setq e (sort (cdr e) (lambda (a b) (string< (car a) (car b)))))
	(setq src "")
	(setq loc "")
	(dolist (f e)
	  (setq dx (split-string (car f) "-"))
	  (if (not (equal loc (car dx)))
	      (progn
		(setq loc (car dx))
		(if (equal "loc" loc)
		    (insert "*** " hd "loc\n"))))
	  (if (not (equal src (car (cdr dx))))
	      (progn
		(setq src (car (cdr dx)))
		(if (not (equal loc "loc"))
		    (if (equal src "abc")
			(insert "*** lyt\n" loc)
		    (insert "*** " hd (if src src "") "\n")))))
	  (insert
	   (concat
	    (if (equal src "hydcd1") "**** " "")
	    (if (not (equal loc "loc"))
		(concat
		 (car (cdr (cdr dx))) (car (cdr (cdr (cdr dx)) )) ": " ))
	    (if (equal loc "loc")
		(redict-loc-entry f)
	      (cdr f)) "\n"))
;      (fill-paragraph)
      )))
    (redict-mode)
;    (org-overview)
    (hide-sublevels 2)
    (goto-char (point-min))
    (switch-to-buffer-other-window result-buffer t)
;    (other-window 1)
    ))

(defun redict-maybe-substring (s len)
  (if (> (length s) len)
      (concat (substring s 0 len) " … " )
    s))

(defun redict-position-at-point ()
  (interactive)
  (save-excursion
    (let ((p (point)))
      (if
	  (re-search-backward "<pb:" nil t)
	  (progn
	    (re-search-forward "\\([^_]*\\)_\\([^_]*\\)>" nil t)
	    (setq textid (match-string 1))
	    (setq page (match-string 2))
	    (setq line 0)
	    (while (and
		    (< (point) p )
		    (re-search-forward "¶" (point-max) t))
	      (setq line (+ line 1)))
	    ;; in fact, I might want to get a proper link here...
	    (format "%s:%s%2.2d" textid page line))
	;; looks like this is not a mandoku buffer, lets get filename,  line number
	(format "%s:%s" (buffer-file-name) (line-number-at-pos)))
      )))

(defun redict-loc-entry (f)
  "f is a two-element list, with the first of the form 'loc-daikanwa-01' and the second of
the form V07-p08115-129"
  (let ((dict (car (cdr (split-string (car  f) "-"))))
	(loc  (cdr f)))
    (if (equal dict "daikanwa")
	;; p07-08115.djvu
	(format "[[%sdkw/p%s-%s.djvu][%s : %s]]"
		redict-dict-img-dir
		(substring (car (split-string loc "-")) 1 )
		(substring (car (cdr (split-string loc "-"))) 1)
		dict
		loc)
    (if (equal dict "hydzd")
	;; hydzd : // hydzd-p00001 V1-p0031-04
	(format "[[%shydzd/hydzd-p%s.djvu][%s : %s]]"
		redict-dict-img-dir
		(substring (car (cdr (split-string loc "-"))) 1)
		dict
		loc)
      (if (equal dict "koga")
	  ;;koga-p0001.djvu
	  (format "[[%skoga/koga-p%4.4d.djvu][%s : %s]]"
		  redict-dict-img-dir
		(string-to-int loc)
		dict
		loc)
	(if (equal dict "naka")
	    ;; naka-p1241.djvu
	    (format "[[%snaka/naka-p%4.4d.djvu][%s : %s]]"
		    redict-dict-img-dir
		    (string-to-int (substring loc 0 (- (length loc) 1 )))
		    dict
		    loc)
	  (if (equal dict "zgd")
	      (format "[[%szgd/zgd-p%4.4d.djvu][%s : %s]]"
		      redict-dict-img-dir
		      (string-to-int (substring loc 0 (- (length loc) 1 )))
		      dict
		      loc)
	  (if (equal dict "zhongwen")
	      (format "[[%szhwdcd/%5.5d.djvu][%s : %s]]"
		      redict-dict-img-dir
		      (string-to-int (substring (car (cdr (split-string loc "-"))) 1 ))
		      dict
		      loc)
	  (if (equal dict "je")
	      (format "[[%sjeb/jeb-p%4.4d.djvu][%s : %s]]"
		      redict-dict-img-dir
		      (string-to-int (car (cdr (split-string loc "/"))))
		      dict
		      loc)
	  (if (equal dict "yo")
	      (format "[[%syokoi/yokoi-p%4.4d.djvu][%s : %s]]"
		      redict-dict-img-dir
		      (string-to-int (car (split-string loc "/")))
		      dict
		      loc)
	  (if (equal dict "ina")
	      (format "[[%sina/ina-p%4.4d.djvu][%s : %s]]"
		      redict-dict-img-dir
		      (string-to-int (car (split-string loc "/")))
		      dict
		      loc)
	  (if (equal dict "bcs")
	      (format "[[%sbcs/bcs-p%4.4d.djvu][%s : %s]]"
		      redict-dict-img-dir
		      (string-to-int (car (split-string loc "/")))
		      dict
		      loc)
	  (if (equal dict "bsk")
	      (let ((vol  (format "vol%2.2d" (- (string-to-char loc) 9311)))
		    (page (format "p%4.4d" (string-to-number (substring loc 1 -1)))))
		(format "[[%sbsk/%s/bsk-%s-%s.djvu][%s : %s]]" redict-dict-img-dir vol vol page dict loc))

	  (if (equal dict "mz")
	      (let ((vol (string-to-int (substring (car (split-string loc "p")) 1)))
		    (page (string-to-int (substring (car (split-string loc ",")) 3 -1) )))
	      (format "[[%smz/vol%2.2d/mz-v%2.2d-p%4.4d.djvu][%s : %s]]"
		      redict-dict-img-dir
		      vol 
		      vol
		      page
		      dict
		      loc))
;	  (if (equal dict "dummy")
	;; dummys, for entries that would otherwise be unrichable
;	      ""
	    (format "%s : %s" dict loc)))))))))))))))
  

(defun redict-get-next-line ()
  "display the entries for the next line in the Dict Buffer"
  (interactive)
  (if (equal (buffer-name) "*Dict Result*")
      (other-window 1))
  (forward-line 1)
  (redict-get-line)
)
(defun redict-get-prev-line ()
  "display the entries for the next line in the Dict Buffer"
  (interactive)
  (if (equal (buffer-name) "*Dict Result*")
      (other-window 1))
  (forward-line -1)
  (redict-get-line)
)
(defun redict-repeat-line ()
  "display the entries for the next line in the Dict Buffer"
  (interactive)
  (if (equal (buffer-name) "*Dict Result*")
      (other-window 1))
  (let ((redis-delay 1))
    (redict-get-line))
)

;; redict-view-mode

(defvar redict-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" 'view-mode)
    (define-key map " " 'View-scroll-page-forward)
    (define-key map "r" 'redict-repeat-line)
    (define-key map "n" 'redict-get-next-line)
    (define-key map "p" 'redict-get-prev-line)
    (define-key map "p" 'redict-get-prev-line)
         map)
  "Keymap for redict-view mode"
)

(define-derived-mode redict-mode org-mode "redict-mode"
  "a mode to view dictionary files
  \\{redict-mode-map}"
  (setq case-fold-search nil)
  (set (make-local-variable 'org-startup-folded) 'overview)
  (toggle-read-only 1)
;  (view-mode)
)


;(global-set-key (kbd "C-c g") 'redict-get-line)
(global-set-key (kbd "M-a") 'redict-get-line) ; was backward-sentence.
(global-set-key (kbd "M-s a") 'redict-get-line)
;(global-set-key (kbd "M-s n") 'redict-get-next-line)
;(global-set-key (kbd "M-s p") 'redict-get-prev-line)
;(global-set-key (kbd "M-s r") 'redict-repeat-line)

(define-key redict-mode-map
             "n" 'redict-get-next-line)
(defun redict-highlight (state)
  (interactive)
  (cond 
   ((and (eq major-mode 'redict-mode)
	     (memq state '(children subtree)))
    (save-excursion
    (let ((hw (progn 
		(while (> (org-current-level) 2)
		  (outline-previous-heading))
		(car (split-string  (org-get-heading))))))
      (hi-lock-mode 1)
      (highlight-regexp hw))))
   ((and (eq major-mode 'redict-mode)
	     (memq state '(overview folded)))
      (hi-lock-mode 0))))

(add-hook 'org-cycle-hook 'redict-highlight) 
	 
(provide 'redis-dict)


;;; redis-dict.el ends here
