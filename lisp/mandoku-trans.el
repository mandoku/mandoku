;;; mandoku-trans.el --- Add translation side by side to Chinese text
;; -*- coding: utf-8 -*-
;;created [2017-12-19T15:26:04+0900]
;; add numbered items to begin of following 10 lines:
(defvar mandoku-circled-number-first 9312)
(defvar mandoku-circled-number-last 9326)
(defvar mandoku-circled-number-register ?C)
(defvar mandoku-textpos-marker nil)
(defvar mandoku-transpos-marker nil)
(defvar mandoku-trans-initialized-p nil)
;;;###autoload
(defun mandoku-trans-initialize ()
  "This adds a translation to a text.  It will first add circled
numbers to the text, one on each line.  It will then allow the
user to insert circled numbers into the translation and finally
merge text and translation and remove the circled numbers again."
  (interactive)
  (unless mandoku-trans-initialized-p
    (setq mandoku-transpos-marker (make-marker))
    (setq mandoku-textpos-marker (make-marker))
    (local-set-key (kbd "C-;")  'mandoku-add-circled-numbers-to-translation)
    ;; do we have a valid textpos?
    (setq mandoku-trans-initialized-p t)
    )
)
  
(defun mandoku-trans-valid-textpos-p ()
  (save-excursion
    (goto-char mandoku-textpos-marker)
    (looking-at (number-to-string mandoku-circled-number-first))))

(defun mandoku-add-circled-numbers (&optional num)
  (interactive)
  (unless mandoku-trans-initialized-p
    (mandoku-trans-initialize))
  (let ((cnt mandoku-circled-number-first)
	(start (point)))
    (beginning-of-line)
    (setq mandoku-textpos-marker (make-marker))
    (set-marker mandoku-textpos-marker (point))
    (bookmark-set (bookmark-buffer-name) nil)
    (while (< cnt mandoku-circled-number-last)
      (insert (format "%c" cnt))
      (setq cnt (+ cnt 1))
      (mandoku-forward-line))
    (goto-char start)
    (recenter 3)
    (hl-line-mode))
  )

(defun mandoku-trans-circled-number-to-register()
  (interactive)
  (let ((tr-regex (format "[%c-%c]"  mandoku-circled-number-first  mandoku-circled-number-last)))
    (if (looking-at tr-regex)
	(number-to-register (following-char) mandoku-circled-number-register)
)))

(defun mandoku-add-circled-numbers-to-translation()
  (interactive)
  (or (get-register mandoku-circled-number-register)
      (ignore-errors (> (get-register mandoku-circled-number-register) mandoku-circled-number-last)) 
      (number-to-register (- mandoku-circled-number-first 1) mandoku-circled-number-register))
  (increment-register '(1) mandoku-circled-number-register)
  ;; at the end, save translation and move on
  (if (>= (get-register mandoku-circled-number-register)
	  mandoku-circled-number-last)
      (progn
	(mandoku-add-translation-to-text)
	(save-current-buffer
	  (set-buffer (marker-buffer mandoku-textpos-marker))
	  (goto-char mandoku-textpos-marker)
	  (mandoku-add-circled-numbers)
	  (recenter 3))
	(mandoku-add-circled-numbers-to-translation))
    (progn
      (insert (get-register mandoku-circled-number-register))
      (with-current-buffer (marker-buffer mandoku-textpos-marker)
	(goto-char mandoku-textpos-marker)
	(search-forward (format "%c" (get-register mandoku-circled-number-register)))
	(mandoku-forward-line)
	(hl-line-highlight))
    )))

(defun mandoku-add-translation-to-text ()
  "This needs to start in the translation buffer with point at
the end ot the text to be inserted."
  (interactive)
  (let* ((tr-end (point))
	 (tr-regex (format "[%c-%c]"  mandoku-circled-number-first  mandoku-circled-number-last))
	 (tr-start (save-excursion
		     (goto-char (point-min))
		     (re-search-forward tr-regex)
		     (- (point) 1)))
	(tr (cdr (mandoku-split-string (buffer-substring-no-properties tr-start tr-end)
				       tr-regex
				       nil t)))
	(buf (current-buffer))
	(end (make-marker))
	t1 t2 )
    ;; todo: clean up and remove the numbers.
    (with-current-buffer (marker-buffer mandoku-textpos-marker)
      (set-marker end (point-min))
      (while tr
	(goto-char mandoku-textpos-marker)
	(setq t1 (car tr))
	(setq tr (cdr tr))
	(setq t2 (string-trim (car tr)))
	(setq tr (cdr tr))
	(when (< 0 (length t2))
	  (search-forward t1)
	  (end-of-line)
	  (insert (format "\t%s" (replace-regexp-in-string "\n" " " t2)))
	  (if (> (point) end)
	      (set-marker end (point)))))
      (goto-char (point-max))
      (while (re-search-backward tr-regex mandoku-textpos-marker t)
	(replace-match ""))
      (goto-char end)
      (mandoku-forward-line)
      (set-marker mandoku-textpos-marker (point))
      (bookmark-set (bookmark-buffer-name) nil)
      (save-buffer)
      )
    (save-excursion
      (while (re-search-backward tr-regex nil t)
	(replace-match "")))
    )
  ;; reset circled numbers
  (bookmark-set (bookmark-buffer-name) nil)
  (set-marker mandoku-transpos-marker (point))
  (number-to-register (- mandoku-circled-number-first 1) mandoku-circled-number-register)
  )

;; utilities -- maybe move to mandoku.el
(defun mandoku-forward-line ()
  "Forward line, but skip over empty lines, property drawers and headlines."
  (forward-line)
  (when (looking-at ":")
    (let ((case-fold-search t)) (re-search-forward ":end:"))
    (mandoku-forward-line))
  (when (looking-at "\n")
    (mandoku-forward-line))
  (when (looking-at "*")
    (mandoku-forward-line))
  (when (looking-at "#")
    (mandoku-forward-line))
  )

(defun mandoku-split-string (string &optional separators omit-nulls keep-sep)
  "Split STRING into substrings bounded by matches for SEPARATORS."
  (let* ((keep-nulls (not (if separators omit-nulls t)))
         (rexp (or separators split-string-default-separators))
         (start 0)
         this-start this-end
         notfirst
         (list nil)
         (push-one
          (lambda ()
            (when (or keep-nulls (< this-start this-end))
              (let ((this (substring string this-start this-end)))
                (when (or keep-nulls (> (length this) 0))
                  (push this list)))))))
    (while (and (string-match
                 rexp string
                 (if (and notfirst
                          (= start (match-beginning 0))
                          (< start (length string)))
                     (1+ start) start))
                (< start (length string)))
      (setq notfirst t)
      (setq this-start start this-end (match-beginning 0)
            start (match-end 0))
      (funcall push-one)
      (when keep-sep
        (push (match-string 0 string) list)))
    (setq this-start start this-end (length string))
    (funcall push-one)
    (nreverse list)))


;;; mandoku-trans.el ends here
