;; mandoku-two-columns.el   -*- coding: utf-8 -*-
;; created [2017-09-20T09:59:28+0900]
;; remote editing for two-columns from mandoku

(defvar mandoku-two-columns-auto-save-timer nil)
(defvar mandoku-two-columns-allow-write-back t)
(defvar mandoku-two-columns-beg-marker nil)
(defvar mandoku-two-columns-end-marker nil)
(defvar mandoku-two-columns-saved-temp-window-config nil)
(defvar mandoku-two-columns-limit 20)
(defvar mandoku-two-columns-overlay nil)
(defvar mandoku-two-columns-ask-before-returning-to-edit-buffer t)
(defvar mandoku-two-columns-separator "	")
(defvar mandoku-two-columns-translation-pos nil)


(defcustom mandoku-two-columns-window-setup 'reorganize-frame
  "How the source code edit buffer should be displayed.
Possible values for this option are:

current-window    Show edit buffer in the current window, keeping all other
                  windows.
other-window      Use `switch-to-buffer-other-window' to display edit buffer.
reorganize-frame  Show only two windows on the current frame, the current
                  window and the edit buffer.  When exiting the edit buffer,
                  return to one window.
other-frame       Use `switch-to-buffer-other-frame' to display edit buffer.
                  Also, when exiting the edit buffer, kill that frame."
  :group 'org-edit-structure
  :type '(choice
	  (const current-window)
	  (const other-frame)
	  (const other-window)
	  (const reorganize-frame)))

(defun mandoku-two-columns-set-translation ()
  "Sets current buffer at current position as translation source."
  (interactive)
  (setq mandoku-two-columns-translation-pos (make-marker))
  (set-marker mandoku-two-columns-translation-pos (point))
  (message "Translation source set."))

(defun mandoku-two-columns-get-next-translation ()
  "Gets the next chunk of the associated translation."
					; 1. go to the position 2. get the text 3. move to next position and save it.
  (interactive)
  (let (string bpos)
    (unless mandoku-two-columns-translation-pos
      (user-error "Please set the translation buffer before invoking this command."))
    (with-current-buffer (marker-buffer mandoku-two-columns-translation-pos)
      (goto-char mandoku-two-columns-translation-pos)
      (search-forward "\n\n")
      (setq string (string-trim-right (buffer-substring mandoku-two-columns-translation-pos (point))))
      (skip-chars-forward " \n")
      (set-marker mandoku-two-columns-translation-pos (point))
      (bookmark-set (bookmark-buffer-name) nil nil nil)
      )
    (insert (replace-regexp-in-string "\n" " " string))
  ))

(defun mandoku-two-columns-edit-buffer-p (&optional buffer)
  "Non-nil when current buffer is a source editing buffer.
If BUFFER is non-nil, test it instead."
  (let ((buffer (org-base-buffer (or buffer (current-buffer)))))
    (and (buffer-live-p buffer)
	 (local-variable-p 'mandoku-two-columns-beg-marker buffer)
	 (local-variable-p 'mandoku-two-columns-end-marker buffer))))

(defun mandoku-two-columns-construct-edit-buffer-name (mandoku-buffer-name)
  "Construct the buffer name for a source editing buffer."
  (concat "*Mandoku 2C " mandoku-buffer-name " *"))

(defun mandoku-two-columns-edit-buffer (beg end)
  "Return the buffer editing the area between BEG and END.
Return nil if there is no such buffer."
  (catch 'exit
    (dolist (b (buffer-list))
      (with-current-buffer b
	(and (mandoku-two-columns-edit-buffer-p)
	     (= beg mandoku-two-columns-beg-marker)
	     (eq (marker-buffer beg) (marker-buffer mandoku-two-columns-beg-marker))
	     (= end mandoku-two-columns-end-marker)
	     (eq (marker-buffer end) (marker-buffer mandoku-two-columns-end-marker))
	     (throw 'exit b))))))

(defun mandoku-two-columns-source-buffer ()
  "Return source buffer edited by current buffer."
  (unless (mandoku-two-columns-edit-buffer-p) (error "Not in a source buffer"))
  (or (marker-buffer mandoku-two-columns-beg-marker)
      (error "No source buffer available for current editing session")))

(defun mandoku-two-columns-make-source-overlay (beg end edit-buffer)
  "Create overlay between BEG and END positions and return it.
EDIT-BUFFER is the buffer currently editing area between BEG and
END."
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'face 'secondary-selection)
    (overlay-put overlay 'edit-buffer edit-buffer)
    (overlay-put overlay 'help-echo
		 "Click with mouse-1 to switch to buffer editing this segment")
    (overlay-put overlay 'face 'secondary-selection)
    (overlay-put overlay 'keymap
		 (let ((map (make-sparse-keymap)))
		   (define-key map [mouse-1] 'org-edit-src-continue)
		   map))
    (let ((read-only
	   (list
	    (lambda (&rest _)
	      (user-error
	       "Cannot modify an area being edited in a dedicated buffer")))))
      (overlay-put overlay 'modification-hooks read-only)
      (overlay-put overlay 'insert-in-front-hooks read-only)
      (overlay-put overlay 'insert-behind-hooks read-only))
    overlay))

(defun mandoku-two-columns-remove-overlay ()
  "Remove overlay from current source buffer."
  (when (overlayp mandoku-two-columns-overlay) (delete-overlay mandoku-two-columns-overlay)))

(defun mandoku-two-columns-on-datum-p (datum)
  "Non-nil when point is on DATUM.
DATUM is an element or an object.  Consider blank lines or white
spaces after it as being outside."
  (and (>= (point) (org-element-property :begin datum))
       (<= (point)
	   (org-with-wide-buffer
	    (goto-char (org-element-property :end datum))
	    (skip-chars-backward " \r\t\n")
	    (if (eq (org-element-class datum) 'element)
		(line-end-position)
	      (point))))))


(defun mandoku-two-columns-contents-for-write-back ()
  "Return buffer contents in a format appropriate for write back.
Assume point is in the corresponding edit buffer."
  (let ((indentation 0)
	(preserve-indentation nil)
	(contents (progn (mandoku-2C-merge) (buffer-string)))
	;(indentation (or mandoku-two-columns-block-indentation 0))
	;(preserve-indentation mandoku-two-columns-preserve-indentation)
	;(contents (org-with-wide-buffer (buffer-string)))
	(write-back mandoku-two-columns-allow-write-back))
    (with-temp-buffer
      (insert (org-no-properties contents))
      (goto-char (point-min))
      (when (functionp write-back) (funcall write-back))
      (unless (or preserve-indentation (= indentation 0))
	(let ((ind (make-string indentation ?\s)))
	  (goto-char (point-min))
	  (while (not (eobp))
	    (when (looking-at-p "[ \t]*\\S-") (insert ind))
	    (forward-line))))
      (buffer-string))))

(defun mandoku-two-columns-contents-area (datum)
  (list
   (plist-get datum :begin) 
   (plist-get datum :end)
   (plist-get datum :content)
   ))

(defun mandoku-two-columns-switch-to-buffer (buffer context)
  (pcase mandoku-two-columns-window-setup
    (`current-window (pop-to-buffer-same-window buffer))
    (`other-window
     (switch-to-buffer-other-window buffer))
    (`other-frame
     (pcase context
       (`exit
	(let ((frame (selected-frame)))
	  (switch-to-buffer-other-frame buffer)
	  (delete-frame frame)))
       (`save
	(kill-buffer (current-buffer))
	(pop-to-buffer-same-window buffer))
       (_ (switch-to-buffer-other-frame buffer))))
    (`reorganize-frame
     (when (eq context 'edit) (delete-other-windows))
     (org-switch-to-buffer-other-window buffer)
     (when (eq context 'exit) (delete-other-windows)))
    (`switch-invisibly (set-buffer buffer))
    (_
     (message "Invalid value %s for `mandoku-two-columns-window-setup'"
	      mandoku-two-columns-window-setup)
     (pop-to-buffer-same-window buffer))))



(defun mandoku-two-columns-edit-element
    (datum name &optional write-back contents remote)
  "Edit DATUM contents in a dedicated buffer NAME.

When WRITE-BACK is non-nil, assume contents will replace original
region.  Moreover, if it is a function, apply it in the edit
buffer, from point min, before returning the contents.

When CONTENTS is non-nil, display them in the edit buffer.
Otherwise, show DATUM contents as specified by
`mandoku-two-columns-contents-area'.

When REMOTE is non-nil, do not try to preserve point or mark when
moving from the edit area to the source.

Leave point in edit buffer."
  (setq mandoku-two-columns-saved-temp-window-config (current-window-configuration))
  (let* ((area (mandoku-two-columns-contents-area datum))
	 (beg (copy-marker (nth 0 area)))
	 (end (copy-marker (nth 1 area) t))
	 (old-edit-buffer (mandoku-two-columns-edit-buffer beg end))
	 (contents (or contents (nth 2 area))))
    (if (and old-edit-buffer
	     (or (not mandoku-two-columns-ask-before-returning-to-edit-buffer)
		 (y-or-n-p "Return to existing edit buffer ([n] will revert changes)? ")))
	;; Move to existing buffer.
	(mandoku-two-columns-switch-to-buffer old-edit-buffer 'return)
      ;; Discard old edit buffer.
      (when old-edit-buffer
	(with-current-buffer old-edit-buffer (mandoku-two-columns-remove-overlay))
	(kill-buffer old-edit-buffer))
      (let* ((org-mode-p (derived-mode-p 'org-mode))
	     ;; Generate a new edit buffer.
	     (buffer (generate-new-buffer name))
	     (major 'mandoku-view-mode)
	     ;; Add an overlay on top of source.
	     (overlay (mandoku-two-columns-make-source-overlay beg end buffer)))
	;; Switch to edit buffer.
	(mandoku-two-columns-switch-to-buffer buffer 'edit)
	;; Insert contents.
	(insert contents)
	(remove-text-properties (point-min) (point-max)
				'(display nil invisible nil intangible nil))
	(set-buffer-modified-p nil)
	(setq buffer-file-name nil)
	;; TODO: Set up the two buffers for editing...
	;; 
	(mandoku-two-columns-edit-setup beg end)
	;; Start major mode.
	(if (not major) (fundamental-mode)
	  (let ((org-inhibit-startup t))
	    (condition-case e
		  (funcall major)
	      (error (message "Language mode `%s' fails with: %S"
			      major (nth 1 e))))))
	;; Transmit buffer-local variables for exit function.  It must
	;; be done after initializing major mode, as this operation
	;; may reset them otherwise.
	(toggle-truncate-lines 1)
	(setq-local mandoku-two-columns-from-org-mode org-mode-p)
	(setq-local mandoku-two-columns-beg-marker beg)
	(setq-local mandoku-two-columns-end-marker end)
	(setq-local mandoku-two-columns-remote remote)
	;(setq-local mandoku-two-columns-block-indentation ind)
	;(setq-local mandoku-two-columns-preserve-indentation preserve-ind)
	(setq-local mandoku-two-columns-overlay overlay)
	(setq-local mandoku-two-columns-allow-write-back write-back)
	;; Move mark and point in edit buffer to the corresponding
	;; location.
	(if remote
	    (progn
	      ;; Put point at first non read-only character after
	      ;; leading blank.
	      (goto-char
	       (or (text-property-any (point-min) (point-max) 'read-only nil)
		   (point-max)))
	      (skip-chars-forward " \r\t\n"))
	  )))))
	  ;; Set mark and point.
	  ;; (when mark-coordinates
	  ;;   (mandoku-two-columns-goto-coordinates mark-coordinates (point-min) (point-max))
	  ;;   (push-mark (point) 'no-message t)
	  ;;   (setq deactivate-mark nil))
	  ;; (mandoku-two-columns-goto-coordinates
	  ;;  point-coordinates (point-min) (point-max)))))))

(defun mandoku-2C-split ()
  ;; b1 is the left, b2 the right hand buffer
  ;; this is called immediately after 2C-two-columns
  (let ((b1 (current-buffer))
	(b2 (2C-other t))
	left-string
	stringl)
    (goto-char (point-min))
    ;; buffer b2 is still empty
    (while (not (eobp))
      (setq stringl (split-string
		    (buffer-substring
		     (point)
		     (progn (end-of-line) (point))) mandoku-two-columns-separator ))
      (setq left-string (concat left-string (cadr stringl) "\n"))
      (if (string= (car stringl) "")
	  ()
	(delete-backward-char (- (point) (point-at-bol)))
	(insert (car stringl))
	)
      (or (eobp)
	  (forward-char))		; next line
      )
    (set-buffer b2)
    (erase-buffer)
    (insert (string-trim-right left-string) "\n")
    (set-buffer b1)
    ;;
))

(defun mandoku-two-columns-edit-setup (beg end)
  "Prepare for two column editing of the src buffer"
  (let ((2C-window-width 20))
    (2C-two-columns)
    (mandoku-2C-split)
    (with-current-buffer (2C-other)
      (setq-local mandoku-two-columns-beg-marker beg)
      (setq-local mandoku-two-columns-end-marker end)
      (setq-local mandoku-two-columns-remote remote)
      (setq-local mandoku-two-columns-overlay overlay)
      (setq-local mandoku-two-columns-allow-write-back write-back)
      (mandoku-2C-right-mode)
      )
  ))

;;; Mandoku two columns (right) minor mode

(defvar mandoku-2C-right-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c'" 'mandoku-two-columns-exit)
    (define-key map "\C-c\C-k" 'mandoku-two-columns-abort)
    (define-key map "\C-x\C-s" 'mandoku-two-columns-save)
    (define-key map "\C-c\C-n" 'mandoku-two-columns-get-next-translation)
    map))

(define-minor-mode mandoku-2C-right-mode
  "Minor mode for language major mode buffers generated by Org.
\\<org-mode-map>
This minor mode is turned on in two situations:
  - when editing a source code snippet with `\\[org-edit-special]'
  - when formatting a source code snippet for export with htmlize.

\\{mandoku-2C-right-mode-map}

See also `mandoku-2C-right-mode-hook'."
  nil " Mandoku2C" nil
  (when org-edit-src-persistent-message
    (setq-local
     header-line-format
     (substitute-command-keys
      (if mandoku-two-columns-allow-write-back
	  "Edit, then exit with `\\[mandoku-two-columns-exit]' or abort with \
`\\[mandoku-two-columns-abort]'"
	"Exit with `\\[mandoku-two-columns-exit]' or abort with \
`\\[mandoku-two-columns-abort]'"))))
  ;; Possibly activate various auto-save features (for the edit buffer
  ;; or the source buffer).
  (when org-edit-src-turn-on-auto-save
    (setq buffer-auto-save-file-name
	  (concat (make-temp-name "mandoku-2C-")
		  (format-time-string "-%Y-%d-%m")
		  ".txt")))
  (toggle-truncate-lines 1)
  (unless (or mandoku-two-columns-auto-save-timer (zerop org-edit-src-auto-save-idle-delay))
    (setq mandoku-two-columns-auto-save-timer
	  (run-with-idle-timer
	   org-edit-src-auto-save-idle-delay t
	   (lambda ()
	     (save-excursion
	       (let (edit-flag)
		 (dolist (b (buffer-list))
		   (with-current-buffer b
		     (when (mandoku-two-columns-edit-buffer-p)
		       (unless edit-flag (setq edit-flag t))
		       (when (buffer-modified-p) (org-edit-src-save)))))
		 (unless edit-flag
		   (cancel-timer mandoku-two-columns-auto-save-timer)
		   (setq mandoku-two-columns-auto-save-timer nil)))))))))


;; borrowed and adapted from two-column.el (by Daniel Pfeiffer and ESR)
;; 
(defun mandoku-2C-merge ()
  "Merges the associated buffer with the current mandoku buffer.
The associated buffer usually contains an aligned translation.
The value of `mandoku-two-columns-separator' gets inserted on merged lines.  The
two columns are thus pasted side by side, in a single text.  If
the other buffer is not displayed to the left of this one, then
this one becomes the left column. The merging will be done based
on visibility, lines with a :zhu: drawer containing annotations
will be skipped over."
  (interactive)
;  (and (> (car (window-edges)) 0)	; not touching left edge of screen
;       (eq (window-buffer (previous-window))
;	   (2C-other t))
;       (other-window -1))
;  (save-excursion
    (let ((b2 (current-buffer))
	  (b1 (2C-other t))
	  string)
      (with-current-buffer b1
;	(2C-toggle-autoscroll 0)
	(goto-char (point-min)))
      (set-buffer b2)
      (goto-char (point-min))
      (while (not (eobp))
	(setq string (buffer-substring (point)
				       (progn (end-of-line) (point))))
	(or (eobp)
	    (forward-char))		; next line
	(set-buffer b1)
	(if (string= string "")
	    ()
	  (end-of-line)
	  ;(indent-to-column 2C-window-width)
	  (insert mandoku-two-columns-separator string))
	(next-line 1)
	(beginning-of-line)
	(set-buffer b2))
      (set-buffer b1)
      (kill-buffer b2)
      ))
  

;; this is the entry point
;;;###autoload
(defun mandoku-two-columns-edit (&optional code edit-buffer-name)
  "Edit the text/translation at point.  A chunk of text of the
next `mandoku-two-columns-limit' lines is copied to a separate
buffer and split between text and translation for convenient
editing. "
  (interactive)
  (let* ((beg (make-marker))
	 (end (make-marker))
	 (fn (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    (setq beg  (progn (beginning-of-line) (point)))
    (setq end (progn (forward-line mandoku-two-columns-limit) (point)))
    (mandoku-two-columns-edit-element
     (list :begin beg :end end :content (buffer-substring-no-properties beg end))
     (mandoku-two-columns-construct-edit-buffer-name fn)
     t)))

;;;
(defun mandoku-two-columns-abort ()
  "Abort editing of the src code and return to the Org buffer."
  (interactive)
  (let (mandoku-two-columns-allow-write-back) (mandoku-two-columns-exit)))

(defun mandoku-two-columns-save ()
  "Save the edited content back to the source buffer, continue editing."
  )

(defun mandoku-two-columns-exit ()
  "Kill current sub-editing buffer and return to source buffer.
This can be called from either of the two buffers."
  (interactive)
  (unless (mandoku-two-columns-edit-buffer-p) (error "Not in a sub-editing buffer"))
  (let* ((beg mandoku-two-columns-beg-marker)
	 (end mandoku-two-columns-end-marker)
	 (write-back mandoku-two-columns-allow-write-back)
	 (remote mandoku-two-columns-remote)
	 (coordinates (and (not remote)
			   (org-src--coordinates (point) 1 (point-max))))
	 (code (and write-back (mandoku-two-columns-contents-for-write-back))))
    (set-buffer-modified-p nil)
    ;; Switch to source buffer.  Kill sub-editing buffer.
    (let ((edit-buffer (current-buffer)))
      (org-src-switch-to-buffer (marker-buffer beg) 'exit)
      (kill-buffer edit-buffer))
    ;; Insert modified code.  Ensure it ends with a newline character.
    (org-with-wide-buffer
     (when (and write-back (not (equal (buffer-substring beg end) code)))
					;(mandoku-two-columns-remove-overlay)
       (remove-overlays)       
       (undo-boundary)
       (goto-char beg)
       (delete-region beg end)
       (let ((expecting-bol (bolp)))
	 (insert code)
	 (when (and expecting-bol (not (bolp))) (insert "\n")))))
    ;; If we are to return to source buffer, put point at an
    ;; appropriate location.  In particular, if block is hidden, move
    ;; to the beginning of the block opening line.
    (unless remote
      (goto-char beg)
      (cond
       ;; Block is hidden; move at start of block.
       ((cl-some (lambda (o) (eq (overlay-get o 'invisible) 'org-hide-block))
		  (overlays-at (point)))
	(beginning-of-line 0))
       (write-back (org-src--goto-coordinates coordinates beg end))))
    ;; Clean up left-over markers and restore window configuration.
    (set-marker beg nil)
    (set-marker end nil)
    (bookmark-set (bookmark-buffer-name) nil nil nil)
    (save-buffer)
    (when mandoku-two-columns-saved-temp-window-config
      (set-window-configuration mandoku-two-columns-saved-temp-window-config)
      (setq mandoku-two-columns-saved-temp-window-config nil))))


(provide 'mandoku-two-columns)

;;; mandoku-two-columns ends here
