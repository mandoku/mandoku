;;; tls.el --- A tool to access the TLS database
;; -*- coding: utf-8 -*-
;; created [2016-02-10T21:02:54+0900]
;;
;; Copyright (c) 2016 Christian Wittern
;;
;; Author: Christian Wittern <cwittern@gmail.com>
;; URL: https://github.com/krp-zinbun/tls
;; Version: 0.01
;; Keywords: convenience
;; Package-Requires: ((mandoku "20160301.1705") (github-clone "20150705.1705"))
;; This file is not part of GNU Emacs.

(require 'mandoku)
(require 'mandoku-dict)
;; TODO: this needs to be fixed!  Does not work when packaged!
(defvar mandoku-tls-root-path
  (replace-in-string (file-name-directory (or byte-compile-current-file
                                             load-file-name
                                             buffer-file-name)) "mandoku/lisp/" ""))

(defcustom mandoku-tls-lexicon-path
  (concat mandoku-tls-root-path "tls/lexicon/")
  "Path to TLS lexicon"
  :type 'string
  :group 'mandoku-tls)

(defcustom mandoku-tls-text-path
  (concat (replace-in-string mandoku-tls-root-path "tls/" "") "tls-texts/")
  "Path to TLS texts"
  :type 'string
  :group 'mandoku-tls)
(defcustom mandoku-tls-concept-template
  (concat mandoku-tls-lexicon-path "core/concept-template.org")
  "Filename for template for concepts"
  :type 'string
  :group 'mandoku-tls)
  
(defcustom mandoku-tls-types
  '("tls"
    "syn-func"  ;; syntactic functions
    "sem-feat"  ;; semantic features
    )
  "List of types known in `tls'."
  :type '(repeat :tag "List of tls types" string)
  :group 'mandoku-tls)

;; core lexicon
(defvar mandoku-tls-syn-word-locs (concat mandoku-tls-lexicon-path "core/swl.txt"))
(defvar mandoku-tls-syn-words (concat mandoku-tls-lexicon-path "core/syntactic-words.org"))
(defvar mandoku-tls-syn-func-org (concat mandoku-tls-lexicon-path "core/syn-func.org"))
(defvar mandoku-tls-sem-feat-org (concat mandoku-tls-lexicon-path "core/sem-feat.org"))


(defvar mandoku-tls-initialized-p nil "Say whether TLS has been initialized.")


(defvar mandoku-tls-swl nil "Hash table of syntax word locations")
(defvar mandoku-tls-words nil "Hash table of syntax words")
(defvar mandoku-tls-concepts nil "Hash table index for concepts")
(defvar mandoku-tls-pos-info nil "Hash table of POS info, such as syntactic function or semantic feature occurrences in mandoku-tls-words")
(defvar mandoku-tls-syn-func-tab nil "Hash table of syn-func file for helm")

(defvar mandoku-tls-inp nil)


(defun mandoku-tls-read-syn-func-org () 
  "read the syntactic functions table"
  (setq mandoku-tls-syn-func-tab (make-hash-table :test 'equal))
  (when (file-exists-p mandoku-tls-syn-func-org)
    (with-current-buffer (find-file-noselect mandoku-tls-syn-func-org t)
	(org-map-entries 'mandoku-tls-get-syn-func-item "+LEVEL<=1" nil))))

(defun mandoku-tls-get-syn-func-item ()
  (let ((hw (replace-in-string (org-get-heading) "=SYN. FUNCTIONS= " ""))
	(uuid (org-entry-get (point) "CUSTOM_ID"))
	beg end def)
    (search-forward "** DEF" nil t)
    (next-line 1)
    (setq beg (point))
    (search-forward "\n**" nil t)
    (setq end (- (match-beginning 0) 1))
    (setq def (buffer-substring-no-properties beg end))
    (puthash uuid (list :name hw :def def) mandoku-tls-syn-func-tab)
    ))
  
(defun mandoku-tls-read-swl () 
  "read the syntactic word locations table"
  (setq mandoku-tls-swl (make-hash-table :test 'equal))
  (when (file-exists-p mandoku-tls-syn-word-locs)
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8)
	    textid)
	(insert-file-contents mandoku-tls-syn-word-locs) ;
	(goto-char (point-min))
	(while (re-search-forward "^\\(uuid[^\t]+\\)\t\\([^
]+\\)$" nil t)
	  (let* ((uuid (match-string 1))
		(rest (match-string 2))
		(loc (split-string rest "##"))
		(textid (car loc))
		(title (cadr loc))
		(date (nth 2 loc))
		line 
		tchar)
	    (setq line (plist-put line :line (concat "[[tls:text:" textid "][" title "]]\t" (nth 3 loc))))
	    (setq line (plist-put line :date date))
	    (if (gethash uuid mandoku-tls-swl)
		(puthash uuid (cons line (gethash uuid mandoku-tls-swl))  mandoku-tls-swl)
	      (puthash uuid (cons line nil)  mandoku-tls-swl))
	    ))))))

(defun mandoku-tls-read-synwords () 
  "read the syntactic words table"
  (setq mandoku-tls-words (make-hash-table :test 'equal))
  (setq mandoku-tls-pos-info (make-hash-table :test 'equal))
  (when (file-exists-p mandoku-tls-syn-words)
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8)
	    textid)
	(insert-file-contents mandoku-tls-syn-words) ;
	(goto-char (point-min))
	(while (re-search-forward "^\\([^
]+\\)\t\\([^\t
]+\\)\t\\([^\t
]+\\)$" nil t)
	  (let ((concept (match-string 2))
		(posdef (match-string 3))
		(charpin (split-string (match-string 1) " / "))
		tchar type)
	    ;; there could be duplicates? remove them
	    (dolist (c (delq nil (delete-dups charpin)))
	      (setq tchar (car (split-string c "@")))
	      (if (< 0 (length tchar))
		  (if (gethash tchar mandoku-tls-words)
		      (puthash tchar (cons (list (cadr (split-string c "@")) concept posdef) (gethash tchar mandoku-tls-words))  mandoku-tls-words)
		    (puthash tchar (cons (list (cadr (split-string c "@")) concept posdef) nil) mandoku-tls-words))))
	    ;; pos-info
	    (while (string-match "\\[\\[tls:\\([^:]+\\)::#\\([^\]]+\\)\\]" posdef (match-end 0))
	      (setq type (match-string 1 posdef))
	      (setq tchar (match-string 2 posdef))
	      (when (member type mandoku-tls-types)
		(if (gethash tchar mandoku-tls-pos-info)
		    ;(puthash tchar (cons (list :type type (replace-in-string (mapconcat 'identity charpin " ") "@" " ") concept posdef) (gethash tchar mandoku-tls-pos-info))  mandoku-tls-pos-info)
		    (puthash tchar (cons (list (replace-in-string (mapconcat 'identity charpin " ") "@" " ") concept posdef) (gethash tchar mandoku-tls-pos-info))  mandoku-tls-pos-info)
		  (puthash tchar (cons (list (replace-in-string (mapconcat 'identity charpin " ") "@" " ") concept posdef) nil)  mandoku-tls-pos-info)
		(message tchar)
	      )))
	    ))))))

(defun mandoku-tls-read-concepts ()
  "Read the concept files into the hash table"
  (setq mandoku-tls-concepts (make-hash-table :test 'equal))
  (dolist (file (directory-files (concat mandoku-tls-lexicon-path "concepts") nil ".*org$" ))
    (message file)
    (mandoku-tls-read-concept-file (concat mandoku-tls-lexicon-path "concepts/" file))))


(defun mandoku-tls-read-concept-file (file)
  "Read one concept file into the hash table"
  (when (file-exists-p file)
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8)
	    (name (file-name-sans-extension (file-name-nondirectory file)))
	    pointers words plist h)
	(insert-file-contents file)
	(goto-char (point-min))
	(when (search-forward "\n* =CONCEPT=" nil t)
	  (setq plist (plist-put plist :synonyms (delq nil (org-property--local-values "SYNONYM" nil))))
	  (when (search-forward "\n** POINTERS" nil t)
	    (org-next-visible-heading 1)
	    (while (= 3 (org-outline-level))
	      (push (mandoku-tls-read-subtree-list) pointers)
	      (org-next-visible-heading 1)))
	  (goto-char (point-min))
	  (when (search-forward "\n** WORDS" nil t)
	    (while  (search-forward "\n*** " nil t)
	      (push (car (split-string (org-get-heading))) words)))
	  ;; now add the uplink to those concepts referenced in "NARROW CONCEPTS"
	  (dolist (p (cdr (assoc "NARROW CONCEPTS" pointers)))
	    (setq h (gethash p mandoku-tls-concepts))
	    (if h
		(if (plist-get h :up)
		    (puthash p (plist-put h :up (cons name (plist-get h :up))) mandoku-tls-concepts))
	      (puthash p (plist-put h :up (cons name nil)) mandoku-tls-concepts)))
	  ;; and finally add the data to the hashtable
	  (setq plist (plist-put plist :pointers (reverse pointers)))
	  (setq plist (plist-put plist :words words))
	  (setq h (gethash name mandoku-tls-concepts))
	  (setq plist (plist-put plist :up (plist-get h :up)))
	  (puthash name plist mandoku-tls-concepts))
	(kill-buffer)
)))  
)  

(defun mandoku-tls-read-subtree-list ()
  (save-excursion
    (let ((p (point))
	  (limit (org-end-of-subtree))
	  l )
      (goto-char p)
      (push (org-get-heading) l)
      (while (re-search-forward "tls:concept:\\([^\]]+\\)\]" limit t)
	(push (org-match-string-no-properties 1) l))
      (reverse l))))

(defun mandoku-tls-add-uplink-to-concepts ()
  "Read the mandoku-tls-concept table and add the uplinks to the concepts mentioned."
  (unless mandoku-tls-initialized-p
    (mandoku-tls-initialize))
  (let (l)
    (maphash (lambda (k v)
	       (when (plist-get v :up)
		 (setq x (plist-get v :up))
		 (setq l (cons (list k x) l )))) mandoku-tls-concepts)
    (dolist (e l)
      (message (car e))
      (with-current-buffer (find-file-noselect (concat mandoku-tls-lexicon-path "/" (car e) ".org") t)
	(when (search-forward "\n** POINTERS" nil t)
	  (next-line 1)
	  (insert "UP: ")
	  (dolist (f (cadr e))
	    (insert (format  "[[tls:concept:%s][%s]]\n" f f)))
	  (save-buffer)
	  (kill-buffer)))))
)

(defun mandoku-tls-helm-concepts-candidates ()
  "Helm source for concepts"
  (let (l x)
    (maphash (lambda (k v)
	       (push k l)
	       (when (plist-get v :synonyms)
		 (dolist (x (plist-get v :synonyms))
		   (push (concat x "->" k) l))
		 )
	       
	       ) mandoku-tls-concepts)
    (sort l 'string-lessp)
  )
  )

(defun mandoku-tls-helm-tree-candidates (concept)
  "Helm source for concept tree"
  (let (l x)
    (setq x concept)
    (push x l)
    (while (gethash x mandoku-tls-concepts)
      (setq x (car (plist-get (gethash x mandoku-tls-concepts) :up)))
      (push x l)
      )
					;(sort (delq nil  l) 'string-lessp)
    
    (delq nil l))
  )

(defun mandoku-tls-concepts-helm ()
  (interactive)
  (unless mandoku-tls-initialized-p
    (mandoku-tls-initialize))
  (let ((mandoku-tls-helm-source
      '((name . "TLS Concepts")
        (candidates . mandoku-tls-helm-concepts-candidates)
	;; (persistent-action . (lambda (candidate)
	;; 			    (message-box
	;; 			     (mapconcat 'identity (tls-helm-tree-candidates candidate) "\n")
	;; 			     )))
        (action . (("Open" . (lambda (candidate)
			       (setq mandoku-tls-concept (or (cadr (split-string candidate "->")) candidate))
			       (find-file
				(concat mandoku-tls-lexicon-path "concepts/" mandoku-tls-concept ".org") t)
			       (message "%s" candidate))))
		)))
	(fallback-source '((name . "Create new concept")
                          (dummy)
                          (action . mandoku-tls-create-new-annot)) ))

    (helm :sources '(mandoku-tls-helm-source fallback-source))))


(global-set-key (kbd "M-s t") 'mandoku-tls-concepts-helm)


(defun mandoku-tls-helm-syn-func-candidates ()
  "Helm source for syntactic function"
  (let (l x)
    (maphash (lambda (k v)
	       (setq l (cons (list (plist-get v :name) k) l))
	       ) mandoku-tls-syn-func-tab)
   (sort l 'mandoku-tls-sort-by-car)
   ))

(defun mandoku-tls-sort-by-car (s1 s2)
  (string-lessp (car s1) (car s2))
  )

(defun mandoku-tls-syn-func-helm ()
  (interactive)
  (let ((mandoku-tls-syn-func-source
      '((name . "TLS Syntactic functions")
        (candidates . mandoku-tls-helm-syn-func-candidates)
        (action . (("Open" . (lambda (candidate)
			       (org-open-file
				(concat mandoku-tls-lexicon-path "core/syn-func.org") t nil
				(concat "#" (car candidate))))))
		))))
    (helm :sources '(mandoku-tls-syn-func-source))))



(defun mandoku-tls-initialize ()
  (unless mandoku-tls-initialized-p
    (message "Reading the TLS database.  This will take a moment.")
    (mandoku-tls-read-swl)
    (mandoku-tls-read-synwords)
    (mandoku-tls-read-concepts)
    (mandoku-tls-read-syn-func-org)
    (setq mandoku-tls-initialized-p t)
    (message "Finished reading the TLS database.  Ready to go!"))
)    

(defun mandoku-tls-show-words (&optional uuid)
  "Show the words related to the current heading"
  (interactive)
  (let ((uuid (or uuid (org-entry-get (point) "CUSTOM_ID")))
	(hw (concat (if (= 4 (org-outline-level))
		(save-excursion
		  (outline-up-heading 1)
		  (car (split-string (org-get-heading))) ))
		" "
	      (org-get-heading)))
	(result-buffer (get-buffer-create "*Dict Result*"))
	(the-buf (current-buffer)))
    (set-buffer result-buffer)
    (setq buffer-file-name nil)
    (read-only-mode 0)
    (erase-buffer)
    (insert "* " (if hw hw "") "\n")
    (dolist (el (gethash uuid mandoku-tls-pos-info))
      (mandoku-tls-print-elements el))
    (dolist (el (gethash uuid mandoku-tls-swl))
      (mandoku-tls-print-elements (plist-get el :line)))
    (insert "\n")
    (mandoku-dict-mode)
    (hide-sublevels 2)
    (goto-char (point-min))
    (switch-to-buffer-other-window result-buffer t)
))

(defun mandoku-tls-procline (&optional inp) 
  (interactive
   (if (use-region-p)
       (list (buffer-substring-no-properties (region-beginning) (region-end))))
   )  
  (unless mandoku-tls-initialized-p
    (mandoku-tls-initialize))
  (when (derived-mode-p 'mandoku-view-mode)
    (setq mandoku-position (mandoku-position-at-point-internal))
    (set-marker mandoku-position-marker (point))
    )
  (let ((inp (mandoku-remove-punct-and-markup (or inp (mandoku-get-line))))
	(result-buffer (get-buffer-create "*Dict Result*"))
	(the-buf (current-buffer))
	(pos    (mandoku-position-at-point))
	i j tmp res)
    (set-buffer result-buffer)
    (setq mandoku-tls-inp inp)
    (setq buffer-file-name nil)
    (read-only-mode 0)
    (erase-buffer)
    (insert "* " (if pos pos "") " " inp "\n")
    (setq i 0)
    (while (< i (length inp))
      (setq j (+ i 1))
      (setq tmp (substring inp i j))
      (setq res (gethash tmp mandoku-tls-words))
      (while (and (< j (length inp)) res)
	(setq j (+ j 1))
	(setq tmp (substring inp i j))
	(setq res (gethash tmp mandoku-tls-words))
	)
      (unless res
	(setq tmp (substring inp i (-  j 1)))
	(setq res (gethash tmp mandoku-tls-words)))
      (if  (< 0 (length tmp))
	  (progn (insert "
** " tmp  )
      (dolist (el (gethash tmp mandoku-tls-words))
	(mandoku-tls-print-elements el))
      (insert "\n")
      ))
      (setq i (+ i 1)))
    (mandoku-dict-mode)
    (hide-sublevels 2)
    (goto-char (point-min))
    (switch-to-buffer-other-window result-buffer t)))

(defun mandoku-tls-make-attribution (&optional att)
  "This makes an attribution for the current line"
  (interactive)
  (let* ((inp mandoku-tls-inp)
	 mandoku-tls-concept
	 (start (- (string-to-int (read-from-minibuffer (concat inp ": Please enter position of first character to use: ") "1" nil )) 1))
	 (slength (string-to-int (read-from-minibuffer (concat inp ": Please enter number of characters to use: ") "1" nil )))
	 (word (substring inp start (+ start slength)))
	 ;; helm source for concepts
	 (mandoku-tls-helm-source
	  '((name . "TLS Concepts")
	    (candidates . mandoku-tls-helm-concepts-candidates)
	    (action . (("Select" . (lambda (candidate)
			       (setq mandoku-tls-concept (or (cadr (split-string candidate "->")) candidate))))))))
	 (fallback-source '((name . "Create new concept")
			    (dummy)
			    (action . mandoku-tls-create-new-annot)) )
	 ;;
	 concept readings)
    (helm :sources '(mandoku-tls-helm-source fallback-source))
    (find-file
     (concat mandoku-tls-lexicon-path "concepts/" mandoku-tls-concept ".org") t)
    (hide-sublevels 3)
    (unless (search-forward word nil t)
      (goto-char (point-max)))
    ;; more characters!
    (dolist (w (string-to-list word))
      (helm-charinfo (format "%c" w))
      (push (cadr(split-string helm-charinfo-selected)) readings))
    (insert "\n*** "
	    word
	    " "
	    (mapconcat 'identity (reverse readings) " ")
	    "\n")
    (mandoku-tls-new-uuid-for-heading)
    (mandoku-tls-new-syntactic-word)
  )
  )

(defun mandoku-tls-new-swl ()
  "Create a new syntactic word location.  If necessary, also create concept, syntactic word or syntactic function."
  (interactive)
  (let ((hw (org-get-heading))
	char
	swl-line)
    (if (string-match "tls:concept:[^#-]+#uuid" hw)
	(progn
	  (setq char (save-excursion
		  (outline-up-heading 1)
		  (org-get-heading)))
	  (setq swl-line (concat char " " hw))
	  (mandoku-tls-insert-new-annot swl-line)
      
	  )
      ))
  )

(defun mandoku-tls-create-new-annot (concept &optional word)
  "Ann is a plist which contains the necessary information."
  (interactive)
  (with-current-buffer (find-file-other-window (concat mandoku-tls-lexicon-path "concepts/" (upcase concept) ".org"))
      (insert-file-contents mandoku-tls-concept-template)
      (goto-char (point-min))
      (search-forward "=CONCEPT= ")
      (goto-char (match-end 0))
      (insert (upcase concept))
      (search-forward ":CUSTOM_ID: ")
      (goto-char (match-end 0))
      (insert (mandoku-tls-id-new))
      (when word
	(search-forward "** WORDS")
	(forward-line 1)
	(insert "*** " word "\n")
	(mandoku-tls-new-uuid-for-heading)
	)
      (goto-char (point-min))
      (search-forward "** DEFINITION")
      (forward-line 1)
      (save-buffer)
      (message "Please enter the definition")
    ))

(defun mandoku-tls-new-syntactic-word (&optional syn-func def)
  "For an existing Lexeme, we add a syntactic word (usage instance)."
  (interactive)
  (if syn-func
      (progn
	(insert "**** [[tls:syn-func::#" (car syn-func)  "][" (plist-get (gethash (car syn-func) mandoku-tls-syn-func-tab) :name)  "]] / ")
	(mandoku-tls-new-uuid-for-heading)
	)
    (helm :sources '((name . "TLS Syntactic functions")
		     ;(candidates . mandoku-tls-syn-func-list)
		     (candidates . mandoku-tls-helm-syn-func-candidates)
		     (action . (("Select" . mandoku-tls-new-syntactic-word)))))))

 

(defun mandoku-tls-new-uuid-for-heading ()
  (interactive)
  (unless (org-entry-get (point) "CUSTOM_ID" nil)
  (org-set-property "CUSTOM_ID" (mandoku-tls-id-new))))

(defun mandoku-tls-compose-new-annot()
  (let ((concept
	 (replace-in-string
	  (save-excursion
	    (goto-char (point-min))
	    (search-forward "=CONCEPT= ")
	    (org-get-heading))
	  "=CONCEPT= " ""))
	(char (save-excursion
		  (outline-up-heading 1)
		  (org-get-heading)))
	)
    (mandoku-string-remove-all-properties (format "%s\t[[tls:concept:%s::#%s][%s]]\t%s" char concept
	    (org-entry-get (point) "CUSTOM_ID" nil)
	    concept (org-get-heading)))
  ))

(defun mandoku-tls-insert-new-annot (&optional ann)
  "Insert the annotation"
  (interactive)
  (let ((ann (or ann (mandoku-tls-compose-new-annot)))
	(case-fold-search t))
    (with-current-buffer (marker-buffer mandoku-position-marker)
      (goto-char (marker-position mandoku-position-marker))
      (forward-line)
      (beginning-of-line)
      (if (looking-at ":zhu:")
	(progn
	  (re-search-forward ":end:")
	  (beginning-of-line)
	  (insert ann
		  "\n" )
	  (previous-line))
      (progn
	(insert ":zhu:\n \n:end:\n")
	(previous-line 2)
	(beginning-of-line)
	(insert ann)))
    (beginning-of-line)
    (deactivate-mark)
    (save-buffer)
    )
))


(defun mandoku-tls-print-elements (list)
  "Print each element of LIST on a line of its own."
  (insert "
*** ")
  (if (listp list)
      (insert (mapconcat 'identity list " "))
    (insert list))
  (insert "\n"))
;  (setq list (cdr list)))


;; add the swl
(defun mandoku-tls-swl-tab-change (state)
  (interactive)
  (cond 
   ((and (eq major-mode 'mandoku-dict-mode)
	     (memq state '(children subtree)))
    (save-excursion
      (ignore-errors
      (let ((uuid (car (split-string (cadr (split-string  (org-get-heading) "#")) "\\]\\["))))
	(forward-line)
	(if (looking-at "\n")
	    (dolist (i (gethash uuid mandoku-tls-swl))
	      (insert (concat (plist-get i :line) "\n")))
	    (message uuid))))))))

(add-hook 'org-cycle-hook 'mandoku-tls-swl-tab-change) 

;; add link type tls
(org-add-link-type "tls" 'mandoku-tls-follow-link)
;(add-hook 'org-store-link-functions 'org-dic-store-link)


(defun mandoku-tls-follow-link (link)
  "Follow the tls link"
; links for concept, syn-func, sem-feat  
  (let* ((type (car (split-string link ":")))
	 (rest (cdr (split-string link ":")))
	 )
    (cond 
     ((equal type "concept")
      (message type)
      (org-open-file (concat mandoku-tls-lexicon-path "concepts/" (car rest) ".org") t nil (nth 2  rest))
      )
     ((or (equal type "syn-func") (equal type "sem-feat"))
      (message type)
      (org-open-file (concat mandoku-tls-lexicon-path "core/" type ".org") t nil (cadr  rest))
      )
     ((equal type "text")
      (message type)
      (org-open-file (concat mandoku-tls-text-path "/" (car rest) "_" (car (split-string (cadr rest) "-")) ".txt") t nil (cadr  rest))
      )
     )
    )
)

;;* font lock for org-ref

(defcustom mandoku-tls-colorize-links
  t
  "When non-nil, change colors of links."
  :type 'boolean
  :group 'mandoku-tls)


(defcustom mandoku-tls-cite-color
  "forest green"
  "Color of cite like links."
  :type 'string
  :group 'mandoku-tls)


(defcustom mandoku-tls-ref-color
  "dark red"
  "Color of ref like links."
  :type 'string
  :group 'mandoku-tls)


(defcustom mandoku-tls-label-color
  "dark magenta"
  "Color of label links."
  :type 'string
  :group 'mandoku-tls)


(defvar mandoku-tls-re
  (concat "\\(" (mapconcat
                 (lambda (x)
		   (replace-regexp-in-string "\*" "\\\\*" x))
                 mandoku-tls-types "\\|") ":\\)"
                 "\\([a-zA-Z0-9-_:\\./]+,?\\)+")
  "Regexp for cite links.")


(defvar mandoku-tls-label-re
  "label:\\([a-zA-Z0-9-_:]+,?\\)+"
  "Regexp for label links.")


(defvar mandoku-tls-ref-re
  "\\(eq\\)?ref:\\([a-zA-Z0-9-_:]+,?\\)+"
  "Regexp for ref links.")


(defface mandoku-tls-cite-face
  `((t (:inherit org-link
                 :foreground ,mandoku-tls-cite-color)))
  "Color for cite-like links in org-ref.")


(defface mandoku-tls-label-face
  `((t (:inherit org-link :foreground ,mandoku-tls-label-color)))
  "Color for ref links in org-ref.")


(defface mandoku-tls-ref-face
  `((t (:inherit org-link :foreground ,mandoku-tls-ref-color)))
  "Face for ref links in org-ref.")

;;

(defun mandoku-tls-id-new ()
  "Get the id in a compatible way."
  (replace-in-string (downcase (org-id-new "uuid")) ":" "-")) 

;; (defun mandoku-tls-hash-to-list (hashtable)
;;   "Return a list that represent the HASHTABLE."
;;   (let (myList)
;;     (maphash (lambda (kk vv) (setq myList (cons (list (plist-get vv :name) kk ) myList))) hashtable)
;;     myList
;;   )
;; )    

    
(provide 'mandoku-tls)



;;;; here is how to read the hash
;; (setq tx (gethash "意" mandoku-tls-words))
;; (while tx
;;   (setq ty (car tx))
;;   (setq tx (cdr tx))
;;   (if (listp ty)
;;       (insert (format "
;; ** %s %s" (car ty) (cdr ty)))
;;     (progn
;;       (insert (format "
;; ** %s %s" ty tx))
;;       (setq tx nil))))

