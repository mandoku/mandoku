;; init file for mandoku
(require 'mandoku)
(require 'mandoku-remote)
(require 'org-mandoku)

(setq mandoku-repositories-alist '(("ZB" . "http://www.kanripo.org/zb")))

(setq org-return-follows-link t)

(setq mandoku-do-remote t)


(setq mandoku-image-dir (expand-file-name  (concat mandoku-base-dir "images/")))
(setq mandoku-index-dir (expand-file-name  (concat mandoku-base-dir "index/")))
(setq mandoku-meta-dir (expand-file-name  (concat mandoku-base-dir "meta/")))
(setq mandoku-sys-dir (expand-file-name  (concat mandoku-base-dir "system/")))
(setq mandoku-temp-dir (expand-file-name  (concat mandoku-base-dir "temp/")))

;; need to -install -> write mandoku-local-init -> load mandoku-local-init

;; dic
;(setq mandoku-dict-img-dir "/Users/Shared/md/images/dic/")
(setq mandoku-dict-img-dir nil)
(ignore-errors 
(load "mandoku-dict" t)
(global-set-key [f5] 'mandoku-dict-get-line)
)
(global-set-key [f6] 'mandoku-search-text)
(global-set-key [S-f6] 'mandoku-search-titles)

(setq mandoku-dict-url "http://www.kanripo.org/zb")

(setq mandoku-string-limit 20)

(setq mandoku-catalog (concat mandoku-meta-dir "mandoku-catalogx.txt"))

(unless (file-exists-p mandoku-catalog)
  (with-current-buffer (find-file-noselect mandoku-catalog)
    (insert "#-*- mode: mandoku-view; -*-
#+DATE: 2013-09-05
#+TITLE: 漢籍リポジトリ目録

# このファイルは自動作成しますので、編集しないでください
# This file is generated automatically, so please do not edit

リンクをクリックするかカーソルをリンクの上に移動して<enter>してください
Click on a link or move the cursor to the link and then press enter

")

    (dolist (x (sort mandoku-catalogs-alist (lambda (a b) (string< (car a) (car b)))))
      (insert 
       (format "* [[file:%s][%s %s]]\n" 
	       (cdr x) 
	       (car x)
	       (gethash (substring (car x) 2)  mandoku-subcolls))))
    (save-buffer)
    )
  )


    
(unless mandoku-catalogs-alist
  (dolist (dir (directory-files mandoku-meta-dir nil "^[^.,].*"))
    (when (file-directory-p (concat mandoku-meta-dir dir))
      (dolist (file (directory-files (concat mandoku-meta-dir dir) nil ".txt" ))
	(add-to-list 'mandoku-catalogs-alist 
		     (cons (file-name-sans-extension file) (concat mandoku-meta-dir dir "/" file)))))))
      
	    
(mandoku-read-titletables) 
 
(setq mandoku-initialized t)
(message "Loading of local setup for Mandoku finished")

;; mandoku-init ends here
