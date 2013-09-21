;; init file for mandoku
(require 'mandoku)
(require 'mandoku-remote)
(require 'org-mandoku)

(setq mandoku-repositories-alist '(("ZB" . "http://www.kanripo.org/zb")))

;; (setq mandoku-catalogs-alist '(
;; 			       ("ZB1 經部" . "/Users/Shared/md/meta/ZB/ZB1.org")
;; 			       ("ZB2 史部" . "/Users/Shared/md/meta/ZB/ZB2.org")
;; 			       ("ZB3 子部" . "/Users/Shared/md/meta/ZB/ZB3.org")
;; 			       ("ZB4 集部" . "/Users/Shared/md/meta/ZB/ZB4.org")
;; 			       ("ZB6 佛部" . "/Users/Shared/md/meta/ZB/ZB6.org")
;; ))


(setq org-return-follows-link t)

(setq mandoku-do-remote t)

;(setq mandoku-base-dir (expand-file-name  "/Users/Shared/md-remote/"))
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

(global-set-key "\M-\_" 'mandoku-annotate)
;(setq mandoku-index-dir (expand-file-name "~/00scratch/index-skqs/"))
;(setq mandoku-index-dir (expand-file-name "/tmp/index/"))
;(setq mandoku-index-dir (expand-file-name "~/00scratch/index/"))
;(setq mandoku-index-dir "/Users/chris/tmp/index/")
;(setq mandoku-index-dir (expand-file-name  (concat mandoku-base-dir "index/")))

;(setq mandoku-index-dir "/Users/Shared/md-remote/index/")
;(setq mandoku-text-dir   "/Users/Shared/md-remote/text/")
;; /Volumes/verbatimssd/db/images/skqs/s1/64/64268/06064268-055.tif
;; this part will be generated : s1/64/64268/
;; from this filename: 06064268-055.tif
(setq mandoku-string-limit 20)
(setq mandoku-sqks-img-root "/Volumes/verbatimssd/db/images/skqs/")

(setq mandoku-catalog (concat mandoku-meta-dir "mandoku-catalog.txt"))

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
(define-key mandoku-title-list-mode-map  "t" 'mandoku-title-list-goto-text)
(define-key mandoku-title-list-mode-map "c" 'mandoku-title-list-goto-catalog)
(define-key mandoku-title-list-mode-map "i" 'mandoku-title-list-goto-catalog)
 
(setq mandoku-initialized t)

;; mandoku-init ends here
