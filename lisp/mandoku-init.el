;; init file for mandoku
(require 'mandoku)
(require 'org-mandoku)

;; (dolist (dir (directory-files mandoku-text-dir nil "^[^.,].*"))
;;   (when (file-directory-p (concat mandoku-text-dir dir))
;;     (load (concat "mandoku-support-" dir) t )))

;; (setq mandoku-catalogs-alist '(
;; 			       ("ZB1 經部" . (eval `(concat mandoku-meta-dir "ZB/" "ZB1.org")))
;; 			       ("ZB2 史部" . (concat mandoku-meta-dir "ZB/" "ZB2.org"))
;; 			       ("ZB3 子部" . (concat mandoku-meta-dir "ZB/" "ZB3.org"))
;; 			       ("ZB4 集部" . (concat mandoku-meta-dir "ZB/" "ZB4.org"))
;; 			       ("ZB6 佛部" . (concat mandoku-meta-dir "ZB/" "ZB6.org"))
;; ))

(setq mandoku-catalogs-alist '(
			       ("ZB1 經部" . "/Users/chris/md/meta/ZB/ZB1.org")
			       ("ZB2 史部" . "/Users/chris/md/meta/ZB/ZB2.org")
			       ("ZB3 子部" . "/Users/chris/md/meta/ZB/ZB3.org")
			       ("ZB4 集部" . "/Users/chris/md/meta/ZB/ZB4.org")
			       ("ZB6 佛部" . "/Users/chris/md/meta/ZB/ZB6.org")
))

    

;(dolist (x mandoku-catalogs-alist)
;  (message "%s" (cdr x)))

(global-set-key "\M-\_" 'mandoku-annotate)
;(setq mandoku-index-dir (expand-file-name "~/00scratch/index-skqs/"))
;(setq mandoku-index-dir (expand-file-name "/tmp/index/"))
;(setq mandoku-index-dir (expand-file-name "~/00scratch/index/"))
;(setq mandoku-index-dir "/Users/chris/tmp/index/")
;(setq mandoku-index-dir (expand-file-name  (concat mandoku-base-dir "index/")))

