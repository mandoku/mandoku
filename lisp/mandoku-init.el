;;; mandoku-init.el --- Default user settings for mandoku
;; -*- coding: utf-8 -*-

;; This file has some default settings for new users of mandoku.
;; This also includes settings for other packages.
;; Please adapt the file to your needs.

;;; mandoku-base-dir
;(setq mandoku-base-dir "~/krp")
;;; 

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(setq use-package-always-ensure t)

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(prefer-coding-system 'utf-8)
(mac-auto-ascii-mode 1)

(use-package magit
  :bind ("C-x g" . magit-status)
  :ensure t)

(use-package gh
  :ensure t)

(use-package github-clone
  :init (setq github-clone-url-slot :clone-url)
  :ensure t)

(use-package mandoku
  :init (setq org-return-follows-link t)
  (setq org-startup-folded 'showeverything)
  (setq mandoku-do-remote t)
  (setq mandoku-string-limit 10)
  (setq mandoku-index-display-limit 2000)
  (setq mandoku-repositories-alist '(("KR" "http://www.kanripo.org/api/v1.0") ))
  (autoload 'mandoku-view-mode "mandoku" nil t)
  :config
  (define-key mandoku-view-mode-map (kbd "C-c i")  'mandoku-open-image-at-page)
  (define-key mandoku-view-mode-map (kbd "C-c d")  'mandoku-get-remote-text-now)

  :bind (("C-c i" . mandoku-insert-image )
	 ("<f6>" . mandoku-search-text)
	 ("<f7>"  . mandoku-search-titles)  )
)

(use-package guide-key
  :defer 5
  :diminish guide-key-mode
  :config
  (progn
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
  (guide-key-mode 1)))  ; Enable guide-key-mode

