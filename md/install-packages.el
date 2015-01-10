;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Package manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl)
(require 'package)

(setq package-archives
      '(("org"         . "http://orgmode.org/elpa/")
	("gnu"         . "http://elpa.gnu.org/packages/")
;	("original"    . "http://tromey.com/elpa/")	
	("melpa" . "http://melpa.milkbox.net/packages/")
;	("marmalade"   . "http://marmalade-repo.org/packages/")
	))

(add-to-list 'package-archives '("sandbox" . "/Users/chris/Dropbox/netwalker/packages") t)

(setq package-user-dir (expand-file-name "elpa"  starter-kit-dir))

(package-initialize)

(defvar starter-kit-packages
  (list 'flx-ido 'ido-ubiquitous 'smex
        'yasnippet
        'magit 
;	'bbdb 'bbdb-ext
;        'auctex 'reftex
;        'undo-tree
	'diminish
	'json
	;'icicles
	'org 'org-plus-contrib
	'mandoku 'mandoku-meta-zb
;	'elpy
;        'rainbow-mode
	)
  "Libraries that should be installed by default.")

(unless (every #'package-installed-p starter-kit-packages)
  (package-refresh-contents)
  (dolist (package starter-kit-packages)
    (unless (package-installed-p package)
      (message "installing %s" package)
      (package-install package))))

(provide 'install-packages)
