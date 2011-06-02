;; init file for mandoku
(require 'mandoku)
(require 'org-mandoku)
(load "mandoku-support-hist")
(load "mandoku-support-cbeta")
(load "mandoku-support-dz")
(load "mandoku-support-skqs")

(global-set-key "\M-\_" 'mandoku-annotate)
