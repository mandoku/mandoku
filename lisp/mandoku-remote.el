;; mandoku.el   -*- coding: utf-8 -*-
;; created [2013-08-30T11:41:45+0900] chris
;; mandoku library for handling requests to remote host

(require 'url-http)
;; probably best to make this a list sometime...
(defvar mandoku-remote-url "http://localhost:5000/search")
(setq mandoku-remote-url "http://127.0.0.1:5000/search")

(defun mandoku-search-remote (search-string index-buffer)
  (with-current-buffer index-buffer 
    (url-insert-file-contents (concat mandoku-remote-url "?query=" search-string)
			      (lambda (status) (switch-to-buffer (current-buffer))))))


(defun mandoku-open-remote-file (filename)
  
