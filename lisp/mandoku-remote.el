;; mandoku.el   -*- coding: utf-8 -*-
;; created [2013-08-30T11:41:45+0900] chris
;; mandoku library for handling requests to remote host

(require 'url-http)
;; probably best to make this a list sometime...
(defvar mandoku-remote-url "http://localhost:5000/search")

(defun mandoku-do-query (type str)
      (let ((url-request-method "GET")
            (arg-stuff (concat "?query=" (url-hexify-string str)
                         "&filter=" (url-hexify-string type))))
        (url-retrieve (concat mandoku-remote-url arg-stuff)
                      (lambda (status) (switch-to-buffer (current-buffer))))))

