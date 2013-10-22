;;; w32-registry.el --- read the registry from elisp
;;
;; Author: Dino Chiesa
;; Created: Fri, 06 Apr 2012  15:43
;; Package-Requires: ()
;; URL: http://cheeso.members.winisp.net/srcview.aspx?dir=emacs&file=w32-registry.el
;; X-URL: http://cheeso.members.winisp.net/srcview.aspx?dir=emacs&file=w32-registry.el
;; Version: 2012.4.6
;; Keywords: w32 registry
;; License: New BSD

;;; Commentary:

;; This module provides functions to read the Windows registry, by
;; parsing the output of the reg.exe command-line tool and formatting it
;; into something a lisp program can understand.

;; Also, there are convenience functions to read well-known registry
;; values. Well, there's one anyway.

;; - `w32reg-get-ie-proxy-config' is suitable for setting
;;   url-proxy-services, which is used by the `url.el' package.

;; I will add more of these convenience functions later.



;;; Revisions:

;; 2012.4.6  2012-Apr-06  Dino Chiesa
;;    initial creation.
;;


;;; License
;;
;; This code is distributed under the New BSD License.
;;
;; Copyright (c) 2008-2012, Dino Chiesa
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.
;;
;; Neither the name of the author or any contributors, nor the names of
;; any organizations they belong to, may be used to endorse or promote
;; products derived from this software without specific prior written
;; permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
;; OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
;;

(if (not (fboundp 'string/ends-with))
    (defun string/ends-with (s ending)
      "return non-nil if string S ends with ENDING"
      (let ((elength (length ending)))
        (string= (substring s (- 0 elength)) ending))))

(if (not (fboundp 'string/starts-with))
    (defun string/starts-with (s arg)
      "returns t if string S starts with ARG.  Else nil."
      (cond ((>= (length s) (length arg))
             (string-equal (substring s 0 (length arg)) arg))
            (t nil))))


(defun w32reg-read-key (key)
  "Read all values and subkeys for a key path in the Windows registry.
The return value is a list (KEYNAME VALUES SUBKEYS).  KEYNAME is
the name of the key. VALUES is a list of values, each one
following this form: (NAME TYPE VALUE) where each are strings,
and the TYPE is like \"REG_DWORD\" and so on.

SUBKEYS is a simple list of strings.

If the path does not exist, it returns nil.
"
  (let ((reg.exe (concat (getenv "windir") "\\system32\\reg.exe"))
        keyname values subkeys (state 0))

    (with-temp-buffer
      (insert (shell-command-to-string
               (concat reg.exe " query " "\"" key "\"")))

      (while (not (= (point-min) (point-max)))
        (goto-char (point-min))
        (let ((start (point))
              (end (line-end-position))
              line this-value)
          (setq line (buffer-substring-no-properties start end))
          (delete-region start end)
          (delete-char 1) ;; NL

          (cond
           ((string/starts-with line "ERROR:")
            nil)

           ((string= "" line)
            (setq state (1+ state)))

           ((not keyname)
            (setq keyname line
                  state 1))

           ((eq state 1)
            (let ((parts (split-string line nil t)))
              (setq this-value (mapconcat 'identity (cddr parts) " "))

              ;; convert to integer, maybe
              (if (string= (nth 1 parts) "REG_DWORD")
                  (setq this-value
                        (string-to-number (substring this-value 2))))

              (setq values (cons (list (nth 0 parts)
                                     (nth 1 parts)
                                     this-value) values))))

           ((eq state 2)
            (setq subkeys (cons
                           (if (string/starts-with line keyname)
                               (substring line (1+ (length keyname)))
                             line)
                           subkeys)))

           (t nil)))))

    (and keyname
         (list keyname values subkeys))))


(defun w32reg-read-value (key value)
  "Read a value from a key location in the registry. The result
is a list like (NAME TYPE VALUE), each item a string, where TYPE
is like \"REG_DWORD\", \"REG_SZ\", \"REG_BINARY\", and so on.

If the key value does not exist, it returns nil.
"
  (let ((all (w32reg-read-key key))
        (c 0) L n r values)
    (and all
         (setq values (nth 1 all)
               L (length values))
    (while (and (not r) (< c L))
      (setq n (nth c values)
            c (1+ c))
      (if (string= value (car n))
          (setq r n))))
    r))



(defun w32reg-get-ie-proxy-config ()
  "Return the Proxy Server settings configured for IE, if enabled.
The result is a list of cons cells; like this:

  ((\"http\" . \"127.0.0.1:8888\")
   (\"https\" .  \"127.0.0.1:8888\"))

...which is suitable for use with (setq url-proxy-services ...)

"
  (let* ((rpath "HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings")
         (enabled (w32reg-read-value rpath "ProxyEnable"))
         r)
    (if (and enabled
             (numberp (nth 2 enabled))
             (>  (nth 2 enabled) 0))
        (let ((proxy (w32reg-read-value rpath "ProxyServer")))
          (mapcar `(lambda (elt)
                     (let ((x (split-string elt "=" t)))
                       (cons (car x) (cadr x))))
                  (split-string (nth 2 proxy) ";" t))))))


(provide 'w32-registry)

;;; w32-registry.el ends here