;;; w32-registry.el --- read the registry from elisp
;;
;; Author: Dino Chiesa
;; Created: Fri, 06 Apr 2012  15:43
;; Package-Requires: ()
;; URL: http://cheeso.members.winisp.net/srcview.aspx?dir=emacs&file=w32-registry.el
;; X-URL: http://cheeso.members.winisp.net/srcview.aspx?dir=emacs&file=w32-registry.el
;; Version: 2012.4.9
;; Keywords: w32 registry chakra jsshell jslint jshint csslint javascript ie
;; License: New BSD

;;; Commentary:

;; This module provides functions to read and update the Windows
;; registry, using the reg.exe command-line tool.  The former is done by
;; parsing the output of the tool and formatting it into an s-expression.

;; Also, there are convenience functions to read or update well-known
;; registry values.

;; - `w32reg-get-ie-proxy-config' is suitable for reading the HTTP proxy
;;   configuration for IE for the current user. This is also useful for
;;   setting `url-proxy-services', which is used by the `url.el' package.

;; - `w32reg-maybe-expose-chakra' updates the registry to expose the IE9
;;   Chakra Javascript engine to CScript.exe. This allows javascript
;;   things like jsshell, jslint, jshint, and csslint to run faster.

;; I will add more of these convenience functions later.



;;; Revisions:

;; 2012.4.9  2012-Apr-09  Dino Chiesa
;;    corrected `w32reg-read-key' to handle keys with no values.  Also
;;    added two new functions: `w32reg-insert-value' which inserts a
;;    registry value, and `w32reg-maybe-expose-chakra', which
;;    conditionally updates the registry with the values required to
;;    expose IE9's Chakra EcmaScript engine to CScript.exe.
;;
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

(if (eq system-type 'windows-nt)
    (progn

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

(defvar w32reg-chakra-clsid "16d51579-a30b-4c8b-a276-0ff4dc41e755"
  "CLSID for the Chakra javascript engine that is installed with IE9
and later.")

(defun w32reg-read-key (key)
  "Read all values and subkeys for a key path in the Windows
registry.  The return value is a list (KEYNAME VALUES SUBKEYS).
KEYNAME is the name of the key. VALUES is a list of values, each
one following this form: (NAME TYPE VALUE) where each are
strings, and the TYPE is like \"REG_DWORD\" and so on.

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
            (if (not (string/starts-with line " "))
                (if (> (length values) 0)
                    ;; sanity
                    (error "error processing output.")
                  ;; whoops, there are no values for this key.
                  (let ((parts (split-string keyname "\\\\" t)))
                    (setq state 2
                          subkeys (cons (car (last parts)) subkeys)
                          keyname (mapconcat 'identity (butlast parts) "\\"))))

            (let ((parts (split-string line nil t)))
              (setq this-value (mapconcat 'identity (cddr parts) " "))

              ;; convert to integer, maybe
              (if (string= (nth 1 parts) "REG_DWORD")
                  (setq this-value
                        (string-to-number (substring this-value 2))))

              (setq values (cons (list (nth 0 parts)
                                       (nth 1 parts)
                                       this-value) values)))))

           ((eq state 2)
            (setq subkeys (cons
                           (if (string/starts-with line keyname)
                               (substring line (1+ (length keyname)))
                             line)
                           subkeys)))
           (t nil)))))

    (and keyname
         (list keyname values subkeys))))


(defun w32reg-read-value (key &optional value)
  "From a KEY location in the registry, read a VALUE. The result
is a list like (NAME TYPE VALUE), where the first two items are
strings.  TYPE is like \"REG_DWORD\", \"REG_SZ\", \"REG_BINARY\",
and so on.  If TYPE is \"REG_DWORD\", then the VALUE is a number.

If the VALUE is nil, then it reads the default value for the KEY.

If the key value does not exist, it returns nil.
"
  (let ((all (w32reg-read-key key))
        (value (or value "(Default)"))
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



(defun w32reg-insert-value (key &optional value value-name type)
  "Insert, at the given KEY in the registry, the VALUE with the
given VALUE-NAME.  If the VALUE is nil, then the default value
for the string is set to an empty string.  If the VALUE-NAME is
nil then the default value is set for the key. The TYPE is a
string that specifies the data type, and should be one of:

    REG_SZ    | REG_MULTI_SZ | REG_EXPAND_SZ |
    REG_DWORD | REG_QWORD    | REG_BINARY    | REG_NONE

The KEY should begin with one of

      HKLM | HKCU | HKCR | HKU | HKCC

...and should entail the full key path. Be careful to use
double-backslashes.

"
  (let ((reg.exe (concat (getenv "windir") "\\system32\\reg.exe"))
        cmd
        (tfile (concat
                (make-temp-name (concat
                                 (file-name-as-directory temporary-file-directory)
                                 "emacs.reg.exe." ))
                ".out")))

    ;;with-temp-buffer
    (with-temp-file tfile
      (setq cmd
            (concat reg.exe " add " "\"" key "\" "
                    (if value-name (concat "/v \"" value-name "\" ")
                      "/ve ")
                    (if type (concat "/t " type " ")
                      " ")
                    "/d "
                    (if value (concat "\"" value "\"")
                      "\"\"")
                    " /f"))
      (insert (shell-command-to-string cmd)))))



(defun w32reg-maybe-expose-chakra ()
  "It is possible for CScript.exe to use the \"Chakra\" engine
from IE9, from a cscript.exe command. The benefit is, Javascript
programs run faster. For emacs users, this means things like csslint,
jslint, or jshint all run in about half the time as required for
\"regular\" Javascript.  This is nice for a scenario in which
the checker runs very often, as with flymake.

To make Chakra available, just invoke this function.

This function checks for the IE9 JScript dll, and if it finds it,
modifies the registry to enable CScript.exe to use Chakra under
the scripting engine name, \"Chakra\".

flymake-for-jshint, the csslint module, and jsshell are smart
enough to use chakra when it is available.

"
  (interactive)
  (let* ((key1 (format "HKLM\\SOFTWARE\\Classes\\CLSID\\{%s}\\InprocServer32"
                          w32reg-chakra-clsid))
         (value1 (w32reg-read-value key1)))

    (if (and value1
             (file-exists-p (nth 2 value1)))
        (let* ((key2 (format
                      "HKLM\\SOFTWARE\\Classes\\CLSID\\{%s}\\ProgID"
                      w32reg-chakra-clsid))
               (value2 (w32reg-read-value key2)))

          (if (and value2
                   (string= (nth 2 value2) "Chakra"))
              (message "Chakra is already available as a scripting engine.")

            (w32reg-insert-value key2 "Chakra")
            (w32reg-insert-value "HKLM\\SOFTWARE\\Classes\\Chakra" "EcmaScript5 Language")
            (w32reg-insert-value "HKLM\\SOFTWARE\\Classes\\Chakra\\CLSID"
                                 (format "{%s}" w32reg-chakra-clsid))
            (w32reg-insert-value "HKLM\\SOFTWARE\\Classes\\Chakra\\OLEScript" )

            ;; Conditionally update the wow6432Node
            (let* ((key3 "HKLM\\SOFTWARE\\Classes\\Wow6432Node")
                   (value3 (w32reg-read-value key3)))
              (when value3
                  (setq key3 (format "%s\\CLSID\\{%s}\\ProgID" key3 w32reg-chakra-clsid))
                  (w32reg-insert-value key3 "Chakra")))

            (message "Chakra is now exposed for use by CScript.exe")))
      (message "JScript9.dll is apparently not installed on this computer."))))




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
))


(provide 'w32-registry)

;;; w32-registry.el ends here
