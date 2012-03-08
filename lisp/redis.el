;;; redis.el --- Emacs List interface to the redis database
;;;
;;; Author:  Christian Wittern <cwittern@gmail.com>
;;;
;;; Version: 0.01
;;; Keywords: data comm database redis nosql
;;; Copyright: (C) 2010  Christian Wittern
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.
;;

;; comments
;; Redis datatypes:
;; Strings  string
;; Hashes   hash table
;; Integer  int
;; Lists    list
;; Sets
;; Sorted sets

;; TODO
;; encoding: Currently encoding agnostic, de facto assuming UTF-8
;; currently this is a very basic, mostly covering what I needed so far.
(require 'org)

(defvar redis-buffer "*redis-buffer*")
(defvar redis-host "localhost")
(defvar redis-port 6379)
(defvar redis-delay 0.1
"it seems that this is necessary.  If problems arise, set it to a higher value might help" )

(defvar redis-single-line-commands (split-string "PING SET SELECT SAVE BGSAVE SHUTDOWN RENAME LPUSH RPUSH LSET LTRIM"))

;; bool (that is 0 or 1):
;            'AUTH DEL EXISTS EXPIRE EXPIREAT HDEL HEXISTS HMSET MOVE MSETNX '
;            'PERSIST RENAMENX SADD SISMEMBER SMOVE SETEX SETNX SREM ZADD ZREM',  HSET
;; int:
;            'DECRBY GETBIT HLEN INCRBY LINSERT LLEN LPUSHX RPUSHX SCARD '
;            'SDIFFSTORE SETBIT SETRANGE SINTERSTORE STRLEN SUNIONSTORE ZCARD '
;            'ZREMRANGEBYRANK ZREMRANGEBYSCORE',

;; response OK?
;;             'LPUSH RPUSH',

;; response OK?
;;            'FLUSHALL FLUSHDB LSET LTRIM MSET RENAME '
;;            'SAVE SELECT SET SHUTDOWN SLAVEOF WATCH UNWATCH',


; single line reply commands:
; PING SET SELECT SAVE BGSAVE SHUTDOWN RENAME LPUSH RPUSH LSET LTRIM

; Errors are sent exactly like Single Line Replies. The only difference is that the first byte is "-" instead of "+".

; integer reply: SETNX DEL EXISTS INCR INCRBY DECR DECRBY DBSIZE LASTSAVE RENAMENX MOVE LLEN SADD SREM SISMEMBER SCARD

;(setq redis (open-network-stream "*redis*" redis-buffer "localhost" 6379))
;(process-status redis)

;; we need utf-8 !!

(add-to-list 'process-coding-system-alist '("redis" . utf-8))

;; open the connection

(defun redis-open-connection (buffer &optional host port)
  (let ((host (if (not (equal nil host)) (host) redis-host))
	(port (if (not (equal nil port)) (host) redis-port)))
    (open-network-stream "*redis*" buffer host port)))
    


;; (process-send-string
;;  redis
;;  (format(concat
;; 	 "hgetall 禮山\r\n"
;; 	 "\r\n")))

;; (delete-process redis)

(defun redis-dict (beg end)
(interactive "r")
(with-current-buffer redis-buffer (erase-buffer))
(process-send-string
 redis
 (format(concat
	 "hgetall "
	 (buffer-substring-no-properties beg end)
	 "\r\n"
	 "\r\n"))))

(defun redis-read-result-buffer (command-name command &optional options)
  "Will parse the network response and return the result as appropriate
and then empty the network buffer.
"
  (let ((result-count 0)
	(res-string
	 (with-current-buffer redis-buffer (buffer-substring-no-properties (point-min) (point-max)))))
    (split-string res-string "$")
    ))


(defun redis-execute-command (args &optional options)
;; need to pass in the connection to the db??
;;or better use locally scoped vars in a let binding when calling this
;; at the moment, there are no options

  (if (not (memq (process-status redis) '(connect listen open run)))
      (setq redis (open-network-stream "*redis*" redis-buffer redis-host redis-port)))
  (with-current-buffer redis-buffer (erase-buffer))
  (process-send-string redis (redis-format-command args)))


(defun redis-format-command (command &optional options)
"command is the string used to execute the redis command, e.g.
SET mykey myvalue
"
(let ((cmd (split-string command "#")))
  (setq value (concat "*" (number-to-string (length cmd)) "\r\n"))
  (dolist (s cmd)
    (setq value (concat value "$" (number-to-string (string-bytes s)) "\r\n" s "\r\n")))
  value))

(defun redis-parse-response (command-name &optional redis-buffer)
;; not using this at the moment. CW [2011-01-08T10:51:06+0900]
  (let ((response (with-current-buffer redis-buffer (buffer-substring-no-properties (point-min) (point-max))))
	(flag (with-current-buffer redis-buffer (buffer-substring-no-properties (point-min) 2))))
    (if (equal "-1" (substring response 1 3))
	(substring response 1)
      (if (equal "ERR" (substring response 1 4))
	  (throw redis-error (substring response 5))
	(if (member command-name redis-single-line-commands)
	    (redis-parse-single-line-commands command-name)

)))))

(defun redis-response ()
(with-current-buffer redis-buffer (buffer-substring-no-properties (point-min) (point-max))))

(defun redis-parse-response-int ()
"parse a returned int response"
(let ((response  (redis-response)))
(string-to-int (substring response 1 -2))
))

(defun redis-parse-response-str ()
"parse a returned value, maybe string or int or nil"
(let* ((response  (redis-response))
       (flag (substring response 0 1)))
  (cond ((equal ":" flag)
	 (string-to-int (substring response 1 -2)))
	((equal "$-1" (substring response 0 3))
	 nil)
	((equal "$" flag)
	 (car (cdr (split-string response "\r\n")))))))

(defun redis-parse-response-list ()
"parse a returned list response"
;; todo: take care of nil elements: $-1
(let ((response  (redis-response))
      (value))
  (dolist (el (cdr (split-string response "\\$")))
    (push (car (cdr (split-string el  "\r\n"))) value ))
  (nreverse value)
))


(defun redis-parse-response-hash ()
"parse a returned hash response"
;; todo: take care of nil elements: $-1  (but I guess this does not happens with hashes??
(let ((response  (redis-response))
  (myhash (make-hash-table :test 'equal))
  (key))
  (dolist (el (cdr (split-string response "\\$")))
    (if key
	(progn
	  (puthash key (car (cdr (split-string el "\r\n"))) myhash)
	  (setq key))
      (setq key (car (cdr (split-string el))))))
  myhash
))

(defun redis-parse-response-alist ()
  "parse a returned hash response"
  ;; todo: take care of nil elements: $-1  (but I guess this does not happens with hashes??
  (let ((response  (redis-response))
	(em '())
	(key))
    (dolist (el (cdr (split-string response "\\$")))
      (if key
	  (progn
	    (aput 'em key (car (cdr (split-string el "\r\n"))))
	    (setq key))
	(setq key (car (cdr (split-string el))))))
    em
    ))


(defun redis-parse-single-line-commands (command-name)
)

(defun redis-parse-info (buffer)
    "Parse the result of Redis's INFO command into a hash"
    (let
	((res (with-current-buffer buffer (buffer-substring-no-properties (point-min) (point-max))))
	 (info (make-hash-table :test 'equal)))
      (dolist (el  (cdr (split-string res)))
	(let ((key (car (split-string el ":")))
	      (value  (car (cdr (split-string el ":")))))
	  (puthash key value info)))
      info)
)

(defun redis-cmd-info (&optional buffer)
  (if (not buffer)
      (setq buffer redis-buffer))
  (redis-execute-command "INFO")
  (redis-parse-info buffer))

(defun redis-cmd-lastsave ()
  (redis-execute-command "LASTSAVE")
  (sit-for redis-delay)
  (let ((res (with-current-buffer redis-buffer (buffer-substring-no-properties (point-min) (point-max)))))
    (format-time-string "%F %T" (seconds-to-time (string-to-int (substring res 1 -2))))))

;; hash commands

(defun redis-cmd-hlen (key)
  (redis-execute-command (format "HLEN#%s" key))
  (sit-for redis-delay)
  (redis-parse-response-int))

(defun redis-cmd-hdel (key field)
  (redis-execute-command (format "HDEL#%s#%s" key field))
  (sit-for redis-delay)
  (redis-parse-response-int))

(defun redis-cmd-hexists (key field)
  (redis-execute-command (format "HEXISTS#%s#%s" key field))
  (sit-for redis-delay)
  (redis-parse-response-int))

(defun redis-cmd-hget (key field)
"Get the value of a hash field"
  (redis-execute-command (format "HGET#%s#%s" key field))
  (sit-for redis-delay)
  (redis-parse-response-str))

(defun redis-cmd-hgetall (key)
"Get all the fields and values in a hash"
  (redis-execute-command (format "HGETALL#%s" key))
  (accept-process-output redis)
;  (sit-for redis-delay)
  (redis-parse-response-alist))

(defun redis-cmd-hincrby (key field increment)
"Increment the integer value of a hash field by the given number"
  (redis-execute-command (format "HINCRBY#%s#%s#%s" key field increment))
  (sit-for redis-delay)
  (redis-parse-response-int))

(defun redis-cmd-hkeys (key)
"Get all the fields in a hash"
  (redis-execute-command (format "HKEYS#%s" key))
  (sit-for redis-delay)
  (redis-parse-response-list))

(defun redis-cmd-hlen (key)
"Get the number of fields in a hash"
  (redis-execute-command (format "HLEN#%s" key))
  (sit-for redis-delay)
  (redis-parse-response-int))

(defun redis-cmd-hmget (key field &optional fields)
"Get the values of all the given hash fields"
"-ERR client command not implemented"
)

(defun redis-cmd-hmset (key field value &optional fields values)
"Set multiple hash fields to multiple values"
"-ERR client command not implemented"
)

(defun redis-cmd-hset (key field value)
"Set the string value of a hash field"
  (redis-execute-command (format "HSET#%s#%s#%s" key field value))
  (sit-for redis-delay)
  (redis-parse-response-int))

(defun redis-cmd-hsetnx (key field value)
"Set the value of a hash field, only if the field does not exist"
  (redis-execute-command (format "HSETNX#%s#%s#%s" key field value))
  (sit-for redis-delay)
  (redis-parse-response-int))

(defun redis-cmd-hvals (key)
  "Get all the values in a hash"
  (redis-execute-command (format "HVALS#%s" key))
  (sit-for redis-delay)
  (redis-parse-response-int))

;; (defun redis-cmd- (key field)
;; (defun redis-cmd- (key field)
;; (defun redis-cmd- (key field)


;; auto-generated stuff


;;----connection-----------
;;------------------------


(defun redis-cmd-auth (password)
"Authenticate to the server"
(redis-execute-command (format "AUTH#%s" password))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-echo (message)
"Echo the given string"
(redis-execute-command (format "ECHO#%s" message))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-ping ()
"Ping the server"
(redis-execute-command (format "PING" ))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-quit ()
"Close the connection"
(redis-execute-command (format "QUIT" ))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-select (index)
"Change the selected database for the current connection, index is the number of the database"
(redis-execute-command (format "SELECT#%s" index))
(sit-for redis-delay)
(redis-parse-response-int))

;;----generic-----------
;;------------------------


(defun redis-cmd-del (key [key ...])
"Delete a key"
(redis-execute-command (format "DEL#%s#%s#%s" key [key ...]))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-exists (key)
"Determine if a key exists"
(redis-execute-command (format "EXISTS#%s" key))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-expire (key seconds)
"Set a key's time to live in seconds"
(redis-execute-command (format "EXPIRE#%s#%s" key seconds))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-expireat (key timestamp)
"Set the expiration for a key as a UNIX timestamp"
(redis-execute-command (format "EXPIREAT#%s#%s" key timestamp))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-keys (pattern)
"Find all keys matching the given pattern"
(redis-execute-command (format "KEYS#%s" pattern))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-move (key db)
"Move a key to another database"
(redis-execute-command (format "MOVE#%s#%s" key db))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-persist (key)
"Remove the expiration from a key"
(redis-execute-command (format "PERSIST#%s" key))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-randomkey ()
"Return a random key from the keyspace"
(redis-execute-command (format "RANDOMKEY" ))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-rename (key newkey)
"Rename a key"
(redis-execute-command (format "RENAME#%s#%s" key newkey))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-renamenx (key newkey)
"Rename a key, only if the new key does not exist"
(redis-execute-command (format "RENAMENX#%s#%s" key newkey))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-sort (cmd)
  "Sort the elements in a list, set or sorted set"
  (redis-execute-command (cmd))
  (sit-for redis-delay)
  (redis-parse-response-alist))

(defun redis-cmd-ttl (key)
"Get the time to live for a key"
(redis-execute-command (format "TTL#%s" key))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-type (key)
"Determine the type stored at key"
(redis-execute-command (format "TYPE#%s" key))
(sit-for redis-delay)
(redis-parse-response-int))


;;----list-----------
;;------------------------


(defun redis-cmd-blpop (key [key ...] timeout)
"Remove and get the first element in a list, or block until one is available"
(redis-execute-command (format "BLPOP#%s#%s#%s#%s" key [key ...] timeout))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-brpop (key [key ...] timeout)
"Remove and get the last element in a list, or block until one is available"
(redis-execute-command (format "BRPOP#%s#%s#%s#%s" key [key ...] timeout))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-brpoplpush (source destination timeout)
"Pop a value from a list, push it to another list and return it; or block until one is available"
(redis-execute-command (format "BRPOPLPUSH#%s#%s#%s" source destination timeout))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-lindex (key index)
"Get an element from a list by its index"
(redis-execute-command (format "LINDEX#%s#%s" key index))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-linsert (key BEFORE|AFTER pivot value)
"Insert an element before or after another element in a list"
(redis-execute-command (format "LINSERT#%s#%s#%s#%s" key BEFORE|AFTER pivot value))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-llen (key)
"Get the length of a list"
(redis-execute-command (format "LLEN#%s" key))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-lpop (key)
"Remove and get the first element in a list"
(redis-execute-command (format "LPOP#%s" key))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-lpush (key value)
"Prepend a value to a list"
(redis-execute-command (format "LPUSH#%s#%s" key value))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-lpushx (key value)
"Prepend a value to a list, only if the list exists"
(redis-execute-command (format "LPUSHX#%s#%s" key value))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-lrange (key start stop)
"Get a range of elements from a list"
(redis-execute-command (format "LRANGE#%s#%s#%s" key start stop))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-lrem (key count value)
"Remove elements from a list"
(redis-execute-command (format "LREM#%s#%s#%s" key count value))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-lset (key index value)
"Set the value of an element in a list by its index"
(redis-execute-command (format "LSET#%s#%s#%s" key index value))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-ltrim (key start stop)
"Trim a list to the specified range"
(redis-execute-command (format "LTRIM#%s#%s#%s" key start stop))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-rpop (key)
"Remove and get the last element in a list"
(redis-execute-command (format "RPOP#%s" key))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-rpoplpush (source destination)
"Remove the last element in a list, append it to another list and return it"
(redis-execute-command (format "RPOPLPUSH#%s#%s" source destination))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-rpush (key value)
"Append a value to a list"
(redis-execute-command (format "RPUSH#%s#%s" key value))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-rpushx (key value)
"Append a value to a list, only if the list exists"
(redis-execute-command (format "RPUSHX#%s#%s" key value))
(sit-for redis-delay)
(redis-parse-response-int))

;;----pubsub-----------
;;------------------------


(defun redis-cmd-psubscribe (pattern)
"Listen for messages published to channels matching the given patterns"
(redis-execute-command (format "PSUBSCRIBE#%s" pattern))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-publish (channel message)
"Post a message to a channel"
(redis-execute-command (format "PUBLISH#%s#%s" channel message))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-punsubscribe ([pattern [pattern ...]])
"Stop listening for messages posted to channels matching the given patterns"
(redis-execute-command (format "PUNSUBSCRIBE#%s#%s#%s" [pattern [pattern ...]]))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-subscribe (channel)
"Listen for messages published to the given channels"
(redis-execute-command (format "SUBSCRIBE#%s" channel))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-unsubscribe ([channel [channel ...]])
"Stop listening for messages posted to the given channels"
(redis-execute-command (format "UNSUBSCRIBE#%s#%s#%s" [channel [channel ...]]))
(sit-for redis-delay)
(redis-parse-response-int))

;;----server-----------
;;------------------------


(defun redis-cmd-bgrewriteaof ()
"Asynchronously rewrite the append-only file"
(redis-execute-command (format "BGREWRITEAOF" ))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-bgsave ()
"Asynchronously save the dataset to disk"
(redis-execute-command (format "BGSAVE" ))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-config get (parameter)
"Get the value of a configuration parameter"
(redis-execute-command (format "CONFIG GET#%s" parameter))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-config set (parameter value)
"Set a configuration parameter to the given value"
(redis-execute-command (format "CONFIG SET#%s#%s" parameter value))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-config resetstat ()
"Reset the stats returned by INFO"
(redis-execute-command (format "CONFIG RESETSTAT" ))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-dbsize ()
"Return the number of keys in the selected database"
(redis-execute-command (format "DBSIZE" ))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-debug object (key)
"Get debugging information about a key"
(redis-execute-command (format "DEBUG OBJECT#%s" key))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-debug segfault ()
"Make the server crash"
(redis-execute-command (format "DEBUG SEGFAULT" ))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-flushall ()
"Remove all keys from all databases"
(redis-execute-command (format "FLUSHALL" ))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-flushdb ()
"Remove all keys from the current database"
(redis-execute-command (format "FLUSHDB" ))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-info ()
"Get information and statistics about the server"
(redis-execute-command (format "INFO" ))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-lastsave ()
"Get the UNIX time stamp of the last successful save to disk"
(redis-execute-command (format "LASTSAVE" ))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-monitor ()
"Listen for all requests received by the server in real time"
(redis-execute-command (format "MONITOR" ))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-save ()
"Synchronously save the dataset to disk"
(redis-execute-command (format "SAVE" ))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-shutdown ()
"Synchronously save the dataset to disk and then shut down the server"
(redis-execute-command (format "SHUTDOWN" ))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-slaveof (host port)
"Make the server a slave of another instance, or promote it as master"
(redis-execute-command (format "SLAVEOF#%s#%s" host port))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-sync ()
"Internal command used for replication"
(redis-execute-command (format "SYNC" ))
(sit-for redis-delay)
(redis-parse-response-int))

;;----set-----------
;;------------------------


(defun redis-cmd-sadd (key member)
"Add a member to a set"
(redis-execute-command (format "SADD#%s#%s" key member))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-scard (key)
"Get the number of members in a set"
(redis-execute-command (format "SCARD#%s" key))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-sdiff (key [key ...])
"Subtract multiple sets"
(redis-execute-command (format "SDIFF#%s#%s#%s" key [key ...]))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-sdiffstore (destination key [key ...])
"Subtract multiple sets and store the resulting set in a key"
(redis-execute-command (format "SDIFFSTORE#%s#%s#%s#%s" destination key [key ...]))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-sinter (key [key ...])
"Intersect multiple sets"
(redis-execute-command (format "SINTER#%s#%s#%s" key [key ...]))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-sinterstore (destination key [key ...])
"Intersect multiple sets and store the resulting set in a key"
(redis-execute-command (format "SINTERSTORE#%s#%s#%s#%s" destination key [key ...]))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-sismember (key member)
"Determine if a given value is a member of a set"
(redis-execute-command (format "SISMEMBER#%s#%s" key member))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-smembers (key)
"Get all the members in a set"
(redis-execute-command (format "SMEMBERS#%s" key))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-smove (source destination member)
"Move a member from one set to another"
(redis-execute-command (format "SMOVE#%s#%s#%s" source destination member))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-spop (key)
"Remove and return a random member from a set"
(redis-execute-command (format "SPOP#%s" key))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-srandmember (key)
"Get a random member from a set"
(redis-execute-command (format "SRANDMEMBER#%s" key))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-srem (key member)
"Remove a member from a set"
(redis-execute-command (format "SREM#%s#%s" key member))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-sunion (key [key ...])
"Add multiple sets"
(redis-execute-command (format "SUNION#%s#%s#%s" key [key ...]))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-sunionstore (destination key [key ...])
"Add multiple sets and store the resulting set in a key"
(redis-execute-command (format "SUNIONSTORE#%s#%s#%s#%s" destination key [key ...]))
(sit-for redis-delay)
(redis-parse-response-int))

;;----sorted_set-----------
;;------------------------


(defun redis-cmd-zadd (key score member)
"Add a member to a sorted set, or update its score if it already exists"
(redis-execute-command (format "ZADD#%s#%s#%s" key score member))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-zcard (key)
"Get the number of members in a sorted set"
(redis-execute-command (format "ZCARD#%s" key))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-zcount (key min max)
"Count the members in a sorted set with scores within the given values"
(redis-execute-command (format "ZCOUNT#%s#%s#%s" key min max))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-zincrby (key increment member)
"Increment the score of a member in a sorted set"
(redis-execute-command (format "ZINCRBY#%s#%s#%s" key increment member))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-zinterstore (destination numkeys key [key ...] [WEIGHTS weight [weight ...]] [AGGREGATE SUM|MIN|MAX])
"Intersect multiple sorted sets and store the resulting sorted set in a new key"
(redis-execute-command (format "ZINTERSTORE#%s#%s#%s#%s#%s#%s#%s#%s#%s#%s#%s" destination numkeys key [key ...] [WEIGHTS weight [weight ...]] [AGGREGATE SUM|MIN|MAX]))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-zrange (key start stop [WITHSCORES])
"Return a range of members in a sorted set, by index"
(redis-execute-command (format "ZRANGE#%s#%s#%s#%s" key start stop [WITHSCORES]))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-zrangebyscore (key min max [WITHSCORES] [LIMIT offset count])
"Return a range of members in a sorted set, by score"
(redis-execute-command (format "ZRANGEBYSCORE#%s#%s#%s#%s#%s#%s#%s" key min max [WITHSCORES] [LIMIT offset count]))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-zrank (key member)
"Determine the index of a member in a sorted set"
(redis-execute-command (format "ZRANK#%s#%s" key member))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-zrem (key member)
"Remove a member from a sorted set"
(redis-execute-command (format "ZREM#%s#%s" key member))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-zremrangebyrank (key start stop)
"Remove all members in a sorted set within the given indexes"
(redis-execute-command (format "ZREMRANGEBYRANK#%s#%s#%s" key start stop))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-zremrangebyscore (key min max)
"Remove all members in a sorted set within the given scores"
(redis-execute-command (format "ZREMRANGEBYSCORE#%s#%s#%s" key min max))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-zrevrange (key start stop [WITHSCORES])
"Return a range of members in a sorted set, by index, with scores ordered from high to low"
(redis-execute-command (format "ZREVRANGE#%s#%s#%s#%s" key start stop [WITHSCORES]))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-zrevrangebyscore (key max min [WITHSCORES] [LIMIT offset count])
"Return a range of members in a sorted set, by score, with scores ordered from high to low"
(redis-execute-command (format "ZREVRANGEBYSCORE#%s#%s#%s#%s#%s#%s#%s" key max min [WITHSCORES] [LIMIT offset count]))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-zrevrank (key member)
"Determine the index of a member in a sorted set, with scores ordered from high to low"
(redis-execute-command (format "ZREVRANK#%s#%s" key member))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-zscore (key member)
"Get the score associated with the given member in a sorted set"
(redis-execute-command (format "ZSCORE#%s#%s" key member))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-zunionstore (destination numkeys key [key ...] [WEIGHTS weight [weight ...]] [AGGREGATE SUM|MIN|MAX])
"Add multiple sorted sets and store the resulting sorted set in a new key"
(redis-execute-command (format "ZUNIONSTORE#%s#%s#%s#%s#%s#%s#%s#%s#%s#%s#%s" destination numkeys key [key ...] [WEIGHTS weight [weight ...]] [AGGREGATE SUM|MIN|MAX]))
(sit-for redis-delay)
(redis-parse-response-int))

;;----string-----------
;;------------------------


(defun redis-cmd-append (key value)
"Append a value to a key"
(redis-execute-command (format "APPEND#%s#%s" key value))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-decr (key)
"Decrement the integer value of a key by one"
(redis-execute-command (format "DECR#%s" key))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-decrby (key decrement)
"Decrement the integer value of a key by the given number"
(redis-execute-command (format "DECRBY#%s#%s" key decrement))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-get (key)
"Get the value of a key"
(redis-execute-command (format "GET#%s" key))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-getbit (key offset)
"Returns the bit value at offset in the string value stored at key"
(redis-execute-command (format "GETBIT#%s#%s" key offset))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-getrange (key start end)
"Get a substring of the string stored at a key"
(redis-execute-command (format "GETRANGE#%s#%s#%s" key start end))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-getset (key value)
"Set the string value of a key and return its old value"
(redis-execute-command (format "GETSET#%s#%s" key value))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-incr (key)
"Increment the integer value of a key by one"
(redis-execute-command (format "INCR#%s" key))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-incrby (key increment)
"Increment the integer value of a key by the given number"
(redis-execute-command (format "INCRBY#%s#%s" key increment))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-mget (key [key ...])
"Get the values of all the given keys"
(redis-execute-command (format "MGET#%s#%s#%s" key [key ...]))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-mset (key value [key value ...])
"Set multiple keys to multiple values"
(redis-execute-command (format "MSET#%s#%s#%s#%s#%s" key value [key value ...]))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-msetnx (key value [key value ...])
"Set multiple keys to multiple values, only if none of the keys exist"
(redis-execute-command (format "MSETNX#%s#%s#%s#%s#%s" key value [key value ...]))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-set (key value)
"Set the string value of a key"
(redis-execute-command (format "SET#%s#%s" key value))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-setbit (key offset value)
"Sets or clears the bit at offset in the string value stored at key"
(redis-execute-command (format "SETBIT#%s#%s#%s" key offset value))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-setex (key seconds value)
"Set the value and expiration of a key"
(redis-execute-command (format "SETEX#%s#%s#%s" key seconds value))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-setnx (key value)
"Set the value of a key, only if the key does not exist"
(redis-execute-command (format "SETNX#%s#%s" key value))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-setrange (key offset value)
"Overwrite part of a string at key starting at the specified offset"
(redis-execute-command (format "SETRANGE#%s#%s#%s" key offset value))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-strlen (key)
"Get the length of the value stored in a key"
(redis-execute-command (format "STRLEN#%s" key))
(sit-for redis-delay)
(redis-parse-response-int))

;;----transactions-----------
;;------------------------


(defun redis-cmd-discard ()
"Discard all commands issued after MULTI"
(redis-execute-command (format "DISCARD" ))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-exec ()
"Execute all commands issued after MULTI"
(redis-execute-command (format "EXEC" ))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-multi ()
"Mark the start of a transaction block"
(redis-execute-command (format "MULTI" ))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-unwatch ()
"Forget about all watched keys"
(redis-execute-command (format "UNWATCH" ))
(sit-for redis-delay)
(redis-parse-response-int))

(defun redis-cmd-watch (key [key ...])
"Watch the given keys to determine execution of the MULTI/EXEC block"
(redis-execute-command (format "WATCH#%s#%s#%s" key [key ...]))
(sit-for redis-delay)
(redis-parse-response-int))

(provide 'redis)


;;; redis.el ends here
