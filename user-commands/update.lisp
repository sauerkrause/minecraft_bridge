;; Copyright 2013 Robert Allen Krause <robert.allen.krause@gmail.com>

;;     This file is part of Robort.

;;     Robort is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 3 of the License, or
;;     (at your option) any later version.

;;     Robort is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.

;;     You should have received a copy of the GNU General Public License
;;     along with Robort.  If not, see <http://www.gnu.org/licenses/>.

(require :trivial-shell)

(in-package :user-commands)

(defun message-string (msg)
  (cadr (irc::arguments msg)))

(defun update (msg connection)
  (when (< (length (rest-words (message-string msg))) 1)
    (error 'user-command-helpers::flooped-command))
  (let* ((msg-list (rest-words (message-string msg)))
	 (remote (car msg-list))
	 (branch (if (< (length msg-list) 2)
		    "master"
		    (cadr msg-list))))
    ;; error out before attempting anything when we don't have the args
    (progn
      (multiple-value-bind (response output error-output status)
	  (trivial-shell:shell-command (format 
					nil 
					"git pull ~a ~a"
					remote
					branch))
	(print output)
	(irc:notice connection (irc:source msg) 
		    output)
	(let ((output-list
	       (loop for i = 0 then (1+ j)
		  as j = (position #\linefeed response :start i)
		  collect (subseq response i j)
		  while j)))
	  (dolist (line output-list)
	    (progn
	      (irc:notice connection
			  (irc:source msg)
			  line)
	      (sleep 0.1))))))))
	
(register-auth #'update)

(export 'update)
