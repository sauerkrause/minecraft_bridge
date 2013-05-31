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
(require :cl-irc)
(require :trivial-shell)

;; need a package for these helpers separate from the user-commands
(defpackage :mcirc
  (:use :common-lisp))

(in-package :mcirc)

(load "configs/rcon.lisp")

(defun handle-message(msg connection)
  (say-to-rcons msg "{~a} ~a"))

(defun handle-action (msg connection)
  (let ((action (subseq (cadr (irc:arguments msg)) (length "ACTION  "))))
    (princ action)
    (setf (cadr (irc:arguments msg)) action)
    (say-to-rcons msg "* ~a ~a")))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
			 (loop with part-length = (length part)
			       for old-pos = 0 then (+ pos part-length)
			       for pos = (search part string
						 :start2 old-pos
						 :test test)
			       do (write-string string out
						:start old-pos
						:end (or pos (length string)))
			       when pos do (write-string replacement out)
			       while pos)))

(defun say-to-rcons (msg rcons-msg)
  (let ((message (cadr (irc:arguments msg))))
    (trivial-shell:shell-command 
     (format nil "mcrcon -s -H ~a -P ~a -p ~a \"say ~a\""
	     *rcon-host* 
	     *rcon-port* 
	     *rcon-passwd*
	     (replace-all 
	      (format nil rcons-msg
		      (irc:source msg)
		      message)
	      "\"" "\\\"")))))
