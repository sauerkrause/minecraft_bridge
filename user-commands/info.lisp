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

(require :usocket)
(require :flexi-streams)

(in-package :user-commands)

(load "user-commands/common.lisp")

;; Contains *mc-server* and *mc-port*
(load "configs/mc.lisp")

;; terrible recursion I know...,
;; but split-sequence in cl-utilities apparently can't do this ಠ_ಠ
(defun split-by-string (str delimiter)
  (let ((idx (search delimiter str)))
    (if idx
	(cons (subseq str 0 idx) (split-by-string (subseq str (+ 2 idx)) delimiter))
	(cons str ()))))

(defun handle-mc-server-output (server output)
  (let ((output-list (split-by-string output "||")))
    (format nil "~a: Players (~a/~a) Version: ~a MOTD: ~a"
	    server
	    (remove #\| (elt output-list 4))
	    (remove #\| (elt output-list 5))
	    (remove #\| (elt output-list 2))
	    (remove #\| (elt output-list 3)))))

(defun get-mc-info (server port)
  (with-open-stream (s
		     (flexi-streams:make-flexi-stream
		      (usocket:socket-stream 
		       (usocket:socket-connect server port
					       :protocol :stream
					       :element-type '(unsigned-byte 8)))))
    (setf (flexi-streams:flexi-stream-element-type s) '(unsigned-byte 8))
    (write-byte #xfe s)
    (write-byte #x1 s)
    (force-output s)
    
    (when (eq (read-byte s) 255)
	
      (setf (flexi-streams:flexi-stream-element-type s) 'character)
      (let ((output (make-string-output-stream)))
	(loop
	   for char = (read-char s nil nil)
	   while char do (format output "~c" (if (char-equal char #\Nul)
						 ;; This isn't used except MAYBE in a motd, but not in mine.
						 #\|
						 char)))
	(handle-mc-server-output server (get-output-stream-string output))))))

(defun info (msg connection)
  (let ((message (get-mc-info *mc-server* *mc-port*)))
    (irc:privmsg connection (get-destination msg) message)))
(export 'info)