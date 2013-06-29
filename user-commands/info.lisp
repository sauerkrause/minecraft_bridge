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
  (if (eql (elt output 3) #\§)
      (handle-new-mc-server-output server output)
    (handle-old-mc-server-output server output)))

(defun handle-new-mc-server-output (server output)
  (princ "Handling new output")
  (let ((output-list (split-by-string output (make-string 2 :initial-element #\Nul))))
    (format nil "~a: Players (~a/~a) Version: ~a MOTD: ~a"
	    server
	    (remove #\Nul (elt output-list 4))
	    (remove #\Nul (elt output-list 5))
	    (remove #\Nul (elt output-list 2))
	    (remove #\Nul (elt output-list 3)))))

(defun handle-old-mc-server-output (server output)
  (princ "Handling old output")
  (let ((output-list (split-by-string output "§")))
    (print output-list)
    (format nil "~a: Players (~a/~a) MOTD: ~a"
	    server
	    (remove #\Nul (elt output-list 1))
	    (remove #\Nul (elt output-list 2))
	    (remove #\Nul (elt output-list 0)))))

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
	   while char do (format output "~c" char))
	(handle-mc-server-output server (get-output-stream-string output))))))

(defun info (msg connection)
  (let ((message (get-mc-info *mc-server* *mc-port*)))
    (irc:privmsg connection (get-destination msg) message)))
(export 'info)
