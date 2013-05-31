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

(defpackage :robort
  (:use :common-lisp :common-lisp-user))
(in-package :robort)

(require :cl-irc)

(load "settings.lisp")
(load "common-defs.lisp")

;; need this as *logins* should be closed over for this.
(defun get-connection (login)
  (progn
    (print (login-info-nick login))
    (print (login-info-server login))
    (irc:connect
     :server (login-info-server login)
     :nickname (login-info-nick login))))

(defun fast-reload ()
    (load "settings.lisp")
    (load "common-defs.lisp")
    (load "init.lisp")
    (reinit *connection*))

(defun fix-screwup ()
  (reinitialize *connection*)
  (main))

(defun reinitialize (connection)
  (progn
    ;; Use quit, not die or disconnect.
    (irc:quit connection)
    (print "Died connection hopefully")))
(export 'reinitialize)

(defparameter *connection* nil)
;; Entry point
(defun main ()
  (progn
    (load "settings.lisp")
    (load "common-defs.lisp")
    (load "init.lisp")
    (defparameter *connection* (get-connection *login*))
    (handler-case
     (progn
       (init *connection*)
       (irc:read-message-loop *connection*))
     (reinitialize-required () (reinitialize *connection*)))))

(load "init.lisp")


(loop
 (main))
