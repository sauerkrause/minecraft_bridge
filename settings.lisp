;; Copyright 2013 Robert Allen Krause <robert.allen.krause@gmail.com

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
(load "common-defs.lisp")

(defparameter *botnick* "mcbort")
;; Need to know at least nick and serv
(defparameter *login*
  (make-login-info
   :nick *botnick*
   :server "irc.drwilco.net"))

;; set of channels
(defparameter *channels* ())
(pushnew "#minecraft" *channels* :test #'equal)

;; command prefixen
(defparameter *prefixen* `("?" 
			   ,(format nil "~a: " *botnick*)))
