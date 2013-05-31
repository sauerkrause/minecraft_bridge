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
(require :bordeaux-threads)

(load "common-defs.lisp")
(load "user-commands.lisp")
(load "irc-mc-bridge.lisp")
(load "mc-irc-bridge.lisp")
(in-package :robort)

(defun init-hooks (connection)
  (irc:remove-hooks connection 'irc::irc-privmsg-message)
  (irc:remove-hooks connection 'irc::ctcp-action-message)
  (irc:add-hook connection 'irc::irc-privmsg-message
		(lambda (msg)
		  (user-command-helpers::handle-command msg connection)))
  (irc:add-hook connection 'irc::irc-privmsg-message
		(lambda (msg)
		  (mcirc::handle-message msg connection)))
  (irc:add-hook connection 'irc::ctcp-action-message
		(lambda (msg)
		  (mcirc::handle-action msg connection))))

;; Do anything that needs to be done prior to reading the loops here.
(defun init (connection)
  ;; Maybe connect to the channels we want here.
  (dolist (s *channels*)
    (irc:join connection s))
  ;; Maybe initialize some hooks.
  (init-hooks connection)
  (when mcirc::*thread*
    (bordeaux-threads:destroy-thread mcirc::*thread*))
  (setf mcirc::*thread* (mcirc::start-bridge connection)))

;; handy reinit command.
(defun reinit (connection)
  (init-hooks connection))
