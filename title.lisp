;;    Copyright (C) 2011  Christopher Hanna

;;    This program is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.

;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.

;;    You should have received a copy of the GNU General Public License
;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Title screen with license information

(load "time.lisp")
(load "economy.lisp")
(defparameter *main-menu*
	`(("Play a world" . ,(lambda () (format t "play")))
		("Generate new world" . ,(lambda () (format t "generate")))
		("Game testing features" . ,(lambda () (format t "test")))
		("Exit" . ,(lambda () (format t "Quitting...")))))

(defun show-menu (title entries)
	(format t "~a~%" title)
	(loop for entry in entries
		 and number from 1
		 do (format t "~a. ~a~%" number (car entry)))
	(format t "Enter your choice: ")
	(let ((choice (or (parse-integer (read-line) :junk-allowed t) 0)))
		(if (< 0 choice (1+ (length entries)))
				(funcall (cdr (elt entries (1- choice))))
				(progn (format t "~%~%That's not an option.~%")
							 (show-menu title entries)))))

(defun license ()
	(format t 
					"Guild Master Pre-Alpha Copyright (C) 2011 Christopher Hanna~%~%"))

(license)
(show-menu "GUILD MASTER" *main-menu*)
