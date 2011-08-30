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
	(let ((choise (or (parse-integer (read-line) :junk-allowed t) 0)))
		(if (< 0 choise (1+ (length entries)))
				(funcall (cdr (elt entries (1- choise))))
				(progn (format t "~%~%That's not an option.~%")
							 (show-menu title entries)))))

(show-menu "GUILD MASTER" *main-menu*)
