;; A lunisolar calendar for the world based on the Metonic Cycle

;; Constants
(defconstant +cycle+ 19) ;;19 year cycle with 6940 days
(defconstant +lunar-year+ 354) ;; Days in a lunar year
(defconstant +long-year+ 384) ;; Days in year with the extra month
(defconstant +longer-year+ 388) ;; Days with extra long month
(defconstant +month-short+ 29) ;; First month in year is this many days
(defconstant +month-long+ 30) ;; Following month is this and these alternate
(defconstant +month-extra+ 30) ;; 13th month on year 3, 6, 8, 11, 14 ,19
(defconstant +month-extra-long+ 34) ;; 13th month on year 17
(defconstant +week-regular+ 7) ;; Regular week
(defconstant +week-short+ 8) ;; Last week of short month
(defconstant +week-long+ 9) ;; Last week of long month and the extra long month
(defconstant +week-extra-long+ 6) ;; Last week in month 13 of year 8

;; Global variables
(defvar *year* 0) ;; Holds years that have passed
(defvar *year-in-cycle* 0) ;; Holds the year in the cycle
(defvar *month* 0) ;; Holds the current month type
(defvar *month-of-year* 0) ;; Holds the month in year so far
(defvar *week* 0)  ;; Holds the current week type
(defvar *week-in-month* 0) ;; Holds current week in the month so far
(defvar *day* 0) ;; Holds days in year so far
(defvar *days-in-year* 0) ;; Holds the total days in the year
(defvar *day-of-month* 0) ;; Holds days in month so far
(defvar *day-of-week* 0) ;; Holds the current day of the week

;; Counter functions
(defun increase-day-counter () ;; Changes the day
	(incf *day*))       ;; the most basic time unit in the game
(defun reset-day-counter () ;; Set back to one on a new year
	(setf *day* 1))
(defun increase-day-of-week-counter () ;; Keeps track of day of the week --
	(incf *day-of-week*))                 ;; needs same time as day-counter
(defun reset-day-of-week-counter ()
	(setf *day-of-week* 1))
(defun increase-day-of-month-counter () ;; Keeps track of day of the month --
	(incf *day-of-month*))                 ;; needs same time as day-counter
(defun reset-day-of-month-counter ()
	(setf *day-of-month* 1))
(defun increase-week-in-month-counter () ;; Used to track week in month to set
	(incf *week-in-month*))                ;; weeks not of seven days
(defun reset-week-in-month-counter ()
	(setf *week-in-month* 1))
(defun increase-month-counter () ;; Used to increase the month number
	(incf *month-of-year*))
(defun reset-month-counter () ;; Set back to one on a new year
	(setf *month-of-year* 1))
(defun increase-year () ;; Increases every year; does not need to be reset
	(incf *year*))
(defun increase-year-in-cycle () ;; Keep track of year in the 19 year cycle to 
	(incf *year-in-cycle*))        ;; know when to add a 13th month 
(defun reset-year-in-cycle () ;; Set back to one at the end of the cycle
	(setf *year-in-cycle* 1))

;; Definition functions

;; Year functions
(defun regular-year () ;; Sets the days in the year to 354
	(setf *days-in-year* +lunar-year+))
(defun long-year () ;; Sets the days in the year to 384
	(setf *days-in-year* +long-year+))
(defun longer-year () ;; Sets the days in the year to 388
	(setf *days-in-year* +longer-year+))

;; Month functions
(defun start-short-month () ;; Sets *month* to a short month
	(setf *month* +month-short+))
(defun start-long-month () ;; Sets *month* to a long month
	(setf *month* +month-long+))
(defun start-extra-month () ;; Sets *month* to the extra month
	(setf *month* +month-extra+))
(defun start-extra-long-month () ;; Sets *month* to the extra long month
	(setf *month* +month-extra-long+))

;; Week functions
(defun start-regular-week ()
	(setf *week* +week-regular+))
(defun start-short-week () ;; Sort of a misnomer, short corresponds to the month
	(setf *week* +week-short+)) ;; ,since this week is actually longer than normal
(defun start-long-week ()
	(setf *week* +week-long+))
(defun start-extra-long-week ()
	(setf *week* +week-extra-long+))

;; Checks for loops

;; Year checks
(defun year-check () ;; Checks if it is time to go to the next year -- part of
	(year-check-1)     ;; main game loop
	(year-check-2)
	(year-check-3)
	(year-check-4)
	(year-check-5))	 
(defun year-check-1 () ;; Regular year
		(when (and (eql *days-in-year* +lunar-year+) (eql *day* 355))
			(regular-year)
			(increase-year-in-cycle)
			(increase-year)
			(reset-month-counter)
			(reset-day-counter)))
(defun year-check-2 () ;; Checking if the next year is a long year
	(when (and (eql *days-in-year* +lunar-year+) (eql *day* 355)
						 (or (eql *year-in-cycle* 2) (eql *year-in-cycle* 5)
								 (eql *year-in-cycle* 7) (eql *year-in-cycle* 10)
								 (eql *year-in-cycle* 13) (eql *year-in-cycle* 18)))
		(long-year)
		(increase-year-in-cycle)
		(increase-year)
		(reset-month-counter)
		(reset-day-counter)))
(defun year-check-3 () ;; Checking if the next year is a longer year
		(when (and (eql *days-in-year* +lunar-year+) (eql *day* 355)
							 (eql *year-in-cycle* 16))
			(longer-year)
			(increase-year-in-cycle)
			(increase-year)
			(reset-month-counter)
			(reset-day-counter)))
(defun year-check-4 () ;; Starting a year at the end of a long year
		(when (and (eql *days-in-year* +lunar-year+) (eql *day* 385))
			(regular-year)
			(increase-year-in-cycle)
			(increase-year)
			(reset-month-counter)
			(reset-day-counter)))
(defun year-check-5 () ;; Starting a year at the end of a longer year
		(when (and (eql *days-in-year* +longer-year+) (eql *day* 389))
			(regular-year)
			(increase-year-in-cycle)
			(increase-year)
			(reset-month-counter)
			(reset-day-counter)))

;; Month Checks
(defun month-check () ;; Checks if a new month is here -- it needs a loop and
	(month-check-1)     ;; a time increment to check
	(month-check-2)
	(month-check-3)
	(month-check-4))

(defun month-check-1 () ;; See if a long month needs to be started
	(when (and (eql *month* +month-short+) (eql *day-of-month* 30))
		(start-long-month)
		(reset-day-of-month)
		(reset-week-in-month-counter)
		(start-regular-week)
		(increase-month-counter)))
(defun month-check-2 () ;; See if a short month needs to be started
	(when (and (eql *month* +month-long+) (eql *day-of-month* 31))
		(start-short-month) 
		(reset-day-of-month)
		(reset-week-in-month-counter)
		(start-regular-week)
		(increase-month-counter)))
(defun month-check-3 () ;; See if a 13th month is necessary
	(when (and (eql *month-of-year* 12) (eql *day-of-month* 31) 
						 (or (eql *year-in-cycle* 3) (eql *year-in-cycle* 6)
								 (eql *year-in-cycle* 8) (eql *year-in-cycle* 11)
								 (eql *year-in-cycle* 14) (eql *year-in-cycle* 19)))
		(start-extra-month)
		(reset-day-of-month)
		(reset-week-in-month-counter)
		(start-regular-week)
		(increase-month-counter)))
(defun month-check-4 () ;; See if a long 13th month is necessary
		(when (and (eql *month-of-year* 12) (eql *day-of-month* 31)
							 (eql *year-in-cycle* 17))
		(start-extra-month)
		(reset-day-of-month)
		(reset-week-in-month-counter)
		(start-regular-week)
		(increase-month-counter)))

;; Week checks
(defun week-check ()
	(week-check-1)
	(week-check-2)
	(week-check-3))

(defun week-check-1 ()
	(when (and (eql *week* +week-regular+) (eql *day-of-week* 8))
		(start-regular-week)
		(increase-week-in-month-counter)
		(reset-day-of-week-counter)))
(defun week-check-2 ()
	(when (and (eql *week-in-month* 3) (eql *day-of-week* 8) 
						 (eql *month* +month-short+))
		(start-short-week)
		(increase-week-in-month)
		(reset-day-of-week-counter)))
(defun week-check-3 ()
	(when (and (eql *week-in-month* 3) (eql *day-of-week* 8) 
						 (eql *month* +month-long+))
		(start-long-week)
		(increase-week-in-month)
		(reset-day-of-week-counter))))

;; The ticker on which the days change
(defvar *tick-p* t)
(let ((prev-time 0) (cur-time 0) (difference 0))
(defun set-prev-time () (setf prev-time (get-universal-time)))
(defun run-time-functions () 
	(increase-day-counter)
	(increase-day-of-week-counter)
	(increase-day-of-month-counter)
	(year-check)
	(month-check)
	(week-check)
	(set-prev-time)
	(setf *tick-p* nil))
(defun ticker ()
	(setf cur-time (get-universal-time))
	(setf difference (- cur-time prev-time))
	(if (>= difference 30)
			(setf *tick-p* t))
	(if (eql *tick-p* t)
			(run-time-functions)))
(defun loop-start ()
	(loop			 
		 (ticker))))