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

;; The inner workings of the economic system and for now the trade interface 
;; will be here as well

(defvar *quantity* 0) ;; How much the merchant wants to buy or sell
(defparameter *money* 10000) ;; How much money the merchant has

(defun print-money ()
	(format t "Current funds: ~d~%" *money*))

(let ((price 0) (new-supply 0) (supply 20) (rate 14.2) (initial-price 2050))
(defun print-price ()
	(format t "Price: ~d~%" price))
(defun linear ()
	(setf price (round (+ initial-price (* supply (- rate))))))
(defun subtract-money ()
	(setf *money* (- *money* price)))
(defun add-money ()
	(setf *money* (+ *money* price)))
(defun find-new-buy-supply ()
	(setf new-supply (- supply *quantity*)))
(defun find-new-sell-supply ()
	(setf new-supply (+ supply *quantity*)))
(defun decrease-supply ()
	(setf supply (decf supply)))
(defun increase-supply ()
	(setf supply (incf supply)))
(defun set-new-supply ()
	(setf supply new-supply))

(defun buy-loop ()
	(find-new-buy-supply)
	(format t "s~d ns~d~%" supply new-supply)
	(do ((intermediate supply (1- intermediate)))
			((<= intermediate new-supply))
		(linear)
		(print-price)
		(subtract-money)
		(decrease-supply))
	(print-money))
			 
(defun sell-loop ()
	(find-new-sell-supply)
	(format t "s~d ns~d~%" supply new-supply)
	(do ((intermediate supply (1+ intermediate)))
			((>= intermediate new-supply))
		(linear)
		(print-price)
		(add-money)
		(increase-supply))
	(print-money)))
