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

(load "time.lisp")

(defvar *quantity* 0) ;; How much the merchant wants to buy or sell
(defparameter *money* 10000) ;; How much money the merchant has
(defvar *supply* 0)
(defvar *supply-p* nil)

(defun print-money () ;; Shows how much money the player has
	(format t "Current funds: ~d~%" *money*))

(let ((price 0) (new-supply 0) (supply 20) (rate 14.2) (initial-price 2050)
			(price-list 0) (average-price 0) (buy-p nil) (sell-p nil))
(defun reset-price-list ()
	(setf price-list nil))
(defun add-price ()
	(push price price-list))
(defun average-price-function (price-list)
	(setf average-price (let ((list-nonil (remove nil price-list)))
		(/ (apply #'+ list-nonil) (length list-nonil)))))
(defun print-price () ;; Shows how much the unit cost; will be replaced by a
	(format t "Price: ~d~%" price)) ;; price averager
(defun print-average-price ()
	(format t "Average price per unit: ~d~%" average-price))
(defun linear () ;; A linear plot for any good
	(setf price (round (+ initial-price (* supply (- rate))))))
(defun subtract-money ()
	(setf *money* (- *money* price)))
(defun add-money ()
	(setf *money* (+ *money* price)))
(defun find-new-buy-supply () ;; Necessary to get the loop to stop
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
		(add-price)
		(print-price)
		(subtract-money)
		(decrease-supply))
	(average-price-function price-list)
	(print-average-price)
	(reset-price-list)
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
	(print-money))

;; Trade menus

(defvar *goods-db* nil)
(defparameter *goods-menu*
	`(("Beer" . ,(lambda ()))
		("Fruit Juice" . ,(lambda ()))
		("Wine" . ,(lambda ()))
		("Fish" . ,(lambda ()))
		("Fruit" . ,(lambda ()))
		("Grain" . ,(lambda ()))
		("Honey" . ,(lambda ()))
		("Meat" . ,(lambda ()))
		("Roots" . ,(lambda ()))
		("Salt" . ,(lambda ()))
		("Vegetables" . ,(lambda ()))
		("Medicine" . ,(lambda ()))
		("Bronze Ingots" . ,(lambda ()))
		("Bronze Wares" . ,(lambda ()))
		("Cloth" . ,(lambda ()))
		("Clothing" . ,(lambda ()))
		("Dyes" . ,(lambda ()))
		("Glass" . ,(lambda ()))
		("Hemp" . ,(lambda ()))
		("Furs" . ,(lambda ()))
		("Iron Ingots" . ,(lambda ()))
		("Iron Wares" . ,(lambda ()))
		("Leather" . ,(lambda ()))
		("Pitch" . ,(lambda ()))
		("Pottery" . ,(lambda ()))
		("Stone" . ,(lambda ()))
		("Whale Oil" . ,(lambda ()))
		("Wool" . ,(lambda ()))
		("Leave" . ,(lambda ()))))
(defparameter *trade-menu*
	`(("Buy" . ,(lambda ()
											(set-buy-status)
											(format t "What would you like to buy?~%")
											(goods-menu *goods-menu*)))
		("Sell" . ,(lambda ()
											 (set-sell-status)
											 (format t "What would you like to sell?~%")
											 (goods-menu *goods-menu*)))
		("Leave" . ,(lambda () (format t "Come again sometime.~%")))))


(defun load-goods-db (&optional (goods.db "./goods.db"))
  (with-open-file (in goods.db) (read in)))

(defun trade-interface-start ()
	(market-menu "Welcome to the market." *trade-menu*))

(defun set-buy-status ()
	(setf buy-p t))
(defun reset-buy-status () ;; Used to initiate conditional functions based on 
	(setf buy-p nil))        ;; whether the player is buying or selling
(defun set-sell-status ()
	(setf sell-p t))
(defun reset-sell-status ()
	(setf sell-p nil))
(defun set-quantity-prompt ()
	(setf *quantity* (or (parse-integer (read-line *query-io*) :junk-allowed t) 0)))

(defun market-menu (title entries)
	(format t "~a~%" title)
	(dolist (entry entries)
    (format t "~a~%" (car entry)))
	(format t "Type the word corresponding to your choice.~%Typing leave will return you to the city.~%")
	(let ((input (read-line)))
		(dolist (entry entries)
			(when (string-equal input (car entry))
        (funcall (cdr entry)) (return)))
		(unless
				(dolist (entry entries)
					(when (string-equal input (car entry))
						(funcall (cdr entry)) (return t)))
			(format t "~%~%That's not an option.~%")
			(trade-interface-start))))
(defun goods-menu (entries)
	(dolist (entry entries)
    (format t "~a~%" (car entry)))
	(format t "Type the word corresponding to your choice.~%Typing cancel will return you to the previous menu.~%")
	(let ((input (read-line)))
		(dolist (entry entries)
			(when (string-equal input (car entry))
        (funcall (cdr entry)) (return)))
		(unless
				(dolist (entry entries)
					(when (string-equal input (car entry))
						(funcall (cdr entry)) (return t)))
			(format t "~%~%That's not an option.~%")
			(goods-menu *goods-menu*))))
(defun trade-menu ()
	(when (eql buy-p t)
	(format t "How much would you like to buy?~%")
	(set-quantity-prompt)
	(buy-loop)
	(reset-buy-status)
	(repeat-trade))
	(when (eql sell-p t)
	(format t "How much would you like to sell?~%")
	(set-quantity-prompt)
	(sell-loop)
	(reset-sell-status)
	(repeat-trade)))
(defun repeat-trade ()
	(y-or-n-p "Would you like to trade anything else?  ")
	(if (eql *query-io* t)
			(market-menu "What else would you like to do?" *trade-menu*))))
