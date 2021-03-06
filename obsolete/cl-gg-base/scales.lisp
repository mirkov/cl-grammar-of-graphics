;; Mirko Vukovic
;; Time-stamp: <2011-09-29 22:15:01 scales.lisp>
;; 
;; Copyright 2011 Mirko Vukovic
;; Distributed under the terms of the GNU General Public License
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; scales
(in-package :cl-gg)

(defclass scale ()
  ((dim :accessor dim
	:documentation "Identify applicable dimension - integer from 0 up"))
  (:documentation "Base class for storing scale information"))

(defmethod print-object ((self scale) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (class-name (class-of self))))

(defmethod describe-object ((self scale) stream)
  (format stream "A grammar of graphics scale object
It is of class ~a
It's associate with dimension ~a~%"
	  (class-name (class-of self)) (dim self)))

(defclass interval-scale (scale)
  ((min :accessor scale-min
	:initform nil
	:documentation "Scale minimum.  If nil, use the data minimum")
   (max :accessor scale-max
	:initform nil
	:documentation "Scale maximum. If nil, use the data maximum"))
  (:documentation "Interval scale"))

(defmethod describe-object :after ((self interval-scale) stream)
  (format stream "The minimum value is ~a
The maximum value is ~a~%"
	  (aif (min self) it "undefined")
	  (aif (max self) it "undefined")))

(defun make-interval-scale (dim &key min max)
  (let ((obj (make-instance 'interval-scale)))
    (setf (dim obj) dim)
    (when min
      (setf (scale-min obj) min))
    (when max
      (setf (scale-max obj) max))
    obj))

(defclass log-scale (interval-scale)
  ((base :accessor base
	 :initform 10
	 :documentation "Log base"))
  (:documentation "Logarithmic scale

Wilkinson 6.2.4.1"))

(defmethod describe-object :after ((self interval-scale) stream)
  (format stream "The logarithm base is ~a~%"
	  (aif (base self) it "undefined")))

(defun make-log-scale (dim &key base min max)
  (let ((obj (make-instance 'log-scale)))
    (setf (dim obj) dim)
    (when base
      (setf (base obj) base))
    (when min
      (setf (scale-min obj) min))
    (when max
      (setf (scale-max obj) max))
    obj))
