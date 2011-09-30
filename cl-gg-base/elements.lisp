;; Mirko Vukovic
;; Time-stamp: <2011-09-29 21:41:44 elements.lisp>
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

;; plot elements
(in-package :cl-gg)

(defclass gg-element ()
  ((data :accessor data))
  (:documentation "Definition of a plot element"))

(defmethod print-object ((self gg-element) stream)
  (print-unreadable-object (self stream :type t :identity t)))

(defmethod describe-object ((self gg-element) stream)
  (format stream "Grammar of Graphics Element object~%")
  (if (slot-boundp self 'data)
      (format stream "The data slot value is" (data self))))

(defclass point-element (gg-element)
  ()
  (:documentation "gg-element of type point"))

(defmethod describe-object ((self point-element) stream)
  (format stream "Grammar of Graphics point element object~%")
  (if (slot-boundp self 'data)
      (format stream "The data slot value is" (data self))))

(defun make-point-element (data)
  (let ((el (make-instance 'point-element)))
    (setf (data el) data)
    el))

(defclass line-element (gg-element)
  ()
  (:documentation "gg-element of type line"))

(defmethod describe-object ((self line-element) stream)
  (format stream "Grammar of Graphics line element object~%")
  (if (slot-boundp self 'data)
      (format stream "The data slot value is" (data self))))

(defun make-line-element (data)
  (let ((el (make-instance 'line-element)))
    (setf (data el) data)
    el))


(defgeneric render-element (type element)
  (:documentation "Using information in `element', a subclass of
gg-element create the commands appropriate for the plotting system
specified by the symbol/keyword `type'"))