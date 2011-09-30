;; Mirko Vukovic
;; Time-stamp: <2011-09-29 22:09:45 guides.lisp>
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


;; guides
(in-package :cl-gg)



(defclass guide ()
  ((source :accessor source
	   :documentation "Guide source")
   (label :accessor label
	  :documentation "Guide label.  This is text, a function, or some
other label specification"))
  (:documentation "Base class for plot guides: axes and legends"))

(defmethod print-object ((self guide) stream)
  (print-unreadable-object (self stream :type t :identity t)))

(defmethod describe-object ((self guide) stream)
  (format stream "A grammar of graphics guide object
It is of class ~a
It's source is
It's label is"
	  (class-name (class-of self)) (source self) (label self)))

(defclass legend (guide)
  ())


(defun make-legend-guide (label &optional source)
  (let ((obj (make-instance 'legend)))
    (setf (label obj) label)
    (when source
      (setf (source obj) source))
    obj))

(defclass axis (guide)
  ())

(defun make-axis-guide (label &optional source)
  (let ((obj (make-instance 'axis)))
    (setf (label obj) label)
    (when source
      (setf (source obj) source))
    obj))

(defclass text ()
  ((text :accessor text
	 :documentation "Store the string to be displayed or a function
	 that will generate the string"))
  (:documentation "Store text associated with the plot"))

(defmethod print-object ((self text) stream)
  (print-unreadable-object (self stream :type t :identity t)))

(defmethod describe-object ((self text) stream)
  (format stream "A grammar of graphics text object
It is of class ~a
The text string is ~a"
	  (class-name (class-of self)) (text self)))

(defclass title-text (text)
  ()
  (:documentation "Store the graphics title"))

(defun make-title-text (text)
  (let ((obj (make-instance 'title-text)))
    (setf (text obj) text)
    obj))

(defclass footnote-text (text)
  ()
  (:documentation "Store the graphics' footnote"))

(defun make-footnote-text (text)
  (let ((obj (make-instance 'footnote-text)))
    (setf (text obj) text)
    obj))



