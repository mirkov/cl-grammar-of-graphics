;; Mirko Vukovic
;; Time-stamp: <2011-09-28 21:29:46 guides.lisp>
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
  ((title :accessor title)
   (footnote :accessor footnote))
  (:documentation "Store text associated with the plot"))

(defun make-text-guide (&key title footnote)
  (let ((obj (make-instance 'text)))
    (when title
      (setf (slot-value obj 'title) title))
    (when footnote
      (setf (footnote obj) footnote))
    obj))

