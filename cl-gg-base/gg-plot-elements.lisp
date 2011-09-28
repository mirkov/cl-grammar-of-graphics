;; Mirko Vukovic
;; Time-stamp: <2011-09-28 15:47:08EDT gg-plot-elements.lisp>
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

(in-package :cl-gg)


(defgeneric gg-create-plot (obj)
  (:documentation "The final function that will invoke the machinery
  for creating the plot

`obj' must be a sub-class of `gg-plot-components'"))

(defclass gg-element ()
  ((data :accessor data))
  (:documentation "Definition of a plot element"))

(defclass gg-point (gg-element)
  ()
  (:documentation "gg-element of type point"))

(defun make-point-element (data)
  (let ((el (make-instance 'gg-point)))
    (setf (data el) data)
    el))

(defclass gg-line (gg-element)
  ()
  (:documentation "gg-element of type line"))


(defun make-line-element (data)
  (let ((el (make-instance 'gg-line)))
    (setf (data el) data)
    el))

(defgeneric render-element (type element)
  (:documentation "Using information in `element', a subclass of
gg-element create the commands appropriate for the plotting system
specified by the symbol/keyword `type'"))

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


(defclass gg-plot-components ()
  ((elements :accessor elements
	     :initform nil
	     :documentation "List of all plot elements.  All list
	     elements must be a sub-class of `gg-element'")
   (legends :accessor legends
	    :initform nil
	    :documentation "Legend specification.  Its interpretation
	    depends on the graphics engine.  It can be a function")
   (axes :accessor axes
	 :initform nil
	 :documentation "List of all axes specifications The ordering
of the axes specification in the list matters.  The ordering
corresponds to the following:
- x-axis
- y-axis
- (optional) z-axis, if the plot is 3 dimensional
- horizontal plots
- vertical plots")
   (text :accessor text
	 :documentation "List of all text elements in plot"))
  (:documentation "Container that stores all the components of the plot
The contents of this container are used to generate the plot"))

(defun make-gg-plot-components-container ()
  (make-instance 'gg-plot-components))

(defmethod add-element ((self gg-plot-components) (element gg-element))
  (push element (elements self)))

(defmethod add-legend-guide ((self gg-plot-components) legend)
  (push legend (legends self)))

(defmethod add-axis-guide ((self gg-plot-components) axis)
  (push axis (axes self)))

(defmethod add-text-guide ((self gg-plot-components) text)
  (setf (text self) text))

