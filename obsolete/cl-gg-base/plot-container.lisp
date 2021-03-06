;; Mirko Vukovic
;; Time-stamp: <2011-10-03 09:55:58 plot-container.lisp>
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


;; The plot information is stored in a container
(in-package :cl-gg)


(defgeneric gg-create-plot (obj)
  (:documentation "The final function that will invoke the machinery
  for creating the plot

`obj' must be a sub-class of `gg-plot-components'"))



(defclass gg-plot-components ()
  ((transformations :accessor transformations
		    :documentation "List of transformations associated
		    with a plot"
		    :initform (list))
   (scales :accessor scales
	   :documentation "Scale information"
	   :initform (list))
   (elements :accessor elements
	     :initform (list)
	     :documentation "List of all plot elements.  All list
	     elements must be a sub-class of `gg-element'")
   (axes :accessor axes
	 :initform (list)
	 :documentation "List of all axes specifications")
   (legends :accessor legends
	    :initform (list)
	    :documentation "Legend specification.  Its interpretation
	    depends on the graphics engine.  It can be a function")
   (title :accessor title
	 :documentation "Graphics title")
   (footnote :accessor footnote
	 :documentation "Graphics footnote"))
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

(defmethod add-scale ((self gg-plot-components) (scale scale))
  (push scale (scales self)))

(defmethod add-footnote ((self gg-plot-components) (footnote footnote-text))
  (push footnote (footnote self)))

(defmethod add-transformation ((self gg-plot-components) (transformation transformation))
  (push transformation (transformations self)))


