;; Mirko Vukovic
;; Time-stamp: <2011-09-28 17:05:14EDT gg-gnuplot-interface.lisp>
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

;; (defclass gnuplot-data (gg-data)
;;   ()
;;   (:documentation))


(defmethod gg-create-plot-command ((self gg-plot-components))
  (let* ((elements (elements self))
	 (legend (first (legends self)))
	 (strings
	  (if legend
	      (loop for element in elements
		 collect
		 (format nil "~a title \"~a\""
			 (render-element :gnuplot element)
			 (funcall (print (label legend))
				  (data element))))
	      (loop for element in elements
		 collect
		 (format nil "~a notitle"
			 (render-element :gnuplot element))))))
    strings))


(defmethod render-element ((type (eql :gnuplot)) (element gg-point))
  (format nil "~a" (data-access-string (data element))))

(defmethod render-element ((type (eql :gnuplot)) (element gg-line))
  (format nil "~a with lines" (data-access-string (data element))))

(defmethod data-access-string ((data column))
  (let ((source (source data))
	(index-info (column-index data)))
    (assert (typep source 'pathname)
	    ()
	    "Data source must be a path")
    (format nil "'~a' using ~{~a:~a~}" source index-info)))
    
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod set-gnuplot-labels ((container gg-plot-components))
    "Set gnuplot axis labels based on definitions stored in container"
    (let ((axes-defs (axes container)))
      (loop for def in axes-defs
	 for axis in '("xlabel" "ylabel" "zlabel")
	 do
	 (gnuplot-interface:gnuplot-command
	  (format nil "set ~a \"~a\"" axis (label def))))))
  (defmethod set-gnuplot-title ((container gg-plot-components))
    "Set gnuplot axis labels based on definitions stored in container"
    (let ((text-defs (text container)))
      (when (slot-boundp text-defs 'title)
	(gnuplot-interface:gnuplot-command
	  (format nil "set title \"~a\""  (title text-defs)))))))

(defmacro with-gnuplot-axes-labels (container &body body)
  `(unwind-protect
	(progn
	  (set-gnuplot-labels ,container)
	  ,@body)
     (progn
       (gnuplot-interface:gnuplot-command
	(format nil "unset xlabel"))
       (gnuplot-interface:gnuplot-command
	(format nil "unset ylabel"))
       (gnuplot-interface:gnuplot-command
	(format nil "unset zlabel")))))

(define-test with-gnuplot-axes-labels
  (let ((x-axis (make-axis-guide "Time (sec)"))
	(y-axis (make-axis-guide "n"))
	(container (make-gg-plot-components-container)))
    (add-axis-guide container y-axis)
    (add-axis-guide container x-axis)
    (assert-expands
     '(unwind-protect
       (progn
	 (set-gnuplot-labels container)
	 t)
       (progn
	 (gnuplot-interface:gnuplot-command
	  (format nil "unset xlabel"))
	 (gnuplot-interface:gnuplot-command
	  (format nil "unset ylabel"))
	 (gnuplot-interface:gnuplot-command
	  (format nil "unset zlabel"))))
     (with-gnuplot-axes-labels container t ))))

(defmacro with-gnuplot-title (container &body body)
  "Macro for defining textual objects in gnuplot (such as
the title) and cleaning up"
  `(unwind-protect
	(progn
	  (set-gnuplot-title ,container)
	  ,@body)
     (gnuplot-interface:gnuplot-command
      (format nil "unset title"))))

(define-test with-gnuplot-title
  (let ((text (make-text-guide :title "Example plot"))
	(container (make-gg-plot-components-container)))
    (add-text-guide container text)
    (assert-expands
     '(unwind-protect
       (progn
	 (set-gnuplot-title container)
	 t)
       (gnuplot-interface:gnuplot-command
	(format nil "unset title")))
     (with-gnuplot-title container t ))))

#|
(let ((dat1 (make-column-data
	     (merge-pathnames "columnar-data.dat"
			      *test-files-path*)
	     '(0 6)))
      (dat2 (make-column-data
	     (merge-pathnames "columnar-data.dat"
			      *test-files-path*)
	     '(0 7)))
      (legend (make-legend-guide
	       #'(lambda (data)
		  (format nil "~a" (second (column-index data))))))
      (x-axis (make-axis-guide "Time (sec)"))
      (y-axis (make-axis-guide "y"))
      (text (make-text-guide :title "Example plot")))
  (let ((e1 (make-point-element dat1))
	(e2 (make-line-element dat2))
	(container (make-gg-plot-components-container)))
    (add-element container e1)
    (add-element container e2)
    (add-legend-guide container legend)
    (add-axis-guide container y-axis)
    (add-axis-guide container x-axis)
    (add-text-guide container text)
    (with-gnuplot-axes-labels container
      (with-gnuplot-title container
	(gnuplot-interface:gnuplot-command
	 (format nil "plot ~{~a~^,~}"
		 (gg-create-plot-command container)))))))

|#