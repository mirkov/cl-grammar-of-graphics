;; Mirko Vukovic
;; Time-stamp: <2011-10-05 12:33:18EDT gg-gnuplot-interface.lisp>
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

(defclass gnuplot-plot-components (gg-plot-components)
  ((inline-data :accessor inline-data
		:initform (list)
		:documentation
"A list of data elements that will be plotted inline.  The ordering is
important.  The data elements are pushed into the list"))
  (:documentation "gg-plot-components subclass for gnuplot's needs"))

(defmethod gg-create-plot-command ((type (eql :gnuplot))
				   (self gg-plot-components))
  (let* ((elements (elements self))
	 (legend (first (legends self)))
	 (strings
	  (if legend
	      (loop for element in elements
		 collect
		 (format nil "~a title \"~a\""
			 (render-element :gnuplot element self)
			 (funcall (label legend) (data element))))
	      (loop for element in elements
		 collect
		 (format nil "~a " ;;notitle"
			 (render-element :gnuplot element self))))))
    strings))


(defgeneric render-element (type element container)
  (:documentation "Return a string that `plot' will use to render the 
element

`type' must be :gnuplot
`element' is an object of type `element'
`container' is an object of type `gg-plot-components'"))

(defmethod render-element ((type (eql :gnuplot)) (element point-element)
			   (container gg-plot-components))
  (let ((string
	 (format nil "~a" (data-access-string :gnuplot (data element) container))))
    string))

(defmethod render-element ((type (eql :gnuplot)) (element line-element)
			   (container gg-plot-components))
  (format nil "~a with lines" (data-access-string :gnuplot (data element) container)))

(defgeneric data-access-string (type data container)
  (:documentation "Create a string for plot to access the data

`type' must be :gnuplot
`element' is an object of type `element'
`container' is an object of type `gg-plot-components'"))

(defmethod data-access-string ((type (eql :gnuplot)) (data column)
			       (container gg-plot-components))
  (let ((source (source data))
	(column-index-info (column-index data)))
    (assert (typep source 'pathname)
	    ()
	    "Data source must be a path")
    (if (atom column-index-info)
	(let ((column-access-specifier
	       (if (slot-boundp container 'transformations)
		   (one-col-access-transform-maybe
		    column-index-info
		    (transformations container))
		   (1+ column-index-info))))
	  (format nil "'~a' using ~a" source column-access-specifier))
	(let ((column-access-specifier
	       (if (slot-boundp container 'transformations)
		   (two-col-access-transform-maybe
		    column-index-info
		    (transformations container))
		   (mapcar #'1+ column-index-info))))
	  (format nil "'~a' using ~{~a:~a~}" source column-access-specifier)))))

(defmethod data-access-string ((type (eql :gnuplot)) (data list-data)
			       (container gg-plot-components))
  ;; when using lisp sequences, the data access string is the special
  ;; file '-'.  More action occurs in in the main calling routine,
  ;; where the inline data is sent to gnuplot
  (push data (inline-data container))
  "'-' ")

(defmethod data-access-string ((type (eql :gnuplot)) (data matrix-data)
			       (container gg-plot-components))
  ;; when using lisp sequences, the data access string is the special
  ;; file '-'.  More action occurs in in the main calling routine,
  ;; where the inline data is sent to gnuplot
  (push data (inline-data container))
  "'-' ")

(defun two-col-access-transform-maybe
    (column-indices transformations)
  "Generate a two-column access string or index based on the two
element list `column-indices' and `transformations'.

The tranformation for dim 0 (if defined) is applied to the first list
element, and likewise for transformation for dim 1 and the second list
element

Thus, assuming tranformations \"$*5\" and \"log($)\" for dimensions 0
and 1 respectively, the list '(0 3) will return '(\"($1*5)\"
\"(log($4))\""
  (destructuring-bind (x-index y-index)
      column-indices
    (list (aif (find 0 transformations :key #'dim)
	       (format nil "(~a)"
		       (insert-column-index (def it) (1+ x-index)))
	       (1+ x-index))
	  (aif (find 1 transformations :key #'dim)
	       (format nil "(~a)"
		       (insert-column-index (def it) (1+ y-index)))
	       (1+ y-index)))))


(defun one-col-access-transform-maybe
    (column-index transformations)
  "If the `transformations' contains one for dim 1 is specified, generate a string to that effect.  Otherwise, return 1+column-index

Thus, assuming tranformations \"$*5\" dimensions 1 column-index 3 will
return return '(\"($1*5)\""
  (aif (find 1 transformations :key #'dim)
       (format nil "(~a)"
	       (insert-column-index (def it) (1+ column-index)))
       (1+ column-index)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod set-gnuplot-labels ((container gg-plot-components))
    "Set gnuplot axis labels based on definitions stored in container"
    (let ((axes-defs (axes container))
	  (identifyer-string #("xlabel" "ylabel" "zlabel")))
      (dolist (def axes-defs)
	(gnuplot-interface:command
	 (format nil "set ~a \"~a\"" (elt identifyer-string (dim def))
		 (label def))))))
  (defmethod set-gnuplot-title ((container gg-plot-components))
    "Set gnuplot axis labels based on definitions stored in container"
    (aif (title container)
	  (gnuplot-interface:command
	   (format nil "set title \"~a\""  (text it))))))

(defmacro with-gnuplot-axes-labels (container &body body)
  `(unwind-protect
	(progn
	  (set-gnuplot-labels ,container)
	  ,@body)
     (progn
       (gnuplot-interface:command
	(format nil "unset xlabel"))
       (gnuplot-interface:command
	(format nil "unset ylabel"))
       (gnuplot-interface:command
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
	 (gnuplot-interface:command
	  (format nil "unset xlabel"))
	 (gnuplot-interface:command
	  (format nil "unset ylabel"))
	 (gnuplot-interface:command
	  (format nil "unset zlabel"))))
     (with-gnuplot-axes-labels container t ))))

(defmacro with-gnuplot-title (container &body body)
  "Macro for defining textual objects in gnuplot (such as
the title) and cleaning up"
  `(unwind-protect
	(progn
	  (when (slot-boundp ,container 'title)
	    (set-gnuplot-title ,container))
	  ,@body)
     (gnuplot-interface:command
      (format nil "unset title"))))

(define-test with-gnuplot-title
  (let ((title (make-text-guide :title "Example plot"))
	(container (make-gg-plot-components-container)))
    (add-text-guide container title)
    (assert-expands
     '(unwind-protect
       (progn
	 (setf (title container) title)
	 t)
       (gnuplot-interface:command
	(format nil "unset title")))
     (with-gnuplot-title container t ))))

(defmacro with-log-scales (container &body body)
  `(let* ((scales (scales ,container))
	  (string
	   (apply #'concatenate 'string
		  (loop for scale in scales
		     when (typep scale 'log-scale)
		     collect (case (dim scale)
			       (0 "x")
			       (1 "y")
			       (t (error "Invalid dimension")))))))
     (if string
	 (unwind-protect
	      (progn
		(gnuplot-interface:command
		 (format nil "set logscale ~a" string))
		,@body)
	   (gnuplot-interface:command
	    (format nil "unset logscale ~a" string)))
	 ,@body)))

(define-test with-log-scales
  (let* ((x-scale (make-interval-scale 0 :min 0.2e-8 :max 1.3e-8))
	 (y-scale (make-log-scale 1))
	 (container (make-gg-plot-components-container)))
    (add-scale container x-scale)
    (add-scale container y-scale)
    (assert-expands
     '(let* ((scales (scales container))
	     (string
	      (apply #'concatenate 'string
		     (loop for scale in scales
			when (typep scale 'log-scale)
			collect (case (dim scale)
				  (0 "x")
				  (1 "y")
				  (t (error "Invalid dimension")))))))
       (if string
	   (unwind-protect
		(progn
		  (gnuplot-interface:command
		   (format nil "set logscale ~a" string))
		  T)
	     (gnuplot-interface:command
	      (format nil "unset logscale ~a" string)))
	   T))
     (with-log-scales container t))))

(defmethod make-range-string ((type (eql :gnuplot)) (container gg-plot-components))
  (let* ((scales (scales container))
	 (x-scale (find 0 scales :key #'dim))
	 (y-scale (find 1 scales :key #'dim)))
    (concatenate 'string
		 (if x-scale
		     (format-range 
		      (cons (aif (scale-min x-scale) it)
			    (aif (scale-max x-scale) it)))
		     (format-range))
		 (if y-scale
		     (format-range 
		      (cons (aif (scale-min y-scale) it)
			    (aif (scale-max y-scale) it)))
		     (format-range)))))

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
      (x-axis (make-axis-guide 0 "Time (sec)"))
      (y-axis (make-axis-guide 0 "y"))
      (title (make-title-text "Example plot"))
      (x-scale (make-interval-scale 0))
      (y-scale (make-log-scale 1))
      (sec->usec (make-transformation 0 :gnuplot "$*1e6")))
  (let ((e1 (make-point-element dat1))
	(e2 (make-line-element dat2))
	(container (make-gg-plot-components-container)))
    (add-element container e1)
    (add-element container e2)
    (add-legend-guide container legend)
    (add-axis-guide container y-axis)
    (add-axis-guide container x-axis)
    (setf (title container) title)
    (add-scale container x-scale)
    (add-scale container y-scale)
    (add-transformation container sec->usec)
    (with-log-scales container
      (with-gnuplot-axes-labels container
	(with-gnuplot-title container
	  (gnuplot-interface:command
	   (format nil "plot ~a ~{~a~^,~}"
		   (make-range-string :gnuplot container)
		   (gg-create-plot-command :gnuplot container))))))))

(let* ((dat1 (make-column-data
	     (merge-pathnames "columnar-data.dat"
			      *test-files-path*)
	     '6))
       (e1 (make-point-element dat1))
       (container (make-gg-plot-components-container)))
    (add-element container e1)
    (gnuplot-interface:command
     (format nil "plot ~a ~{~a~^,~}"
	     (make-range-string :gnuplot container)
	     (gg-create-plot-command :gnuplot container))))

(let* ((dat1 (make-list-data (list 1 2 8 -5)))
       (e1 (make-line-element dat1))
       (container (make-instance 'gnuplot-plot-components)))
  (add-element container e1)
  (gnuplot-interface:send-line 
   (format nil "plot ~a ~{~a~^,~}"
	   (make-range-string :gnuplot container)
	   (gg-create-plot-command :gnuplot container)))
  (awhen (inline-data container)
    (mapcar #'(lambda (inlined-data)
		(print-inline-data gnuplot-interface::*command*
				   (source inlined-data)))
	    it))
  (finish-output gnuplot-interface::*command*))

(let* ((dat1 (make-matrix-data
	      (make-array '(5 2)
			  :initial-contents
			  '((1 2)
			    (2 3)
			    (3 8)
			    (5 -2)
			    (6 0)))
	      '(0 1)))
       (e1 (make-line-element dat1))
       (container (make-instance 'gnuplot-plot-components)))
  (add-element container e1)
  (gnuplot-interface:send-line 
   (format nil "plot ~a ~{~a~^,~}"
	   (make-range-string :gnuplot container)
	   (gg-create-plot-command :gnuplot container)))
  (awhen (inline-data container)
    (mapcar #'(lambda (inlined-data)
		(print-inline-data gnuplot-interface::*command*
				   (source inlined-data)))
	    it))
  (finish-output gnuplot-interface::*command*))


|#