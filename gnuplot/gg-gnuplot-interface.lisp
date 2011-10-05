;; Mirko Vukovic
;; Time-stamp: <2011-09-30 14:21:13EDT gg-gnuplot-interface.lisp>
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
		 (format nil "~a notitle"
			 (render-element :gnuplot element self))))))
    strings))


(defmethod render-element ((type (eql :gnuplot)) (element point-element)
			   (container gg-plot-components))
  (format nil "~a" (data-access-string :gnuplot (data element) container)))

(defmethod render-element ((type (eql :gnuplot)) (element line-element)
			   (container gg-plot-components))
  (format nil "~a with lines" (data-access-string :gnuplot (data element) container)))

(defmethod data-access-string ((type (eql :gnuplot)) (data column)
			       (container gg-plot-components))
  (let ((source (source data))
	(using-args
	 (if (slot-boundp container 'transformations)
	     (destructuring-bind (x-index y-index)
		 (column-index data)
	       (let ((transformations (transformations container)))
		 (list (aif (find 0 transformations :key #'dim)
			    (format nil "(~a)"
				    (insert-column-arg (def it) x-index))
			    (1+ x-index))
		       (aif (find 1 transformations :key #'dim)
			    (format nil "(~a)"
				    (insert-column-arg (def it) y-index))
			    (1+ y-index)))))
	     (mapcar #'1+ (column-index data)))))
    (assert (typep source 'pathname)
	    ()
	    "Data source must be a path")
    (format nil "'~a' using ~{~a:~a~}" source using-args)))

(define-test insert-column-arg
  (assert-equal "abc" (insert-column-arg "abc" 1))
  (assert-equal "$1abc" (insert-column-arg "$abc" 1))
  (assert-equal "a$1bc" (insert-column-arg "a$bc" 1))
  (assert-equal "abc$1" (insert-column-arg "abc$" 1)))

    
(defun insert-column-arg (def index)
  (let* ((pos (position #\$ def)))
    (if pos
	(concatenate 'string
		     (subseq def 0 (1+ pos))
		     (prin1-to-string index)
		     (subseq def (1+ pos)))
	def)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod set-gnuplot-labels ((container gg-plot-components))
    "Set gnuplot axis labels based on definitions stored in container"
    (let ((axes-defs (axes container))
	  (identifyer-string #("xlabel" "ylabel" "zlabel")))
      (dolist (def axes-defs)
	(gnuplot-interface:gnuplot-command
	 (format nil "set ~a \"~a\"" (elt identifyer-string (dim def))
		 (label def))))))
  (defmethod set-gnuplot-title ((container gg-plot-components))
    "Set gnuplot axis labels based on definitions stored in container"
    (aif (title container)
	  (gnuplot-interface:gnuplot-command
	   (format nil "set title \"~a\""  (text it))))))

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
	  (when (slot-boundp ,container 'title)
	    (set-gnuplot-title ,container))
	  ,@body)
     (gnuplot-interface:gnuplot-command
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
       (gnuplot-interface:gnuplot-command
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
		(gnuplot-interface:gnuplot-command
		 (format nil "set logscale ~a" string))
		,@body)
	   (gnuplot-interface:gnuplot-command
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
		  (gnuplot-interface:gnuplot-command
		   (format nil "set logscale ~a" string))
		  T)
	     (gnuplot-interface:gnuplot-command
	      (format nil "unset logscale ~a" string)))
	   T))
     (with-log-scales container t))))

(defmethod make-range-string ((type (eql :gnuplot)) (container gg-plot-components))
  (let* ((scales (scales container))
	 (x-scale (find 0 scales :key #'dim))
	 (y-scale (find 1 scales :key #'dim)))
    (concatenate 'string
		 (if x-scale
		     (format nil "[~a:~a] "
			     (aif (scale-min x-scale)
				  it "*")
			     (aif (scale-max x-scale)
				  it "*"))
		     "[] ")
		 (if y-scale
		     (format nil "[~a:~a] "
			     (aif (scale-min y-scale)
				  it "")
			     (aif (scale-max y-scale)
				  it ""))
		     "[] "))))

(define-test make-range-string
  (let* ((x-scale (make-interval-scale 0 :min 0.2e-8 :max 1.3e-8))
	 (y-scale (make-log-scale 1))
	 (container (make-gg-plot-components-container)))
    (add-scale container x-scale)
    (add-scale container y-scale)
    (assert-equal
     "[2.e-9:1.3e-8] [:] "
     (make-range-string :gnuplot container))))
 
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
      (sec->usec (make-transformation 0 :gnuplot "$*1e-6")))
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
	  (gnuplot-interface:gnuplot-command
	   (format nil "plot ~a ~{~a~^,~}"
		   (make-range-string :gnuplot container)
		   (gg-create-plot-command :gnuplot container))))))))

|#