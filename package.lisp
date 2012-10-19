;;;; package.lisp

(defpackage #:cl-grammar-of-graphics
  (:nicknames :cl-gg)
  (:use #:cl :gnuplot-interface :lisp-unit
	:gnuplot-command-strings)
  (:import-from :anaphora
		:it
		:awhen
		:aif)
  (:shadow :cl :count)
  (:export :make-column-data
	   :make-legend-guide
	   :make-axis-guide
	   :make-text-guide
	   :make-point-element
	   :make-line-element
	   :make-transformation
	   :make-gg-plot-components-container
	   :add-axis-guide
	   :add-element
	   :add-transformation
	   :gg-create-plot-command
	   :with-gnuplot-axes-labels
	   :with-gnuplot-title))

