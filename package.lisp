;;;; package.lisp

(defpackage #:cl-grammar-of-graphics
  (:nicknames :cl-gg)
  (:use #:cl :gnuplot-interface :lisp-unit)
  (:import-from :anaphora
		:it
		:aif)
  (:export :make-column-data
	   :make-legend-guide
	   :make-axis-guide
	   :make-text-guide
	   :make-point-element
	   :make-line-element
	   :make-gg-plot-components-container
	   :add-axis-guide
	   :add-element
	   :gg-create-plot-command
	   :with-gnuplot-axes-labels
	   :with-gnuplot-title))

