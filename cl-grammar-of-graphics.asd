;;;; cl-grammar-of-graphics.asd

(asdf:defsystem #:cl-grammar-of-graphics
  :serial t
  :components
  ((:module "cl-gg-init"
	   :pathname #p"./"
	   :components ((:file "package")))
   (:module "cl-gg-base"
	    :depends-on ("cl-gg-init")
	    :pathname #p"cl-gg-base/"
	    :components ((:file "gg-data")
			 (:file "transformations")
			 (:file "gg-algebra")
			 (:file "scales")
			 (:file "elements")
			 (:file "guides")
			 (:file "plot-container")))
   (:module "gg-interface-declarations"
	    :depends-on ("cl-gg-base"))
   (:module "gg-gnuplot"
	    :pathname #p"gnuplot/"
	    :depends-on ("gg-interface-declarations")
	    :components ((:file "gg-gnuplot-interface")))
   (:module "testing"
	    :depends-on ("cl-gg-init")
	    :components ((:file "init-testing"))))
  :depends-on (:lisp-unit
		:anaphora
		:gnuplot-interface
		:gnuplot-command-strings))


