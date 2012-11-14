;; Mirko Vukovic
;; Time-stamp: <2011-10-10 23:57:51 algebra-forms.lisp>
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
;; These file contains classes and methods that combine varsets into
;; algebra factors, terms, and forms.  

;; the classes for algebra factors, terms and forms store the data in
;; a form very similar to the layout in Sect 5.1.2 of tgog.  The
;; meta-data discussed in 5.1.4 and 5.1.4.1 is stored in classes
;; defined in this filel

;; gg algebra returns expressions.  We identify a few classes to
;; identify different expression types.  Algebra expression objects
;; contain meta-information that facilitates their rendering by the
;; geometry methods

(defmethod vector-equal-p (v1 v2)
  (let ((l (length v1)))
    (and (equal l (length v2))
	 (dotimes (i l t)
	   (unless (equal (elt v1 i) (elt v2 i))
	     (return nil))))))

(defmacro assert-vector-equal (arg1 arg2)
  `(assert-true (vector-equal-p ,arg1 ,arg2)))

(defclass algebra-expression (gg)
  ()
  (:documentation 
"Base class for the algebra expressions.  The algebra operators
generate algebra expressions"))

(defclass factor (varset algebra-expression)
  ((tagged-p :accessor tagged-p
	     :initform nil
	     :documentation
"Flags whether the varset is tagged or not"))
  (:documentation
"A factor is a term with no `*' operator.  It is either A or A/B

A factor is made by either (make-varset lisp-sequence) or
by nesting two varsets

In tagged factors, the elements are stored as dotter pairs.  The
element value is `first' and the tag is `rest' (not `second' because
the tag itself might be a list."))

(defmethod describe-object :after ((self factor) stream)
  (format stream
	  "This varset is of type ~a
tagged-p value is ~a"
	  (class-name (class-of self)) (tagged-p self)))

(defclass tuple-factor (factor)
  ()
  (:documentation
"Store a tuple"))

(defclass file-factor (factor)
  ()
  (:documentation
"A column in a file is a factor.  The data is stored in a two-term
vector The first element is the pathname or stream.  The optional
second element is the column accessor"))

(defgeneric make-factor (data &rest rest)
  (:documentation 
   "Create a factor containing the data

data can be either simple list or a vector")
  (:method ((data vector) &rest ignorable)
    (declare (ignore ignorable))
    (let ((obj (make-instance 'tuple-factor))
	  (length (length data)))
      (setf (data-length obj) length
	    (index-vector obj) (vector 0 length))
      (vector-push-extend data (domain obj))
      (setf (order obj) 1)
      obj))
  (:method ((data list) &rest ignorable)
    (declare (ignore ignorable))
    (make-factor (coerce data 'vector)))
  (:method ((data pathname) &rest column)
    (let ((obj (make-instance 'file-factor))
	  (length (if column 2 1)))
      (setf (data-length obj)
	    length)
      (setf (data-length obj) length 
	    (index-vector obj) (vector 0 length))
      (when column
	(setf (column obj) column))
      (vector-push-extend
       (if column (vector data column)
	   (vector data))
       (domain obj))
      (setf (order obj) 1)
      obj)))

(defgeneric nested-values (factor)
  (:documentation
   "Return the `factor's nested values")
  (:method ((self factor))
    (map 'vector #'(lambda (cons)
		     (car cons))
	 (elt (domain self) 0))))

(defgeneric nesting-values (factor)
  (:documentation
   "Return the `factor's nesting values")
  (:method ((self factor))
    (map 'vector #'(lambda (cons)
		     (cdr cons))
	 (elt (domain self) 0))))


(let ((lf (string #\linefeed)))
  (defmethod print-inline-data (stream (data factor))
    (let ((data-vector (elt (domain data) 0)))
      (if (tagged-p data)
	  (dotimes (i (data-length data))
	    (princ lf stream)
	    (princ (car (elt data-vector i)) stream))
	  (dotimes (i (data-length data))
	    (princ lf stream)
	    (princ (elt data-vector i) stream)))
      (princ lf stream)
      (princ "e" stream)
      (princ lf stream)))

  (defmethod print-inline-data (stream (data term))
    (dotimes (i (data-length data))
      (princ lf stream)
      (dotimes (j (order data))
	(princ (elt (elt (domain data) j) i) stream)
	(princ " " stream)))
    (princ lf stream)
    (princ "e" stream)
    (princ lf stream)))

(defclass term (varset algebra-expression)
  ((tag-count :accessor tag-count
	      :initform 0
	      :documentation
"The number of tags applied the data")
   (tagged-varsets :accessor tagged-varsets
		   :initform (make-array 0 :adjustable t :fill-pointer 0)
		   :documentation
"Vector of indices that point to the tagged varsets.

The indices are a cons whose car is the tag identifier, a symbol, and
the cdr is the index, pointing to the tagged varset in the domain
slot")
   (order :accessor order
	  :initform 0
	  :documentation "Order of the term"))
  (:documentation
"A cross & nest combination of varsets.  No blends.  All varsets have
the same length

A term is obtained by crossing a combination of varsets and factors"))

(defmethod describe-object :after ((self term) stream)
  (format stream
	  "This varset is of type ~a
It has ~a tagged varsets
Their indices are ~a~%"
	  (class-name (class-of self)) (tag-count self)
	  (tagged-varsets self)))

(defmethod copy-varset :around ((self term))
  (let ((obj (call-next-method)))
    (setf (tag-count obj) (tag-count self))
    ;; I copy tagged varsets into an adjustable array
    (setf (tagged-varsets obj)
	  (make-array (tag-count self)
		      :adjustable t :fill-pointer (tag-count self)
		      :initial-contents (coerce (tagged-varsets self)
						'list)))
    obj))

(defclass form (varset algebra-expression)
  ((form-order :accessor form-order
	       :documentation
"Number of factors in the terms of the form")
   (term-count :accessor term-count
	       :documentation
"Number of terms in the form")
   (tag-info :accessor tag-info
	     :documentation
"Stores tag information for each term of the form")
   (term-start-index :accessor term-start-index
		:initform (make-array 1 :adjustable t
				      :fill-pointer t
				      :initial-element 0)
		:documentation
"Store the begining and ending index of each term block.  The i-th
block is a (subseq (start-index i) (start-index (1+ i)

Length of term-start-index is 1+term-count.  Last element's value is
the total length of the vectors in domain"))
  (:documentation 
"Store an alebraic form, and information about its domain: number of
terms and its order

A form is produced by the blending varsets, factors and terms.  All
terms in the form are of the same order

The geometry engine operates on forms"))

(defmethod describe-object :after ((self form) stream)
  (format stream
	  "The varset is of type ~a
The number of terms in the form is ~a
"
	   (class-name (class-of self)) (term-count self)))

(define-test nest
  (let* ((v1 (make-varset (list 1 2 3)))
	 (v2 (make-varset (list "A" "b" "$")))
	 (nest-vv (nest v1 v2))
	 (nest-vx (nest v1 'tag))
	 (nest-vn (nest v1 nest-vv)))
    (assert-equal 1 (order nest-vv))
    (assert-equal 3 (data-length nest-vv))
    (assert-true (tagged-p nest-vv))
    (assert-equal 'factor (class-name (class-of nest-vv)))
    (assert-vector-equal (vector '(1 . "A") '(2 . "b") '(3 . "$"))
			 (elt (domain nest-vv) 0))
    (assert-equal 1 (order nest-vx))
    (assert-equal 3 (data-length nest-vx))
    (assert-true (tagged-p nest-vx))
    (assert-equal 'factor (class-name (class-of nest-vx)))
    (assert-vector-equal #('(1 . tag) '(2 . tag) '(3 . tag))
			 (elt (domain nest-vx) 0))
    (assert-equal '(1 . (1 . "A")) (elt (elt (domain nest-vn) 0) 0))))

(defgeneric nest (varset arg)
  (:documentation
"Apply the nesting `/' operator to a `factor', producing another
`factor'

`factor's order must be unity

arg can be a factor or a scalar (a string, number, symbol)
")
  (:method :before ((v varset) arg)
	   (declare (ignore arg))
	   (assert (= 1 (order v)) ()
		   "The order of first argument must be unity"))
  (:method ((v1 tuple-factor) (v2 tuple-factor))
    (let ((obj (change-class (copy-varset v1) 'tuple-factor))
	  (l (data-length v1)))
      (assert (= l (data-length v2))
	      ()
	      "nested varsets must have the same length")
      (setf (tagged-p obj) t)
      (dotimes (i l)
	(let ((e (elt (elt (domain v1) 0) i))
	      (tag (let ((v2-order (order v2)))
		     (if (eq 1 v2-order)
			 (elt (elt (domain v2) 0) i)
		     (loop for j from 0 below v2-order
			  collect (elt (elt (domain v2) j) i))))))
	  (setf (elt (elt (domain obj) 0) i) (cons e tag))))
      obj))
  (:method ((v1 tuple-factor) v2)
    (let ((obj (change-class (copy-varset v1) 'tuple-factor))
	  (l (data-length v1)))
      (setf (tagged-p obj) t)
      (dotimes (i l)
	(let ((e (elt (elt (domain v1) 0) i)))
	  (setf (elt (elt (domain obj) 0) i) (cons e v2))))
      obj))
  (:method ((v1 file-factor) v2)
"Tagging information is stored as a dotted pair:
 ( #(path &optional column) . tag )
"
    (let ((obj (change-class (copy-varset v1) 'file-factor)))
      (setf (tagged-p obj) t)
      (let ((e (elt (domain v1) 0)))
	(setf (elt (domain obj) 0)
	      (cons e v2)))
      obj)))



(define-test cross
  (let* ((v1 (make-factor (list 1 2 3)))
	 (v2 (make-factor (list #\A #\B #\$)))
	 (v3 (make-factor (list 'A 'B 'C)))
	 (v4 (make-factor (list "list" "of" "words"))))
    (let  ((cvv (cross v1 v2)))
      (assert-equal 2 (order cvv))
      (assert-vector-equal #(1 2 3) (elt (domain cvv) 0)))
    (let ((cvf (cross (nest v1 v2) v3)))
      (assert-equal 1 (tag-count cvf))
      (assert-numerical-equal #(0) (tagged-varsets cvf)))
    (let ((cfv (cross v3 (nest v1 v2))))
      (assert-numerical-equal #(1) (tagged-varsets cfv)))
    (let ((cff (cross (nest v1 v2) (nest v3 v4))))
      (assert-equal 2 (tag-count cff))
      (assert-numerical-equal #(0 1) (tagged-varsets cff)))
    (let ((ctv (cross (cross v3 (nest v1 v2)) (nest v4 v1))))
      (assert-equal 3 (order ctv))
      (assert-equal 2 (tag-count ctv))
      (assert-numerical-equal #(1 2) (tagged-varsets ctv)))))

(defmethod copy-domain ((from-varset varset) (to-varset varset))
  (dotimes (i (order from-varset))
    (vector-push-extend (copy-vector
			 (elt (domain from-varset) i))
			(domain to-varset))))

(defmethod copy-domain ((from-varset varset) (to-varset varset))
  (dotimes (i (order from-varset))
    (vector-push-extend (copy-vector
			 (elt (domain from-varset) i))
			(domain to-varset))))


(defgeneric cross (varset1 varset2)
  (:documentation 
   "Apply the  cross `*' operator to `varset1' and `varset2'.

`varset1' can be either a `varset', a `factor' or a `term'
`varset2' must be a `varset' or a `factor' and have order 1

The function must return an object of `term' class
")
  (:method ((v1 tuple-factor) (v2 tuple-factor))
    (assert (= 1 (order v2)) ()
	    "The second argument must be a varset of order 1")
    (assert (= (data-length v1) (data-length v2)) () 
	    "Data lengths of crossed  varsets must be equal.  They are ~a and ~a"
	    (data-length v1) (data-length v2))
    #|(assert (not (equal (class-name (class-of v1)) 'form))
	    ()
	    "Varset1 cannot be a `form' object")
    (assert (not (equal (class-name (class-of v2)) 'form))
	    ()
	    "Varset2 cannot be a `form' object")|#
    (let ((term-varset (change-class (copy-varset v1)
				     'term)))
      (incf (order term-varset))
      (vector-push-extend (elt (domain v2) 0)
			  (domain term-varset))
      (when (and (equal (class-name (class-of v1))
			'factor)
		 (tagged-p v1))
	(incf (tag-count term-varset))
	(vector-push-extend 0 (tagged-varsets term-varset)))
      (when (and (equal (class-name (class-of v2))
			'factor)
		 (tagged-p v2))
	(incf (tag-count term-varset))
	(vector-push-extend (1- (order term-varset))
			    (tagged-varsets term-varset)))
      term-varset))
  (:method ((v1 file-factor) (v2 file-factor))
    (let ((term-varset (change-class (copy-varset v1)
				     'term)))
      (incf (order term-varset))
      (vector-push-extend (elt (domain v2) 0)
			  (domain term-varset))
      (when (tagged-p v1)
	(incf (tag-count term-varset))
	(vector-push-extend 0 (tagged-varsets term-varset)))
      (when (and (equal (class-name (class-of v2))
			'factor)
		 (tagged-p v2))
	(incf (tag-count term-varset))
	(vector-push-extend (1- (order term-varset))
			    (tagged-varsets term-varset)))
      term-varset)))

  
(define-test blend
  (let ((v1 (make-factor (list 1 2 3)))
	(v2 (make-factor (list "A" "b" "$" "K"))))
    (let ((form1 (blend v1 v2)))
      (with-slots (order length term-start-index term-count) form1
	(assert-equal 1 order)
	(assert-equal 2 term-count)
	(assert-equal 7 length)
	(assert-vector-equal #(0 3 7) term-start-index)
	(assert-vector-equal #(1 2 3)
			     (subseq (elt (domain form1) 0)
				     (elt term-start-index 0)
				     (elt term-start-index 1)))
	(assert-vector-equal #("A" "b" "$" "K")
			     (subseq (elt (domain form1) 0)
				     (elt term-start-index 1)
				     (elt term-start-index 2)))))
    (let* ((w1 (make-factor (list 11 12 13)))
	   (c1 (cross v1 w1))
	   (form2 (blend c1 v2)))
      (with-slots (order length term-start-index term-count
			 domain) form2
	(assert-equal 2 order)
	(assert-equal 2 term-count)
	(assert-equal 7 length)
	(assert-vector-equal #(0 3 7) term-start-index)
	(assert-vector-equal #(1 2 3)
			     (subseq (elt domain 0)
				     (elt term-start-index 0)
				     (elt term-start-index 1)))
	(assert-vector-equal #(11 12 13)
			     (subseq (elt domain 1)
				     (elt term-start-index 0)
				     (elt term-start-index 1)))
	(assert-vector-equal #("A" "b" "$" "K")
			     (subseq (elt domain 0)
				     (elt term-start-index 1)
				     (elt term-start-index 2)))
	(assert-vector-equal #(1 1 1 1)
			     (subseq (elt domain 1)
				     (elt term-start-index 1)
				     (elt term-start-index 2)))))
    (let* ((v1 (make-factor (list 1 2 3)))
	   (v2 (make-factor (list "X" "Y" "Z")))
	   (v3 (make-factor (list "a" "text" "string")))
	   (v4 (make-factor (list "A" "b" "$" "K")))
	   (c2 (cross (nest v1 v3) v2))
	   (form3 (blend c2 v4)))
      (with-slots (order length term-start-index term-count
			 tag-info domain) form3
	(assert-equal 2 order)
	(assert-equal 2 term-count)
	(assert-equal 7 length)
	(assert-vector-equal #(0 3 7) term-start-index)
	(assert-vector-equal #((1 . "a") (2 . "text") (3 . "string"))
			     (subseq (elt domain 0)
				     (elt term-start-index 0)
				     (elt term-start-index 1)))
	(assert-vector-equal #("X" "Y" "Z")
			     (subseq (elt domain 1)
				     (elt term-start-index 0)
				     (elt term-start-index 1)))
	(assert-vector-equal #("A" "b" "$" "K")
			     (subseq (elt domain 0)
				     (elt term-start-index 1)
				     (elt term-start-index 2)))
	(assert-vector-equal #(1 1 1 1)
			     (subseq (elt domain 1)
				     (elt term-start-index 1)
				     (elt term-start-index 2)))
	(assert-vector-equal #(0) (elt tag-info 0))
	(assert-true (not (elt tag-info 1)))))))

(defgeneric blend (varset1 varset2 &rest varsets)
  (:documentation 
   "Apply the `+' operator to `varset1',`varset2' and optional rest of
`varsets'

Blending concatenates two varsets

The arguments can be any of the class `factor' or `term'

The function must return an object of `form' class
")
  (:method ((v1 algebra-expression)
	    (v2 algebra-expression)
	    &rest varsets)
    (when varsets
      (assert (every #'(lambda (arg)
			 (multiple-value-bind (first second)
			     (subtypep (class-name (class-of arg))
				       'algebra-expression)
			   (and first second)))
		     varsets)
	      ()
	      "All arguments must be a subtype of algebra-expression"))
    (let* ((varsets (append (list v1 v2) varsets))
	   (expression-order (loop for varset in varsets
				maximize (order varset))))
      (assert (not (zerop expression-order)) ()
	      "Expression order evaluated to zero.  Something wrong with arguments")
      (dolist (varset varsets)
	(augment-order varset expression-order))
      (let ((obj (make-instance 'form)))
	(with-slots (length order term-count
			    index
			    term-start-index
			    domain tag-info) obj
	  (setf length (loop for varset in varsets
			  sum (data-length varset))
		order expression-order
		term-count (length varsets)
		term-start-index
		(loop for varset in varsets
		   with start-index = (list 0)
		   with sum = 0
		   do (incf sum (data-length varset))
		   do (push sum start-index)
		   finally (return 
			     (coerce (nreverse start-index) 'vector)))
		domain (make-array order)
		index (make-array length)
		tag-info (make-array order :initial-element nil))
	  (dotimes (i order)
	    (setf (elt domain i)
		  (make-array length)))
	  (loop for varset in varsets
	     for i from 0 by 1
	     do (setf (subseq index
			      (elt term-start-index i)
			      (elt term-start-index (1+ i)))
		      (index-vector varset))
	     do (dotimes (j order)
		  (setf (subseq (elt domain j)
				(elt term-start-index i)
				(elt term-start-index (1+ i)))
			(elt (domain varset) j)))
	     when (and (typep varset 'factor)
		       (tagged-p varset))
	     do (setf (elt tag-info i)
		      t)
	     when (and (typep varset 'term)
		       (not (zerop (tag-count varset))))
	     do (setf (elt tag-info i)
		      (tagged-varsets varset))))
	obj))))

	    

	   