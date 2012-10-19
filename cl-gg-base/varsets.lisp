;; Mirko Vukovic
;; Time-stamp: <2011-10-15 11:16:26 varsets.lisp>
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

(defclass varset (gg)
  ((index :accessor index-vector
	  :documentation "0-based index of each data point")
   (length :accessor data-length
	   :documentation "number of data points")
   (order :accessor order
	       :documentation "Number of data sets in the varset")
   (domain :accessor domain
	    :initform (make-array 0 :fill-pointer 0 :adjustable t)
	    :documentation 
"List of vectors, each vector containing one data set.  The vectors
are stored in an extendable vector"))
  (:documentation "A varset"))

(defmethod describe-object ((self varset) stream)
  (format stream
	  "a varset
It contains ~a dataset(s) of length ~a
The varset domain contents are ~a~%"
	  (order self) (data-length self)
	  (domain self)))

(define-test make-varset
  (let ((vs (make-varset (list 1 2 3))))
    (assert-equal 3 (data-length vs))
    (assert-equal 1 (order vs)))
  (let ((vs (make-varset (list 1 2 3)
			 (list 8 9 10))))
    (assert-equal 3 (data-length vs))
    (assert-equal 2 (order vs)))
  (let ((vs (make-varset (vector 1 2 3))))
    (assert-equal 3 (data-length vs))
    (assert-equal 1 (order vs)))
  (let ((vs (make-varset (vector 1 2 3)
			 (vector 8 9 10))))
    (assert-equal 3 (data-length vs))
    (assert-equal 2 (order vs))))
  

(defgeneric make-varset (data &rest more-data)
  (:documentation 
   "Create a varset containing the data

data can be either simple lists or vectors

varset domain contents are stored as either a vector or a list of
vectors")
  (:method ((data list) &rest more-data)
    (let ((obj (make-instance 'varset))
	  (length (length data)))
      (setf (data-length obj) length
	    (index-vector obj) (count length))
      (vector-push-extend (coerce data 'vector) (domain obj))
      (if more-data
	  (progn
	    (dolist (next-data more-data)
	      (let ((vec (coerce next-data 'vector)))
		(assert (= length (length vec))
			()
			"All sequences must be of same length")
		(vector-push-extend vec (domain obj))))
	    (setf (order obj) (1+ (length more-data))))
	  (setf (order obj) 1))
      obj))
  (:method ((data vector) &rest more-data)
    (let ((obj (make-instance 'varset))
	  (length (length data)))
      (setf (data-length obj) length
	    (index-vector obj) (count length))
      (vector-push-extend data (domain obj))
      (if more-data
	  (progn
	    (dolist (next-data more-data)
	      (assert (= length (length next-data))
		      ()
		      "All sequences must be of same length")
	      (vector-push-extend next-data
				  (domain obj) 'vector))
	    (setf (order obj) (1+ (length more-data))))
	  (setf (order obj) 1))
      obj)))


(defclass unity-varset ()
  ()
  (:documentation 
"Unity varset"))
(defconstant +unity+ (make-instance 'unity-varset))

;; I define new types for the varset consisting of a list or a vector.
;; I could do without, but this makes the methods a bit more obvious

(define-test atom-car-p
  (assert-true (atom-car-p(list 1 2 3)))
  (assert-true (not (atom-car-p(list (list 1 2 3))))))

(defun atom-car-p (arg)
  "Return t if arg is a list and first item is an atom"
  (and (listp arg)
       (atom (car arg))))

(define-test list-varset-type
  (assert-true (typep (list 1 2 3) 'list-varset))
  (assert-true (not (typep (list (list 1 2 3)) 'list-varset))))
  
(deftype list-varset ()
  `(satisfies atom-car-p))

(define-test vector-varset-type
  (assert-true (typep (vector 1 2 3) 'vector-varset))
  (assert-true (not (typep (list  1 2 3) 'vector-varset))))

(deftype vector-varset ()
  `(satisfies vectorp))


;;;; varset utilities

(define-test copy-vector
  (let ((vec (vector 1 2 3 4)))
    (assert-numerical-equal
     #(1 2 3 4)
     (copy-vector vec)))
    (let* ((vec (vector 1 (cons 2 8) 3 4))
	   (copy (copy-vector vec)))
      (assert-equal
       (aref vec 1)
       (aref copy 1))
      (setf (cdr (aref vec 1)) 'A)
      (assert-equal 8 (cdr (aref copy 1)))))

(defun copy-vector (vector)
  "Make a copy of vector, creating new elements along the way We use
`copy-tree if a vector element is a cons.  This works for conses but
not necesseraly for deeper lists"
  (let ((copy (make-array (length vector))))
    (dotimes (i (length vector) copy)
      (let ((element (aref vector i)))
	(setf (aref copy i)
	      (if (atom element)
		  (aref vector i)
		  (copy-tree element)))))))


(define-test copy-varset
  (let* ((original (make-varset (list 1 2 3) (list 9 10 11)))
	 (copy (copy-varset original)))
    (assert-numerical-equal
     (domain original) (domain copy))
    (assert-numerical-equal
     (data-length original) (data-length copy))
    (assert-numerical-equal
     (order original) (order copy))
    (assert-numerical-equal
     (index-vector original) (index-vector copy))
    (push 'a (domain copy))
    (assert-numerical-equal
     (list #(1 2 3) #(9 10 11))
     (domain original))))
  

(defgeneric copy-domain (from-varset to-varset)
  (:documentation
"Copy `from-varset's domain into `to-varset's"))


(defgeneric copy-varset (varset)
  (:documentation
   "Make a copy of the varset, creating fresh duplicates of its
domain contents

This function is intended for use in algebra expressions")
  (:method ((varset varset))
    (let ((obj (make-instance (class-name (class-of varset))))
	  (length (data-length varset))
	  (order (order varset)))
      (setf (index-vector obj) (copy-vector (index-vector varset))
	    (data-length obj) length
	    (order obj) order
	    (domain obj)
	    (make-array order :fill-pointer 0
			:adjustable t))
      (copy-domain varset obj)
      obj)))



(defgeneric data-set (varset index)
  (:documentation
"Return the data-set stored in varset domain under index")
  (:method ((varset varset) index)
    (elt (domain varset) index)))

(define-test augment-order
  (let* ((vs1 (make-varset #(1 2 3))))
    (augment-order vs1 3)
    (assert-equal 3 (order vs1))
    (assert-numerical-equal #(1 1 1)
			    (data-set vs1 1))
    (assert-numerical-equal #(1 1 1)
			    (data-set vs1 2))))


(defgeneric augment-order (varset order)
  (:documentation "Make `varset's order match `order' by augmenting
  `varset's domain with vectors of unity")
  (:method ((varset varset) order)
    (let ((varset-order (order varset)))
      (assert (<= varset-order order)
	      ()
	      "varset order ~a must be less then or equal to order ~a"
	      varset-order order)
      (when (= varset-order order)
	(return-from augment-order nil))
      (let ((unity-vector (make-array (data-length varset)
				      :initial-element 1)))
	(dotimes (i (- order varset-order))
	  (vector-push-extend unity-vector (domain varset)))
	(setf (order varset) order)))))

;;;;;
;;;;; Unused code

;; this version of varset's uses matrices to store data
#|(defgeneric make-varset (data &rest more-data)
  (:documentation 
   "Create a varset containing the data

data can be either simple lists or vectors

varset domains are stored as either a vector or a matrix.  Each matrix
row stores one data set")
  (:method ((data list) &rest more-data)
    (let ((obj (make-instance 'varset))
	  (length (length data)))
      (setf (data-length obj) length)
      (if more-data
	  (let* ((order (1+ (length more-data)))
		 (domain (make-array (list length order))))
	    (dotimes (i length)
	      (setf (aref domain i 0) (pop data)))
	    (loop for next-data in more-data
	       with j = 1
	       do (dotimes (i length)
		    (setf (aref domain i j) (pop next-data))))
	    (setf (order obj) order
		  (domain obj) domain))
	  (setf (order obj) 1
		(domain obj) (make-array length
					  :initial-domains data)))
      obj))
  (:method ((data vector) &rest more-data)
    (let ((obj (make-instance 'varset))
	  (length (length data)))
      (setf (data-length obj) length)
      (if more-data
	  (let* ((order (1+ (length more-data)))
		 (domain (make-array (list length order))))
	    (dotimes (i length)
	      (setf (aref domain i 0) (elt data i)))
	    (loop for next-data in more-data
	       with j = 1
	       do (dotimes (i length)
		    (setf (aref domain i j) (elt next-data i))))
	    (setf (order obj) order
		  (domain obj) domain))
	  (setf (order obj) 1
		(domain obj) data))
      obj)))
|#
