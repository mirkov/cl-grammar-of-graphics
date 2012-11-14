;; Mirko Vukovic
;; Time-stamp: <2011-09-30 12:46:43EDT transformations.lisp>
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

;; Implementation of transformations associated with a particular
;; dimensions.  We do not use transformations to generate new data out
;; of old.  We do have lisp for that after all.
;;
;; Transformations can be associated with a grahics engine, because
;; some of them can be accomplished there.

(in-package :cl-gg)

(defclass transformation ()
  ((dim :accessor dim)
   (engine :accessor engine)
   (def :accessor def))
  (:documentation "Transformation associated with a particular
  dimension"))

(defmethod print-object ((self transformation) stream)
  (print-unreadable-object (self stream :type t :identity t)))

(defmethod describe-object ((self transformation) stream)
  (format stream "A grammar of graphics transformation object
It is of class ~a
It is associated with dimension ~a
It is associated with the rendering engine ~a
It's definition is ~a"
	  (class-name (class-of self)) (dim self) (engine self)
	  (def self)))

(defun make-transformation (dim engine def)
  (let ((obj (make-instance 'transformation)))
    (setf (dim obj) dim
	  (engine obj) engine
	  (def obj) def)
    obj))

