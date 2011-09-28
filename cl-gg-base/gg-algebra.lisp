;; Mirko Vukovic
;; Time-stamp: <2011-09-28 10:35:15EDT gg-algebra.lisp>
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

;; this file defines the generic functions for the grammar of graphics
;; algebra.  The methods are defined in each individual engine

(defgeneric gg+ (arg1 arg2 &rest args)
  (:documentation "Apply the grammar-of-graphics `+' operator to `arg1',
`arg2' and optional rest of `args'

The argumets are object that are subclasses of the `gg-data' class
"))

(defgeneric gg* (arg1 arg2 &rest args)
  (:documentation "Apply the grammar-of-graphics `*' operator to `arg1',
`arg2' and optional rest of `args'

The argumets are object that are subclasses of the `gg-data' class
"))

(defgeneric gg/ (arg1 arg2 &rest args)
  (:documentation "Apply the grammar-of-graphics `/' operator to `arg1',
`arg2' and optional rest of `args'

The argumets are object that are subclasses of the `gg-data' class
"))