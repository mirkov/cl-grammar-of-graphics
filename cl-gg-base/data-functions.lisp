;; Mirko Vukovic
;; Time-stamp: <2011-10-06 20:26:30 data-functions.lisp>
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


(define-test iterate
  (assert-numerical-equal #(0 2 4) (iterate 0 5 2)))
(defun iterate (from to &optional (step 1))
  (coerce 
   (loop for i from from below to by step
      collect i)
   'vector))

(define-test count
  (assert-numerical-equal #(0 1 2) (count 3)))

(defun count (n)
  (iterate 0 n))