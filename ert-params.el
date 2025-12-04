;;; ert-params.el --- summary -*- lexical-binding: t -*-

;; Author: Sven Johansson (johansson.sven@gmail.com)
;; Maintainer: Sven Johansson (johansson.sven@gmail.com)
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Homepage: https://www.github.com/svjson/scoot
;; Keywords: test, ert, convenience, extension

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert)


;; Utility functions

(defun ert-params--generator-indices (case-params)
  "Locate parameters declared as :generator in CASE-PARAMS.

This function is intended for internal use during test macro expansion
and returns a list of the indices of any case parameters that uses the
:generator keyword.

Example:
  \(ert-params--generator-indices
   `\(\(\"case-with-generators\"
      \(:literal 42)
      \(:generator (number-sequence 1 3))
      \(:generator (list \"A\" \"B\" \"C\"))))
  => \(1 2)

If no generators are present this function returns nil/empty list."
  (let (gen-indices)
    (cl-loop for item in case-params
             for index from 0
             do
             (when (and (listp item)
                        (memq (car item) '(:gen :generator)))
               (push index gen-indices)))
    (nreverse  gen-indices)))

(defun ert-params--expand-generators (case)
  "Expand all :generator forms in CASE.

This function is intended for interal use during test macro expansion
and returns a list of expressed cases in a single test case form.

This basically means that if a test case form contains no :generator
parameters it simply returns a list with CASE as its only element.

If the form does contain :generator parameters the return value is
a list of cases containg a case for each value of generators, where
the generator expressions themselves have been substituted with one
of its values."
  (if-let* ((gen-indices (nreverse (ert-params--generator-indices (cdr case))))
            (gen-values-alist
             (mapcar
              (lambda (idx)
                (let ((gen-form (cadadr (nth idx (cdr case)))))
                  (cons idx (eval gen-form))))
              gen-indices))
            (count (apply #'max (mapcar #'length (mapcar #'cdr gen-values-alist)))))
      (progn
        (cl-loop for pi in (number-sequence 0 (1- count))
                 collect
                 (let* (sel
                        (vals (cl-loop for item in (cdr case)
                                       for index from 0
                                       collect
                                       (if (memq index gen-indices)
                                           (list (caadr item)
                                                 (let ((val (nth pi (alist-get index gen-values-alist))))
                                                   (push val sel)
                                                   val))
                                         item))))
                   (append (list (apply #'format (car case) (nreverse sel)))
                         vals))))
    (list case)))

(defun ert-params--expand-cases (cases)
  "Expand all test cases in CASES with :generator params.

Takes a list of test case forms and performs expansion of generators
on any case that contains them and returns a new flat list of all
expressed test cases."
  (apply #'append
         (mapcar (lambda (case-spec)
                   (let ((lcase (if (symbolp case-spec)
                                    (progn
                                      (message "%S" case-spec)
                                      (eval case-spec))
                                  case-spec)))
                     (if-let ((gen-pos (seq-position (cdr lcase)
                                                     :generator
                                                     (lambda (a b)
                                                       (eq (car a) b)))))
                         (ert-params--expand-generators lcase)
                       (list lcase))))
                 cases)))



;; deftest macros

(defmacro ert-deftest-parametrized (base-name args params &rest body)
  "Define a group of parametrized ERT tests.

BASE-NAME is a symbol or string used as the prefix.
PARAMS is a list of rows:

  ((\"case-name\"
    (:fun     FORM)
    (:literal FORM)
    (:eval    FORM)
   (...))

ARGS is a list of symbols, one per parameter position in each row.
The first parameter in a row binds to the first symbol in ARGS, etc.

BODY is the test body, evaluated with:
  - all :fun parameters bound via cl-flet
  - all :literal/:eval parameters bound via let.
  - all :generator parameters expand to one test case per generator value."
  (declare (indent 3))
  (let* ((prefix (if (symbolp base-name)
                     (symbol-name base-name)
                   base-name))
         (cases (ert-params--expand-cases params)))
    `(progn
       ,@(mapcar
          (lambda (row)
            (let* ((case-name (car row))
                   (test-name (intern (format "%s--%s" prefix case-name)))
                   (items (cdr row))
                   (funcs nil)
                   (values nil))
              (unless (= (length items) (length args))
                (error "In ert-deftest-parametrized: row %S has %d parameters but ARGS has %d"
                       (car row) (length items) (length args)))
              (cl-loop for item in items
                       for sym-name in args
                       do
                       (pcase item
                         (`(:fun ,form)
                          (push `(,sym-name () ,form) funcs))
                         (`(:literal ,form)
                          (push `(,sym-name (quote ,form)) values))
                         (`(:eval ,form)
                          (push `(,sym-name ,form) values))
                         (_ (error "Unknown parameter tag in case '%s':  %S" case-name item))))
              `(ert-deftest ,test-name ()
                 (cl-flet ,(nreverse funcs)
                   (let ,(nreverse values)
                     ,@body)))))
          cases))))




(provide 'ert-params)

;;; ert-params.el ends here
