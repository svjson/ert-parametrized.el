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


(defun ert-params--expand-matrix (case-lists)
  "Expand lists of cases in CASE-LISTS and create cartesian product."
  (if (null case-lists)
      nil
    (let ((first-cases (ert-params--expand-cases (car case-lists)))
          (rest-cases (ert-params--expand-matrix (cdr case-lists)))
          result)
      (dolist (fc first-cases)
        (if rest-cases
            (dolist (rc rest-cases)
              (push (append (list (format "%s--%s"
                                          (car fc)
                                          (car rc)))
                            (cdr fc)
                            (cdr rc))
                    result))
          (push fc result)))
      (nreverse result))))



;; deftest macros

(defmacro ert-params--expand-deftest-macro (base-name args cases body)
  "Utility macro for internal use that produces the final `ert-deftest` forms.

BASE-NAME is the base deftest name.
ARGS is the parameter list of each test.
CASES is a list containing the fully expanded test case inputs.
BODY is the test body to be defined for each resulting test, with the symbol
names of ARG bound to the parameters of each constructed test case."
  (let ((prefix (if (symbolp base-name)
                    (symbol-name base-name)
                  base-name)))
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

(defmacro ert-deftest-parametrized (base-name args case-list &rest body)
  "Define a group of parametrized ERT tests.

BASE-NAME is a symbol or string used as the prefix.

ARGS is a list of symbols, one per parameter position in each row.
The first parameter in a row binds to the first symbol in ARGS, etc.

CASE-LIST is a list of test cases containing a case name and parameter
declarations:

  ((\"case-name\"
    (:fun     FORM)
    (:literal FORM)
    (:eval    FORM)
    (:generator ...)
   (...))

BODY is the test body to be defined for each resulting test, with the symbol
names of ARG bound to the parameters of each constructed test case.

The arguments will be bound according to:
  - all :fun parameters bound via cl-flet
  - all :literal/:eval parameters bound via let.
  - all :generator parameters expand to one test case per generator value."
  (declare (indent 3))
  `(ert-params--expand-deftest-macro ,base-name
                                     ,args
                                     ,(ert-params--expand-cases case-list)
                                     ,body))


(defmacro ert-deftest-matrix (base-name args case-lists &rest body)
  "Define parametrized ERT tests from the cartesian product of two case lists.

BASE-NAME is a symbol or string used as the prefix.

ARGS is a list of symbols, one per parameter position in the test cases
constructed from the combination of the parameters expressed in CASE-LISTS.
The first parameter in a row binds to the first symbol in ARGS, etc.

CASE-LISTS is a list containing lists of test cases containing a case name and
parameters:

  (((\"axis-name-%s\"
    (:fun     FORM)
    (:literal FORM)
    (:eval    FORM)
    (:generator ...)))

   ((\"second-axis\"
     (:eval    FORM))))

BODY is the test body to be defined for each resulting test, with the symbol
names of ARG bound to the parameters of each constructed test case.

The arguments will be bound according to:
  - all :fun parameters bound via cl-flet
  - all :literal/:eval parameters bound via let.
  - all :generator parameters expand to one test case per generator value."
  (declare (indent 3))
  `(ert-params--expand-deftest-macro ,base-name
                                     ,args
                                     ,(ert-params--expand-matrix case-lists)
                                     ,body))



(provide 'ert-params)

;;; ert-params.el ends here
