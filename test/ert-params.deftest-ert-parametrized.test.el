;;; ert-params.ert-deftest-parametrized.test.el --- parametrized test usage -*- lexical-binding: t -*-

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

;; This file contains tests that verify the features provided by the
;; `ert-deftest-parametrized` macro of ert-params.el

;;; Code:



(require 'ert-params)
(require 'ert-params-case-fixtures)



;; deftests for example cases

(ert-deftest-parametrized deftest-params--case-A-literal
    (input expected)
    (("another-simple-case"
      (:literal 8)
      (:literal "ABCD"))
     ("another-simple-case"
      (:literal 8)
      (:literal "ABCD")))
  (should (equal (substring "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 0 (/ input 2))
                 expected)))

(ert-deftest-parametrized deftest-params--case-A-refs
    (input expected)
    (ert-params--case-A1--no-generators
     ert-params--case-A2--no-generators)
  (should (equal (substring "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 0 (/ input 2))
                 expected)))

(ert-deftest-parametrized deftest-params--three-generators
    (input always-one expected-sqr expected-sqr-div2)
    (ert-params--case-D--three-generators--with-lit-siblings)
  (should (equal always-one 1))
  (should (equal (* input input)
                 expected-sqr))
  (should (equal (let ((result (/ (float (* input input)) 2)))
                   (if (= result (truncate result))
                       (truncate result)
                     result))
                 expected-sqr-div2)))

(ert-deftest-parametrized example-test

    ;; Input variables bound for each case
    (input expected)

    ;; Named cases and inputs
    (("numbers"
      (:literal 40)
      (:literal 42))
     ("expr"
      (:eval (+ 18 22 40))
      (:literal 82)))

  ;; Test body
  (should (= (+ input 2) expected)))



;;; Code as parameters using :fun

(ert-deftest-parametrized deftest-param--function-parameters
    (do-the-thing expected)
    (("returns-10"
      (:fun (* 2 (+ 1 4)))
      (:eval 10))
     ("a-static-5"
      (:fun 5)
      (:eval 5)))
  (should (equal (do-the-thing) expected)))



;;; ert-params.ert-deftest-parametrized.test.el ends here
