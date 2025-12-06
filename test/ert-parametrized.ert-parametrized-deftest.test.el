;;; ert-parametrized.ert-parametrized-deftest.test.el --- parametrized test usage -*- lexical-binding: t -*-

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
;; `ert-parametrized-deftest` macro of ert-parametrized.el

;;; Code:



(require 'ert-parametrized)
(require 'ert-parametrized-case-fixtures)



;; deftests for example cases

(ert-parametrized-deftest deftest-params--case-A-literal
    (input expected)
    (("another-simple-case"
      (:eval 8)
      (:eval "ABCD"))
     ("another-simple-case"
      (:eval 8)
      (:eval "ABCD")))
  (should (equal (substring "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 0 (/ input 2))
                 expected)))

(ert-parametrized-deftest deftest-params--case-A-refs
    (input expected)
    (ert-parametrized--case-A1--no-generators
     ert-parametrized--case-A2--no-generators)
  (should (equal (substring "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 0 (/ input 2))
                 expected)))

(ert-parametrized-deftest deftest-params--three-generators
    (input always-one expected-sqr expected-sqr-div2)
    (ert-parametrized--case-D--three-generators--with-lit-siblings)
  (should (equal always-one 1))
  (should (equal (* input input)
                 expected-sqr))
  (should (equal (let ((result (/ (float (* input input)) 2)))
                   (if (= result (truncate result))
                       (truncate result)
                     result))
                 expected-sqr-div2)))

(ert-parametrized-deftest example-test

    ;; Input variables bound for each case
    (input expected)

    ;; Named cases and inputs
    (("numbers"
      (:eval 40)
      (:eval 42))
     ("expr"
      (:eval (+ 18 22 40))
      (:eval 82)))

  ;; Test body
  (should (= (+ input 2) expected)))

(ert-parametrized-deftest generator-example
    (input expected)

    (("%d-multiplied-by-2-equals-%d"
       (:generator (:eval (number-sequence 0 10)))
       (:generator (:eval '(0 2 4 6 8 10 12 14 16 18 20)))))

  (should (equal (* input 2)
                 expected)))



;;; Code as parameters using :fun

(ert-parametrized-deftest deftest-param--function-parameters
    (do-the-thing expected)
    (("returns-10"
      (:fun (* 2 (+ 1 4)))
      (:eval 10))
     ("a-static-5"
      (:fun 5)
      (:eval 5)))
  (should (equal (do-the-thing) expected)))



;;; ert-parametrized.ert-parametrized-deftest.test.el ends here
