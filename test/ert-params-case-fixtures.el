;;; ert-params-case-fixtures.el --- common test input -*- lexical-binding: t -*-

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

;; This file contains common test case inputs for ert-params tests.

;;; Code:



(defconst ert-params--case-A1--no-generators
  '("a-simple-case"
    (:eval 10)
    (:eval "ABCDE"))
  "A simple test case with two literal inputs.")

(defconst ert-params--case-A2--no-generators
  '("another-simple-case"
    (:eval 8)
    (:eval "ABCD"))
  "A sibling case to A1.")

(defconst ert-params--case-B--one-digit-num-generator
  '("one-digit-numbers--%s"
    (:generator (:eval (number-sequence 0 9))))
  "A test-case containing only a generator literal.")

(defconst ert-params--case-C--one-digit-num-generator--with-lit-siblings
  '("one-digit-numbers-and-literals--%s"
    (:eval 1)
    (:generator (:eval (number-sequence 0 9)))
    (:eval 2))
  "A test-case containing a combination of a single generator and static literals.")

(defconst ert-params--case-D--three-generators--with-lit-siblings
  '("multiple-generators--input:%s--sqr:%s--div2:%s"
    (:generator (:eval (number-sequence 0 9)))
    (:eval 1)
    (:generator (:eval '(0 1 4 9 16 25 36 49 64 81)))
    (:generator (:eval '(0 0.5 2 4.5 8 12.5 18 24.5 32 40.5))))
  "A test-case containing multiple generators and a single literal.")


(provide 'ert-params-case-fixtures)

;;; ert-params-case-fixtures.el ends here
