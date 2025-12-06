;;; ert-parametrized.ert-parametrized-deftest-matrix.test.el --- parametrized test usage -*- lexical-binding: t -*-

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
;; `ert-deftest-parametrized` macro of ert-parametrized.el

;;; Code:



(require 'cl-lib)
(require 'ert-parametrized)
(require 'ert-parametrized-case-fixtures)




(ert-parametrized-deftest-matrix test-matrix--produces-even-numbers
    (test-number multiplier)
    ((("num-1"
       (:eval 1))
      ("num-2"
       (:eval 2))
      ("num-3"
       (:eval 3)))

     (("multiplied-by-2"
       (:eval 2))
      ("multiplied-by-4"
       (:eval 4))
      ("multiplied-by-6"
       (:eval 6))))

  (should (cl-evenp (* test-number multiplier))))


(ert-parametrized-deftest-matrix test-matrix-with-generators--produces-even-numbers
    (test-number multiplier)
    ((("num-%s"
       (:generator (:eval (number-sequence 1 5)))))

     (("multiplied-by-%s"
       (:generator (:eval (number-sequence 2 10 2))))))

  (should (cl-evenp (* test-number multiplier))))



;;; ert-parametrized.ert-parametrized-deftest-matrix.test.el ends here
