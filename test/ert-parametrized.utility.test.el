;;; ert-parametrized.utility.test.el --- summary -*- lexical-binding: t -*-
;; -*- no-byte-compile: t -*-

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

;; This file contains tests for the internal utility functions used
;; during macro expansion.

;;; Code:



(load-file (expand-file-name "ert-parametrized-test-paths.el"
                             (file-name-directory (or load-file-name buffer-file-name))))

(require 'ert-parametrized)
(require 'ert-parametrized-case-fixtures)



;; ert-parametrized--generator-indices

(ert-parametrized-deftest ert-parametrized--generator-indices
    (case expected-indices)

    (("no-generators"
      (:eval (cdr ert-parametrized--case-A1--no-generators))
      (:eval nil))

     ("one-digit-numbers"
      (:eval (cdr ert-parametrized--case-B--one-digit-num-generator))
      (:quote (0)))

     ("one-digit-numbers--with-literal-siblings"
      (:eval (cdr ert-parametrized--case-C--one-digit-num-generator--with-lit-siblings))
      (:quote (1)))

     ("three-generators--and-literal"
      (:eval (cdr ert-parametrized--case-D--three-generators--with-lit-siblings))
      (:quote (0 2 3))))

  (should (equal (ert-parametrized--generator-indices case)
                 expected-indices)))



;; ert-parametrized--expand-generators

(ert-parametrized-deftest ert-parametrized--expand-generators
    (case expected-expansion)

    (("no-generators"
      (:eval ert-parametrized--case-A1--no-generators)
      (:eval (list ert-parametrized--case-A1--no-generators)))

     ("one-digit-numbers"
      (:eval ert-parametrized--case-B--one-digit-num-generator)
      (:quote (("one-digit-numbers--0" (:eval 0))
               ("one-digit-numbers--1" (:eval 1))
               ("one-digit-numbers--2" (:eval 2))
               ("one-digit-numbers--3" (:eval 3))
               ("one-digit-numbers--4" (:eval 4))
               ("one-digit-numbers--5" (:eval 5))
               ("one-digit-numbers--6" (:eval 6))
               ("one-digit-numbers--7" (:eval 7))
               ("one-digit-numbers--8" (:eval 8))
               ("one-digit-numbers--9" (:eval 9)))))

     ("one-digit-numbers--with-literal-siblings"
      (:eval ert-parametrized--case-C--one-digit-num-generator--with-lit-siblings)
      (:quote (("one-digit-numbers-and-literals--0"
                (:eval 1)
                (:eval 0)
                (:eval 2))
               ("one-digit-numbers-and-literals--1"
                (:eval 1)
                (:eval 1)
                (:eval 2))
               ("one-digit-numbers-and-literals--2"
                (:eval 1)
                (:eval 2)
                (:eval 2))
               ("one-digit-numbers-and-literals--3"
                (:eval 1)
                (:eval 3)
                (:eval 2))
               ("one-digit-numbers-and-literals--4"
                (:eval 1)
                (:eval 4)
                (:eval 2))
               ("one-digit-numbers-and-literals--5"
                (:eval 1)
                (:eval 5)
                (:eval 2))
               ("one-digit-numbers-and-literals--6"
                (:eval 1)
                (:eval 6)
                (:eval 2))
               ("one-digit-numbers-and-literals--7"
                (:eval 1)
                (:eval 7)
                (:eval 2))
               ("one-digit-numbers-and-literals--8"
                (:eval 1)
                (:eval 8)
                (:eval 2))
               ("one-digit-numbers-and-literals--9"
                (:eval 1)
                (:eval 9)
                (:eval 2)))))

     ("three-generators--and-literal"
      (:eval ert-parametrized--case-D--three-generators--with-lit-siblings)
      (:quote (("multiple-generators--input:0--sqr:0--div2:0"
                (:eval 0)
                (:eval 1)
                (:eval 0)
                (:eval 0))
               ("multiple-generators--input:1--sqr:1--div2:0.5"
                (:eval 1)
                (:eval 1)
                (:eval 1)
                (:eval 0.5))
               ("multiple-generators--input:2--sqr:4--div2:2"
                (:eval 2)
                (:eval 1)
                (:eval 4)
                (:eval 2))
               ("multiple-generators--input:3--sqr:9--div2:4.5"
                (:eval 3)
                (:eval 1)
                (:eval 9)
                (:eval 4.5))
               ("multiple-generators--input:4--sqr:16--div2:8"
                (:eval 4)
                (:eval 1)
                (:eval 16)
                (:eval 8))
               ("multiple-generators--input:5--sqr:25--div2:12.5"
                (:eval 5)
                (:eval 1)
                (:eval 25)
                (:eval 12.5))
               ("multiple-generators--input:6--sqr:36--div2:18"
                (:eval 6)
                (:eval 1)
                (:eval 36)
                (:eval 18))
               ("multiple-generators--input:7--sqr:49--div2:24.5"
                (:eval 7)
                (:eval 1)
                (:eval 49)
                (:eval 24.5))
               ("multiple-generators--input:8--sqr:64--div2:32"
                (:eval 8)
                (:eval 1)
                (:eval 64)
                (:eval 32))
               ("multiple-generators--input:9--sqr:81--div2:40.5"
                (:eval 9)
                (:eval 1)
                (:eval 81)
                (:eval 40.5))))))

  (should (equal (ert-parametrized--expand-generators case)
                 expected-expansion)))



;; expand-matrix

(ert-deftest ert-parametrized--expand-matrix--without-generators ()
  (should (equal (ert-parametrized--expand-matrix
                  '((("one"
                      (:eval 1))
                     ("two"
                      (:eval 2))
                     ("three"
                      (:eval 3)))

                    (("aye"
                      (:eval "A"))
                     ("bee"
                      (:eval "B")))))

                 '(("one--aye"
                    (:eval 1)
                    (:eval "A"))
                   ("one--bee"
                    (:eval 1)
                    (:eval "B"))
                   ("two--aye"
                    (:eval 2)
                    (:eval "A"))
                   ("two--bee"
                    (:eval 2)
                    (:eval "B"))
                   ("three--aye"
                    (:eval 3)
                    (:eval "A"))
                   ("three--bee"
                    (:eval 3)
                    (:eval "B"))))))



;;; ert-parametrized.utility.test.el ends here
