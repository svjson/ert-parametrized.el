;;; ert-params.utility.test.el --- summary -*- lexical-binding: t -*-

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

;; commentary

;;; Code:



(require 'ert-params)
(require 'ert-params-case-fixtures)



;; ert-params--generator-indices

(ert-deftest-parametrized ert-params--generator-indices
    (case expected-indices)

    (("no-generators"
      (:eval (cdr ert-params--case-A1--no-generators))
      (:literal nil))

     ("one-digit-numbers"
      (:eval (cdr ert-params--case-B--one-digit-num-generator))
      (:literal (0)))

     ("one-digit-numbers--with-literal-siblings"
      (:eval (cdr ert-params--case-C--one-digit-num-generator--with-lit-siblings))
      (:literal (1)))

     ("three-generators--and-literal"
      (:eval (cdr ert-params--case-D--three-generators--with-lit-siblings))
      (:literal (0 2 3))))

  (should (equal (ert-params--generator-indices case)
                 expected-indices)))



;; ert-params--expand-generators

(ert-deftest-parametrized ert-params--expand-generators
    (case expected-expansion)

    (("no-generators"
      (:eval ert-params--case-A1--no-generators)
      (:eval (list ert-params--case-A1--no-generators)))

     ("one-digit-numbers"
      (:eval ert-params--case-B--one-digit-num-generator)
      (:literal (("one-digit-numbers--0" (:literal 0))
                 ("one-digit-numbers--1" (:literal 1))
                 ("one-digit-numbers--2" (:literal 2))
                 ("one-digit-numbers--3" (:literal 3))
                 ("one-digit-numbers--4" (:literal 4))
                 ("one-digit-numbers--5" (:literal 5))
                 ("one-digit-numbers--6" (:literal 6))
                 ("one-digit-numbers--7" (:literal 7))
                 ("one-digit-numbers--8" (:literal 8))
                 ("one-digit-numbers--9" (:literal 9)))))

     ("one-digit-numbers--with-literal-siblings"
      (:eval ert-params--case-C--one-digit-num-generator--with-lit-siblings)
      (:literal (("one-digit-numbers-and-literals--0"
                   (:literal 1)
                   (:literal 0)
                   (:eval 2))
                 ("one-digit-numbers-and-literals--1"
                   (:literal 1)
                   (:literal 1)
                   (:eval 2))
                 ("one-digit-numbers-and-literals--2"
                   (:literal 1)
                   (:literal 2)
                   (:eval 2))
                 ("one-digit-numbers-and-literals--3"
                   (:literal 1)
                   (:literal 3)
                   (:eval 2))
                 ("one-digit-numbers-and-literals--4"
                   (:literal 1)
                   (:literal 4)
                   (:eval 2))
                 ("one-digit-numbers-and-literals--5"
                   (:literal 1)
                   (:literal 5)
                   (:eval 2))
                 ("one-digit-numbers-and-literals--6"
                   (:literal 1)
                   (:literal 6)
                   (:eval 2))
                 ("one-digit-numbers-and-literals--7"
                   (:literal 1)
                   (:literal 7)
                   (:eval 2))
                 ("one-digit-numbers-and-literals--8"
                   (:literal 1)
                   (:literal 8)
                   (:eval 2))
                 ("one-digit-numbers-and-literals--9"
                   (:literal 1)
                   (:literal 9)
                   (:eval 2)))))

     ("three-generators--and-literal"
      (:eval ert-params--case-D--three-generators--with-lit-siblings)
      (:literal (("multiple-generators--input:0--sqr:0--div2:0"
                  (:literal 0)
                  (:literal 1)
                  (:literal 0)
                  (:eval 0))
                 ("multiple-generators--input:1--sqr:1--div2:0.5"
                  (:literal 1)
                  (:literal 1)
                  (:literal 1)
                  (:eval 0.5))
                 ("multiple-generators--input:2--sqr:4--div2:2"
                  (:literal 2)
                  (:literal 1)
                  (:literal 4)
                  (:eval 2))
                 ("multiple-generators--input:3--sqr:9--div2:4.5"
                  (:literal 3)
                  (:literal 1)
                  (:literal 9)
                  (:eval 4.5))
                 ("multiple-generators--input:4--sqr:16--div2:8"
                  (:literal 4)
                  (:literal 1)
                  (:literal 16)
                  (:eval 8))
                 ("multiple-generators--input:5--sqr:25--div2:12.5"
                  (:literal 5)
                  (:literal 1)
                  (:literal 25)
                  (:eval 12.5))
                 ("multiple-generators--input:6--sqr:36--div2:18"
                  (:literal 6)
                  (:literal 1)
                  (:literal 36)
                  (:eval 18))
                 ("multiple-generators--input:7--sqr:49--div2:24.5"
                  (:literal 7)
                  (:literal 1)
                  (:literal 49)
                  (:eval 24.5))
                 ("multiple-generators--input:8--sqr:64--div2:32"
                  (:literal 8)
                  (:literal 1)
                  (:literal 64)
                  (:eval 32))
                 ("multiple-generators--input:9--sqr:81--div2:40.5"
                  (:literal 9)
                  (:literal 1)
                  (:literal 81)
                  (:eval 40.5))))))

  (should (equal (ert-params--expand-generators case)
                 expected-expansion)))



;; expand-matrix

(ert-deftest ert-params--expand-matrix--without-generators ()
  (should (equal (ert-params--expand-matrix
                  '((("one"
                      (:literal 1))
                     ("two"
                      (:literal 2))
                     ("three"
                      (:literal 3)))

                    (("aye"
                      (:literal "A"))
                     ("bee"
                      (:literal "B")))))

                 '(("one--aye"
                    (:literal 1)
                    (:literal "A"))
                   ("one--bee"
                    (:literal 1)
                    (:literal "B"))
                   ("two--aye"
                    (:literal 2)
                    (:literal "A"))
                   ("two--bee"
                    (:literal 2)
                    (:literal "B"))
                   ("three--aye"
                    (:literal 3)
                    (:literal "A"))
                   ("three--bee"
                    (:literal 3)
                    (:literal "B"))))))



;;; ert-params.utility.test.el ends here
