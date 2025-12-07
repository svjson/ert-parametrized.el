;;; ert-parametrized-test-paths.el --- CI entry point -*- lexical-binding: t -*-
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

;; This file places the test directory in the load path, which enables
;; the ci batch runner to properly load the test suite.

;;; Code:

(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))
(add-to-list 'load-path (file-name-directory load-file-name))



(provide 'ert-parametrized-test-paths)

;;; ert-parametrized-test-paths.el ends here
