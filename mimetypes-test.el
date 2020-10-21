;;; mimetypes-test.el --- ERT Unit tests for mimetypes.el

;; Copyright (C) 2020 Craig Niles

;; Author: Craig Niles <niles.c at gmail.com>
;; Maintainer: Craig Niles <niles.c at gmail.com>

;; This file is NOT part of GNU Emacs.

;; mimetypes is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mimetypes is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mimetypes.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit tests for mimetypes.el

;;; Code:

(require 'ert)
(require 'mimetypes)

(ert-deftest test-trim-extension ()
  (should (string= "jpg" (mimetypes--trim-extension ".jpg")))
  (should (string= "jpg" (mimetypes--trim-extension "jpg"))))

(ert-deftest test-ignore-line-p-1 ()
  (should (mimetypes--ignored-line-p " ")))

(ert-deftest test-ignore-line-p-2 ()
  (should (mimetypes--ignored-line-p "\n")))

(ert-deftest test-ignore-line-p-3 ()
  (should (mimetypes--ignored-line-p "# foo \n")))

(ert-deftest test-ignore-line-p-4 ()
  (should-not (mimetypes--ignored-line-p "foo bar")))

(ert-deftest test-line-has-extension-1 ()
  (should (mimetypes--line-has-extension "foo foo" "foo")))

(ert-deftest test-line-has-extension-2 ()
  (should-not (mimetypes--line-has-extension "foo" "foo")))

(ert-deftest test-line-has-extension-3 ()
  (should (mimetypes--line-has-extension "foo bar baz" "baz")))

(ert-deftest test-find-in-buffer ()
  (with-temp-buffer
    (insert "#####\n\nfoo bar\nbaz qux\n\n")
    (should (equal "foo" (mimetypes--find-in-buffer "bar")))
    (should (equal "baz" (mimetypes--find-in-buffer "qux")))
    (should (null (mimetypes--find-in-buffer "boo")))))

(provide 'mimetypes-test)

;;; mimetypes-test.el ends here
