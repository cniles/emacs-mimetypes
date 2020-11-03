;;; mimetypes-test.el --- ERT Unit tests for mimetypes.el -*- lexical-binding: t -*-

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

(ert-deftest test-find-in-buffer ()
  (with-temp-buffer
    (insert "#####\n text/foo a baz b\ntext/vnd.+bar-foo qux\n\n")
    (should (string= "text/foo" (mimetypes--find-in-buffer "baz")))
    (should (string= "text/vnd.+bar-foo" (mimetypes--find-in-buffer "qux")))
    (should (null (mimetypes--find-in-buffer "boo")))))

(ert-deftest test-find-in-file ()
  (should (string= "text/plain"
		   (mimetypes--find-in-file "txt" "one"))))

(ert-deftest test-first-known-file ()
  (should (string= "one" (mimetypes--first-known-file '("zero" "one" "two"))))
  (should-not (mimetypes--first-known-file '("zero" "two"))))

(ert-deftest test-find-in-list ()
  (let ((mime-list '(("plain/foo" "foo" "f")
		     ("plain/bar" "bar" "b"))))
    (should-not (mimetypes--find-in-list "foo" nil))
    (should (string= "plain/foo" (mimetypes--find-in-list "foo" mime-list)))
    (should (string= "plain/bar" (mimetypes--find-in-list "bar" mime-list)))
    (should (string= "plain/foo" (mimetypes--find-in-list "f" mime-list)))
    (should (string= "plain/bar" (mimetypes--find-in-list "b" mime-list)))
    (should-not (mimetypes--find-in-list "baz" mime-list))))

(ert-deftest test-find-in-user-file ()
  (cl-letf (((symbol-function 'mimetypes--user-file-name)
	     (lambda () "one")))
    (should (string= "image/jpeg"
		     (mimetypes-extension-to-mime "jpg")))))

(ert-deftest test-guess-mime ()
  ;; extension target
  (let ((mimetypes-known-files '("one")))
    (should (string= "not/real" (mimetypes-guess-mime "foo"))))

  ;; file name no file proc
  (let ((mimetypes-bypass-file-proc t)
	(mimetypes-known-files '("one")))
    (should (string= "image/jpeg" (mimetypes-guess-mime "foo.jpg")))
    (should (eq nil (mimetypes-guess-mime "mimetypes-test.el"))))

  ;; file name
  (should (string= "text/x-lisp" (mimetypes-guess-mime "mimetypes-test.el")))

  ;; buffer target w/ file
  (let ((mimetypes-known-files '("one")))
    (with-current-buffer (get-buffer-create "test.foo")
      (setq buffer-file-name "test.bar")
      (should (string= "not/real" (mimetypes-guess-mime (current-buffer))))
      (kill-buffer-if-not-modified (current-buffer))))

  ;; buffer target w/o file
  (let ((mimetypes-known-files '("one")))
    (with-current-buffer (get-buffer-create "test.foo")
      (should (string= "not/real" (mimetypes-guess-mime (current-buffer))))
      (kill-buffer-if-not-modified (current-buffer)))))

(provide 'mimetypes-test)
;;; mimetypes-test.el ends here
