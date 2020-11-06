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

(defmacro with-temp-mimetypes-file (&rest body)
  "Run BODY with access to a temporary mime.types file."
  `(let ((temp-mime-types (make-temp-file "mime.types")))
     (with-temp-file temp-mime-types
       (insert "################\n")
       (insert "\n")
       (insert "image/jpeg		jpg jpeg\n")
       (insert "text/plain	 	txt md\n")
       (insert "not/real		foo bar\n"))
     (unwind-protect (progn ,@body)
       (delete-file temp-mime-types))))

(ert-deftest test-find-in-file ()
  (with-temp-mimetypes-file
   (should (string= "text/plain"
		    (mimetypes--find-in-file "txt" temp-mime-types)))))

(ert-deftest test-first-known-file ()
  (with-temp-mimetypes-file
   (should (string= temp-mime-types (mimetypes--first-known-file `("zero" ,temp-mime-types "two"))))
   (should-not (mimetypes--first-known-file '("zero" "two")))))

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
  (with-temp-mimetypes-file
   (cl-letf (((symbol-function 'mimetypes--user-file-name)
	      (lambda () temp-mime-types)))
     (should (string= "image/jpeg"
		      (mimetypes-extension-to-mime "jpg"))))))

(ert-deftest test-guess-mime ()
  (cl-letf (((symbol-function 'mimetypes-extension-to-mime)
	     (lambda (ext) (when (stringp ext) (format "ext/%s" ext))))
	    ((symbol-function 'mimetypes--from-file-proc)
	     (lambda (fname)
	       (unless (stringp fname) (setq fname (or buffer-file-name (buffer-name))))
	       (format "fileproc/%s" (or (file-name-extension fname) fname)))))

    (with-current-buffer (get-buffer-create "test.ext")

      (let ((system-type 'windows-nt))
	(should (string= "ext/ext" (mimetypes-guess-mime (current-buffer))))
	(let ((buffer-file-name "temptest.ext2"))
	  (should (string= "ext/ext2" (mimetypes-guess-mime (current-buffer)))))
	(should (string= "ext/custom" (mimetypes-guess-mime "ext" '(("ext/custom" "ext")))))
	(should (string= "ext/ext" (mimetypes-guess-mime "ext")))
	(should (string= "ext/ext" (mimetypes-guess-mime "file.ext"))))

      (let ((system-type 'nix))
	(should (string= "fileproc/ext" (mimetypes-guess-mime (current-buffer))))
	(let ((buffer-file-name "tempteest.ext2"))
	  (should (string= "fileproc/ext2" (mimetypes-guess-mime (current-buffer)))))
	(should (string= "ext/custom" (mimetypes-guess-mime "ext" '(("ext/custom" "ext")))))
	(should (string= "ext/ext" (mimetypes-guess-mime "ext")))
	(should (string= "ext/ext" (mimetypes-guess-mime "file.ext")))))))

(provide 'mimetypes-test)
;;; mimetypes-test.el ends here
