;;; mimetypes.el --- Guess a file's mimetype by extension -*- lexical-binding: t -*-

;; Copyright (C) 2020 Craig Niles

;; Author: Craig Niles <niles.c at gmail.com>
;; Maintainer: Craig Niles <niles.c at gmail.com>
;; URL: https://github.com/cniles/emacs-mimetypes
;; Package-Requires: ((emacs "25.1"))
;; Version: 1.0

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

;; mimetypes provides a library for guessing MIME type by file
;; extension based on the platform.  On Windows it will search the
;; registry.  Otherwise, it will search for one of the known locations
;; that a mime.types file may exist and utilize it.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar mimetypes-bypass-file-proc nil
  "If non-nil, bypasses  using `file' command to guess MIME type.")

(defvar mimetypes-known-files
  '("/etc/mime.types"
    "/etc/httpd/mime.types"
    "/etc/httpd/conf/mime.types"
    "/etc/apache/mime.types"
    "/etc/apache2/mime.types"
    "/usr/local/etc/httpd/conf/mime.types"
    "/usr/local/lib/netscape/mime.types"
    "/usr/local/etc/httpd/conf/mime.types"
    "/usr/local/etc/mime.types")
  "List of known mime.types file locations.")

(defconst mimetypes--file-re-format
  "^\\s *\\(\\([Xx]-\\)?\\w+/\\([Xx]-\\)?[[:alnum:]+-.]+\\)\\(\\s +\\w+\\)*\\(\\s +\\(%s\\)\\)\\(\\s +\\w+\\)*\\s *$"
  "Regular expression format for searching through mime.types file.")

(defun mimetypes--first-known-file (files)
  "Return the first file from a list of file names FILES that exists."
  (when files
    (let ((f-name (car files)))
      (if (file-exists-p f-name) f-name
	(mimetypes--first-known-file (cdr files))))))

(defsubst mimetypes--trim-extension (extension)
  "Trim period and whitespace from EXTENSION."
  (replace-regexp-in-string "^[. \t\n\r]+" "" extension))

(defun mimetypes--read-registry (key-name value-name)
  "Read KEY-NAME and VALUE-NAME from the Windows registry."
  (with-temp-buffer
    (let ((result (call-process "reg.exe" nil t nil "query" key-name "/v" value-name)))
      (when (zerop result) (split-string (buffer-string))))))

(defun mimetypes--find-in-registry (extension)
  "Search the registry for a MIME type for EXTENSION."
  (car (last (mimetypes--read-registry
	      (format "HKEY_LOCAL_MACHINE\\Software\\Classes\\.%s" (mimetypes--trim-extension extension))
	      "Content Type"))))

(defun mimetypes--find-in-buffer (ext)
  "Find MIME type for EXT in current buffer, which should be a mime.types file."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format mimetypes--file-re-format (regexp-quote ext)) nil t) (match-string 1))))

(defun mimetypes--find-in-file (extension file-name)
  "Check for EXTENSION in mime.types file FILE-NAME."
  (when (file-exists-p file-name)
    (with-temp-buffer
      (insert-file-contents-literally file-name)
      (mimetypes--find-in-buffer (downcase (mimetypes--trim-extension extension))))))

(defun mimetypes--find-in-list (extension mime-list)
  "Find EXTENSION in list MIME-LIST.
Each element of MIME-LIST must be a list of strings of the form:
\(mimetype ext1 ext2 ... extn)."
  (let ((extension (mimetypes--trim-extension extension))
	(type-list (car mime-list)))
    (if (null mime-list) nil
      (if (seq-contains (cdr type-list) extension #'string=) (car type-list)
	(mimetypes--find-in-list extension (cdr mime-list))))))

(defun mimetypes--user-file-name ()
  "Get the name of the mimetypes user file."
  (cond ((eq system-type 'windows-nt) (expand-file-name ".mime.types" (getenv "USERPROFILE")))
	((eq system-type 'ms-dos) nil)
	(t (expand-file-name ".mime.types" (getenv "HOME")))))

(defun mimetypes--from-file-proc (target)
  "Use the `file' command to determine MIME type for TARGET.

Target can be a buffer or file name.  If a buffer, then its
contents are provided to `file' through standard input.
Otherwise, the name of the file is provided.  It is an error to
provide a file name that does not exist."
  (when (and (executable-find "file")
	     (not (seq-contains '(ms-dos windows-nt cygwin) system-type)))
    (let* ((output-buffer (generate-new-buffer "*temp*"))
	   (file-name (if (bufferp target) (or (buffer-file-name target) (buffer-name target)) target))
	   (file-target (and (stringp file-name) (file-exists-p file-name)))
	   (proc-fn (if file-target #'call-process #'call-process-region)))
      (let ((args (append
		   (unless file-target (list nil nil))
		   (list "file" nil output-buffer nil "-b" "--mime-type")
		   (list (if file-target file-name "-")))))
	(with-current-buffer (if (bufferp target) target (current-buffer))
	  (apply proc-fn args)))
      (let ((result (string-trim (with-current-buffer output-buffer (buffer-string)))))
	(unless (or (string= "inode/x-empty" result)
		    (string= "application/x-empty" result))
	  result)))))

(defun mimetypes-extension-to-mime (extension &optional extra-types)
  "Guess a mimetype from EXTENSION.
If EXTRA-TYPES is provided, that list takes precedent over
system-provided mimetype mappings or user file mappings."
  (unless (stringp extension) (signal 'wrong-type-argument '(stringp extension)))
  (let ((mime-type (or (mimetypes--find-in-list extension extra-types)
		       (mimetypes--find-in-file extension (mimetypes--user-file-name)))))
    (cond (mime-type mime-type)
	  ((eq system-type 'windows-nt) (mimetypes--find-in-registry extension))
	  ((eq system-type 'ms-dos) nil)
	  ((eq system-type 'cygwin) nil)
	  (t (let ((mime-types-file (mimetypes--first-known-file mimetypes-known-files)))
	       (when mime-types-file (mimetypes--find-in-file extension mime-types-file)))))))

(defun mimetypes-guess-mime (target &optional extra-types)
  "Guess a MIME type TARGET, which can be a buffer or string.

For buffers, an attempt will be made to get the MIME type with
the `file' command if it is available.  If the buffer has a
visited file the file name will specified as an argument to the
`file' command, otherwise the buffer's contents will be passed
through standard input.  If this check returns 'nil' or
'text/plain' a check is made for the MIME type by extension.

If TARGET is a string it is assumed to either be a file name or
an extension.  If a file by the name TARGET exists, a check is
made with the `file' command.  If this returns nil or
'text/plain' then check is made with the extension derived from
TARGET or TARGET itself.

If EXTRA-TYPES is provided, it takes priority over other guessing
mechanism.  If the extension derived from the buffer or string is
found in EXTRA-TYPES it will be returned."
  (cl-assert (or (bufferp target) (stringp target))
	     t "target must be a buffer or file that exists")
  (let* ((file-name (if (bufferp target) (or (buffer-file-name target) (buffer-name target)) target))
	 (extension (or (file-name-extension file-name) file-name)))
    (or (mimetypes--find-in-list extension extra-types)
	(let ((mime-from-file (when (and (null mimetypes-bypass-file-proc)
					 (or (file-exists-p file-name) (bufferp target)))
				(mimetypes--from-file-proc target))))
	  (if (or (not mime-from-file) (string= "text/plain" mime-from-file))
	      (or (mimetypes-extension-to-mime extension)
		  mime-from-file)
	    mime-from-file)))))

(provide 'mimetypes)
;;; mimetypes.el ends here
