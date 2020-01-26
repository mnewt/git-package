;;; git-package-async.el --- Git Package Sub-Commands -*- lexical-binding: t -*-

;; Author: Matthew Sojourner Newton
;; Maintainer: Matthew Sojourner Newton
;; Package: git-package

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Functions which are meant to be called asynchronously using
;; `git-package--async-funcall'.


;;; Code:

(require 'git-package)


;;; Variables:

(defvar git-package--byte-compile-ignore
  '("^\\..*" ".*-\\(pkg\\|autoloads\\)\\.el\\'")
  "Ignore these files during byte compilation.

This is a list of regular expressions.")


;;; Functions:

(defun git-package-async-description--read (header)
  "Read HEADER as it comes from `lm-header-*'."
  (let ((string (if (consp header)
                    (mapconcat #'identity header " ")
                  header)))
    (unless (or (null string) (string= "" string))
      (car (read-from-string string)))))

(autoload 'lm-header-multiline "lisp-mnt")
(autoload 'lm-authors "lisp-mnt")
(autoload 'lm-maintainer "lisp-mnt")
(autoload 'lm-header "lisp-mnt")
;; (autoload 'lm-homepage "lisp-mnt")

(defun git-package-async-description (config)
  "Read package headers for CONFIG and print them to stdout.

NOTE `package' creates a foo-pkg.el file in the package
directory.  Currently we don't create this file because it seems
unnecessary.  We can just scan the package when we need these
details.  The only thing we really need package headers for is
Package-Requires for installing dependencies."
  (let* ((default-directory (git-package--absolute-path (plist-get config :dir)))
         (file-list (git-package--expand-file-list config))
         description)
    (require 'lisp-mnt)
    (while (and (not description) file-list)
      (with-temp-buffer
        (insert-file-contents-literally (pop file-list))
        (when-let ((dependencies
                    (let* ((header (lm-header-multiline "package-requires"))
                           (string (mapconcat #'identity header " ")))
                      (unless (or (null string) (string= "" string))
                        (car (read-from-string string))))))
          (setq description
                (list :authors (lm-authors)
                      :maintainer (lm-maintainer)
                      :version (or (lm-header "package-version")
                                   (lm-header "version"))
                      :homepage (lm-homepage)
                      :dependencies dependencies)))))
    description))

(defun git-package-async-compile (config)
  "Byte compile files for CONFIG and print success to stdout.

FILES is a list of relative paths to .el files.  Wildcards will
be expanded."
  (let ((default-directory (git-package--absolute-path (plist-get config :dir)))
        ignore-list ignore compiled)
    (mapc (lambda (file)
            (setq ignore-list (purecopy git-package--byte-compile-ignore))
            (while (and (not ignore) ignore-list)
              (setq ignore (string-match-p (pop ignore-list) file)))
            (unless ignore
              (with-demoted-errors (byte-compile-file file t))
              (push file compiled)))
          (git-package--expand-file-list config))
    compiled))

(defun git-package-async-batch-pipe ()
  "Called from the child Emacs process' command line.

Read a form from stdin, evaluate it, and print the result to
stdout.  If it results in an error, wrap the error before
printing for parsing in the superior process."
  (let ((form (read (read-from-minibuffer ""))))
    (prin1 form)
    (condition-case err
        (prin1 (eval form))
      (error
       (message "Evaluating form:\n%S\nThrew error: %S" form err)
       (kill-emacs 22)))))

(provide 'git-package-subcommands)

;;; git-package-async.el ends here
