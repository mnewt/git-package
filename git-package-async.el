;;; git-package-async.el --- Git Package Sub-Commands -*- lexical-binding: t -*-

;; Copyright © 2020 Matthew Sojourner Newton
;;
;; Author: Matthew Sojourner Newton <matt@mnewton.com>

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
  '("^\\..*" ".*-\\(pkg\\|autoloads\\)\\.el\\'" "^flycheck_.*")
  "Ignore these files during byte compilation.

This is a list of regular expressions.")


;;; Functions:

(declare-function lm-crack-address "lisp-mnt")
(declare-function lm-header-multiline "lisp-mnt")
(declare-function lm-header "lisp-mnt")
(declare-function lm-maintainer "lisp-mnt")
(declare-function lm-homepage "lisp-mnt")

(defun git-package-async-description (config)
  "Read package headers for CONFIG and print them to stdout.

NOTE `package' creates a foo-pkg.el file in the package
directory.  Currently we don't create this file because it seems
unnecessary.  We can just scan the package when we need these
details.  The only thing we really need package headers for is
Package-Requires for installing dependencies.  And the only time
we need that is during package installatoin."
  (require 'lisp-mnt)
  (with-temp-buffer
    (insert-file-contents-literally
     (concat (file-name-as-directory
              (git-package--absolute-path (plist-get config :dir)))
             (symbol-name (plist-get config :name))
             ".el"))
    ;; TODO Send patch for `lm-authors' to add support for Authors: tag (with an
    ;; s).
    (list :authors (mapcar #'lm-crack-address (lm-header-multiline "authors?"))
          ;; :created (lm-header "created")
          ;; :maintainer (lm-maintainer)
          ;; :keywords (split-string (lm-keywords) "[ \f\t\n\r\v,]+")
          :version (or (lm-header "\\(?:package-\\)?version")
                       (substring (shell-command-to-string "git rev-parse HEAD")
                                  0 7))
          :homepage (lm-homepage)
          :dependencies (let* ((header (lm-header-multiline "package-requires"))
                               (string (mapconcat #'identity header " ")))
                          (unless (or (null string) (string= "" string))
                            (car (read-from-string string)))))))

(defun git-package-async-byte-compile (config)
  "Byte compile files for CONFIG and print success to stdout.

FILES is a list of relative paths to .el files.  Wildcards will
be expanded."
  (require 'bytecomp)
  (let ((default-directory (git-package--absolute-path (plist-get config :dir)))
        ignore-list ignore compiled)
    (dolist (file (git-package--files config ".*\\.el$"))
      (setq ignore-list git-package--byte-compile-ignore)
      (while (and (not ignore) ignore-list)
        (setq ignore (string-match-p (pop ignore-list) file)))
      (unless ignore
        (when (byte-compile-file file)
          (push file compiled))))

    compiled))

(defvar generated-autoload-file)
(defvar autoload-timestamps)
(defvar version-control)

(declare-function autoload-rubric "autoload" (file &optional type feature))

(defun git-package-async-autoloads (config)
  "Generate autoloads for CONFIG.

Adapted `package-generate-autoloads'."
  (require 'autoload)
  (let* ((dir (plist-get config :dir))
         (absolute-dir (git-package--absolute-path dir))
         (auto-name (format "%s-autoloads.el" (plist-get config :name)))
         (generated-autoload-file (concat (file-name-as-directory absolute-dir)
                                          auto-name))
         (autoload-timestamps nil)
         (backup-inhibited t)
         (version-control 'never))
    (write-region (autoload-rubric generated-autoload-file "package" nil)
                  nil generated-autoload-file nil 'silent)
    (update-directory-autoloads absolute-dir)
    auto-name))

(defun git-package-async-makeinfo (config)
  "Generate Info files for CONFIG."
  (require 'texinfmt)
  (let ((default-directory (git-package--absolute-path (plist-get config :dir)))
        info-files)
    (dolist (file (git-package--files config ".*\\.texi$"))
      (message "texi file: %s" file)
      (with-temp-buffer
        (insert-file-contents-literally file)
        (with-demoted-errors (texinfo-format-buffer 'nosplit))
        (save-buffer)
        (push (file-name-nondirectory buffer-file-name) info-files)))
    info-files))

(defun git-package-async-echo (input)
  "Immediately return INPUT."
  input)

(defun git-package-async-batch-pipe ()
  "Called from the child Emacs process' command line.

Read a form from stdin, evaluate it, and print the result to
stdout.  If it results in an error, wrap the error before
printing for parsing in the superior process."
  (let ((load-prefer-newer t)
        (coding-system-for-write 'utf-8-auto)
        form input line)
    ;; `read-from-minibuffer' returns when it receives "\n".  It throws an error
    ;; when it recieves EOF so `line' is set to nil, stopping the loop.
    (while (setq line (ignore-errors (read-from-minibuffer "")))
      (setq input (concat input line)))
    (setq form (read input))
    (condition-case err
        ;; Wrap the result in delimiters so `git-package--process-filter' can
        ;; pick it out of the log output.
        (princ (concat "\0\0" (prin1-to-string (eval form)) "\0\1"))
      (error
       (message "error in async process: %S" err)
       (kill-emacs 22)))))


(provide 'git-package-async)

;;; git-package-async.el ends here
