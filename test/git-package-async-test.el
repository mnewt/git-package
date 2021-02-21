;;; git-package-async-test.el --- Tests for git-package -*- lexical-binding: t -*-

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

;; Tests for `git-package-async'.


;;; Code:

(require 'ert)
(require 'git-package-async)

(defmacro git-package-test--with-test-project (&rest body)
  "Create a test project, run BODY, delete test project."
  `(let* ((test-project-source-dir (git-package--absolute-path
                                    "git-package" "test" "test-project"))
          (dir (file-name-as-directory (make-temp-file "git-package-test-" t)))
          (git-package-user-directory dir)
          (default-directory dir)
          result)
    (shell-command (concat "cp -R " test-project-source-dir " " dir))
    (condition-case err
        (setq result (progn ,@body))
      (error (message "Keeping test directory:\n %s" dir)
       (signal (car err) (cdr err))))
    (message "dir: %s" dir)
    (dolist (buffer (buffer-list))
     (when-let ((file (buffer-file-name buffer)))
      (when (string-prefix-p dir file)
       (message "Killing buffer: %S" buffer)
       (kill-buffer buffer))))
    (delete-directory dir t)
    result))

(ert-deftest git-package-async-test--echo ()
  "Test `git-package-async-echo'."
  (should (equal "hi" (git-package-async-echo "hi"))))

(ert-deftest git-package-async-test--description ()
  "Test `git-package-async-description'."
  (git-package-test--with-test-project
   (should
    (equal '(:authors (("One Person" . "one@example.com"))
             :version "0.99"
             :homepage "https://example.com/test/test-project"
             :dependencies ((emacs "24.3") (some-package "0.98")))
           (git-package-async-description '(:name test-project :dir "test-project"))))))

(ert-deftest git-package-async-test--byte-compile ()
  "Test `git-package-async-byte-compile'."
  (git-package-test--with-test-project
   (should
    (equal '("test-project.el")
           (git-package-async-byte-compile '(:dir "test-project"))))
   (should
    (equal '("test-project.elc")
           (directory-files "test-project" nil ".*\\.elc")))))

(ert-deftest git-package-async-test--autoloads ()
  "Test `git-package-async-autoloads'."
  (git-package-test--with-test-project
   (should
    (equal* "test-project-autoloads.el"
            (git-package-async-autoloads '(:name test-project :dir "test-project"))
            (car (directory-files "test-project" nil ".*-autoloads\\.el"))))))

(ert-deftest git-package-async-test--makeinfo ()
  "Tets `git-package-async-makeinfo'."
  (git-package-test--with-test-project
   (should
    (equal* '("test-project.info")
            (git-package-async-makeinfo '(:dir "test-project"))
            (directory-files "test-project" nil ".*\\.info")))))

;;; git-package-async-test.el ends here
