;;; git-package-test.el --- Tests for git-package -*- lexical-binding: t -*-

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

;; Tests for `git-package'.

;;; Code:

(require 'ert)
(require 'git-package)
(require 'git-package-use-package)

(defconst git-package-test--example1
  '(:name foo
    :url "https://github.com/emacsmirror/foo"
    :dir "foo"))

(defconst git-package-test--example2
  '(:name company
    :url "https://github.com/company-mode/company-mode"
    :dir "company-mode"))

(cl-defun equal* (&rest arguments &aux (x (car arguments)))
  "Like `equal' for each X in an arbitrary number of ARGUMENTS."
  (cl-every (lambda (y) (equal x y)) (cdr arguments)))

(defmacro expand-minimally (form)
  `(let ((use-package-verbose 'errors)
         (use-package-expand-minimally t))
    (macroexpand-1 ',form)))

(defmacro expand-maximally (form)
  `(let ((use-package-verbose 'debug)
         (use-package-expand-minimally nil))
    (macroexpand-1 ',form)))

(ert-deftest git-package-test--normalize-nil ()
  "Create a normalized package config from a `name' only."
  (should-error (git-package--normalize 'foo nil))
  (should (equal (git-package--normalize 'company nil)
                 git-package-test--example2)))

(ert-deftest git-package-test--normalize-symbol ()
  "Create a normalized package config from a symbol."
  (should (equal (git-package--normalize 'company)
                 git-package-test--example2)))

(ert-deftest git-package-test--normalize-string ()
  "Create a normalized package config from a string."
  (should (equal (git-package--normalize 'foo "https://github.com/emacsmirror/foo")
                 git-package-test--example1)))

(ert-deftest git-package-test--normalize-list-string ()
  "Create a normalized package config from a list with a string."
  (should (equal (git-package--normalize 'foo '("https://github.com/emacsmirror/foo"))
                 git-package-test--example1)))

(ert-deftest git-package-test--normalize-plist ()
  "Create a normalized package config from a plist."
  (should
   (equal*
    git-package-test--example1
    (git-package--normalize 'foo '(:url "https://github.com/emacsmirror/foo"))
    (git-package--normalize 'bar '(foo :url "https://github.com/emacsmirror/foo"))
    (git-package--normalize nil '(foo :url "https://github.com/emacsmirror/foo"))
    (git-package--normalize 'foo '(:fetcher github :repo "emacsmirror/foo")))))

(ert-deftest git-package-test--normalize-list-nil ()
  "Don't create a normalized package config from (nil)."
  (should (equal (git-package--normalize 'foo '(nil)) nil)))

(ert-deftest git-package-test--use-package-1 ()
  "Create `use-package' forms."
  (let ((use-package-always-ensure nil)
        (git-package-use-package-always-ensure nil))
    (git-package-setup-use-package)
    (should (equal (expand-minimally
                    (use-package foo :git "https://github.com/emacsmirror/foo"))
                   `(git-package-ensure ',git-package-test--example1)))))

(ert-deftest git-package-test--use-package-2 ()
  "Create `use-package' forms."
  (let ((use-package-always-ensure nil)
        (git-package-use-package-always-ensure t))
    (git-package-setup-use-package)
    (should (equal (expand-minimally
                    (use-package foo :git "https://github.com/emacsmirror/foo"))
                   `(git-package-ensure ',git-package-test--example1)))
    (should (equal (expand-minimally (use-package company))
                   `(git-package-ensure ',git-package-test--example2)))))

;;; git-package-test.el ends here
