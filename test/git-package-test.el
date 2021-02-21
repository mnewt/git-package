;;; git-package-test.el --- Tests for git-package -*- lexical-binding: t -*-

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

;; Tests for `git-package'.


;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'git-package)
(require 'git-package-use-package)

(defconst git-package-test--example1
  '(:name foo
    :url "https://github.com/emacsmirror/foo.git"
    :dir "foo"))

(defconst git-package-test--example2
  '(:name company
    :url "https://github.com/company-mode/company-mode.git"
    :dir "company-mode"))

(cl-defun equal* (&rest arguments &aux (x (car arguments)))
  "Like `equal' for each X in an arbitrary number of ARGUMENTS."
  (cl-every (lambda (y) (equal x y)) (cdr arguments)))

(defun plist-equal (&rest plists)
  "Return t if PLISTS' props and vals are all `equal'."
  (when (apply #'= (mapcar #'length plists))
    (let ((p t)
          (p1 (pop plists))
          prop1 val1)
      (while (and p p1)
        (setq prop1 (pop p1)
              val1 (pop p1))
        (setq p (apply #'equal* val1 (mapcar (lambda (plist) (plist-get plist prop1))
                                             plists))))
      p)))

(defmacro expand-minimally (form)
  `(let ((use-package-verbose 'errors)
         (use-package-expand-minimally t))
    (macroexpand-1 ',form)))

(defmacro expand-maximally (form)
  `(let ((use-package-verbose 'debug)
         (use-package-expand-minimally nil))
    (macroexpand-1 ',form)))

(ert-deftest git-package-test--absolute-path ()
  "Test `git-package--absolute-path'."
  (should (equal*
           (expand-file-name (concat user-emacs-directory "git/git-package"))
           (git-package--absolute-path "git-package")
           (git-package--absolute-path '(:dir "git-package"))))
  (should (not (git-package--absolute-path nil))))

(ert-deftest git-package-test--installed-p ()
  "Test `git-package-installed-p'."
  (should (and (git-package-installed-p 'git-package)
               (not (git-package-installed-p 'fake-package)))))

(ert-deftest git-package-test--recipe-custom ()
  "Test `git-package-recipe-custom'."
  (let ((git-package-recipe-alist '((hi :name hi))))
    (should (equal '(:name hi) (git-package-recipe-custom 'hi)))))

(ert-deftest git-package-test--recipe-cache ()
  "Test `git-package-recipe-cache'."
  (let ((git-package-recipe-cache '((hi :name hi))))
    (should (equal '(:name hi) (git-package-recipe-cache 'hi)))))

(ert-deftest git-package-test--find-recipe ()
  "Test `git-package--find-recipe.'"
  (let ((git-package-recipe-alist '((hi :name hi)))
        (git-package-recipe-functions '(git-package-recipe-custom
                                        git-package-recipe-cache))
        (git-package-recipe-cache nil)
        (git-package-recipe-cache-file "/dev/null"))
    (should (equal '(:name hi) (git-package--find-recipe 'hi)))
    (should (not (git-package--find-recipe 'bye)))))

(ert-deftest git-package-test--resolve-url ()
  "Test `git-package--resolve-url'."
  (let ((repo "my/repo")
        (url "https://github.com/my/repo.git"))
    (should (equal* url
                    (git-package--resolve-url `(:url ,url))
                    (git-package--resolve-url `(:repo ,repo))))
    (should (equal "https://gitlab.com/my/repo.git"
                   (git-package--resolve-url `(:repo ,repo :fetcher gitlab))))))

(ert-deftest git-package-test--prepend-file ()
  "Test `git-package--prepend-file'."
  (should (equal "dir/file.txt"
                 (git-package--prepend-file "file.txt" "dir"))))

(ert-deftest git-package-test--prepend-file-list ()
  "Test `git-package--prepend-file-list'."
  (should (equal '("dir/file.txt")
                 (git-package--prepend-file-list '("file.txt") "dir")))
  (should (equal '("dir/file.txt" (:exclude "dir/nofile.txt"))
                 (git-package--prepend-file-list
                  '("file.txt" (:exclude "nofile.txt")) "dir"))))

(ert-deftest git-package-test--normalize-nil ()
  "Create a normalized package config from nil."
  (should-error (git-package--normalize nil nil)))

(ert-deftest git-package-test--normalize-symbol ()
  "Create a normalized package config from a symbol."
  (should-error (git-package--normalize 'nonexistent-package nil))
  (should (equal (git-package--normalize 'company)
                 git-package-test--example2)))

(ert-deftest git-package-test--normalize-string ()
  "Create a normalized package config from a string."
  (should
   (equal
    git-package-test--example1
    (git-package--normalize 'foo "https://github.com/emacsmirror/foo.git"))))

(ert-deftest git-package-test--normalize-list-string ()
  "Create a normalized package config from a list with a string."
  (should
   (equal
    git-package-test--example1
    (git-package--normalize 'foo '("https://github.com/emacsmirror/foo.git")))))

(ert-deftest git-package-test--normalize-plist ()
  "Create a normalized package config from a plist."
  (should
   (equal*
    git-package-test--example1
    (git-package--normalize 'foo '(:url "https://github.com/emacsmirror/foo.git"))
    (git-package--normalize 'bar '(foo :url "https://github.com/emacsmirror/foo.git"))
    (git-package--normalize nil '(foo :url "https://github.com/emacsmirror/foo.git"))
    (git-package--normalize 'foo '(:repo "emacsmirror/foo"))
    (git-package--normalize 'foo '(:fetcher github :repo "emacsmirror/foo")))))

(ert-deftest git-package-test--normalize-plist-files ()
  "Create a normalized package config with :files."
  (should
   (equal*
    (append git-package-test--example1 '(:files ("*.elc")))
    (git-package--normalize 'foo '(:repo "emacsmirror/foo" :files "*.elc"))
    (git-package--normalize 'foo '(:repo "emacsmirror/foo" :files ("*.elc"))))))

(ert-deftest git-package-test--normalize-plist-subdir ()
  "Create a normalized package config with :subdir."
  (should
   (equal*
    (append git-package-test--example1 '(:subdir "bar"))
    (git-package--normalize 'foo '(:repo "emacsmirror/foo" :subdir "bar")))))

(ert-deftest git-package-test--normalize-plist-ref ()
  "Create a normalized package config with :ref."
  (should
   (equal*
    (append git-package-test--example1 '(:ref "bar"))
    (git-package--normalize 'foo '(:repo "emacsmirror/foo" :ref "bar"))
    (git-package--normalize 'foo '(:repo "emacsmirror/foo" :branch "bar"))
    (git-package--normalize 'foo '(:repo "emacsmirror/foo" :commit "bar")))))

;; TODO Don't think this is used anymore?
;; (ert-deftest git-package-test--normalize-list-nil ()
;;   "Don't create a normalized package config from (nil)."
;;   (should (equal (git-package--normalize 'foo '(nil)) nil)))

(ert-deftest git-package-test--use-package-ensure-nil ()
  "Create `use-package' forms without `git-package-use-package-always-ensure'."
  (let ((use-package-always-ensure nil)
        (git-package-use-package-always-ensure nil))
    (git-package-setup-use-package)
    (should (equal (expand-minimally
                    (use-package foo :git "https://github.com/emacsmirror/foo.git"))
                   `(git-package-ensure ',git-package-test--example1)))))

(ert-deftest git-package-test--use-package-ensure-t ()
  "Create `use-package' forms with `git-package-use-package-always-ensure'."
  (let ((use-package-always-ensure nil)
        (git-package-use-package-always-ensure t))
    (git-package-setup-use-package)
    (should (equal (expand-minimally
                    (use-package foo :git "https://github.com/emacsmirror/foo.git"))
                   `(git-package-ensure ',git-package-test--example1)))
    (should (equal (expand-minimally (use-package company))
                   `(git-package-ensure ',git-package-test--example2)))))

(ert-deftest git-package-test--files ()
  "Test `git-package--files'."
  (should
   (equal
    '("README.org")
    (git-package--files '(:dir "git-package" :files ("README.*")))))
  (should
   (equal
    "git-package-async.el"
    (car (git-package--files
          '(:dir "git-package"
            :files ("*.el" (:exclude "git-package.el" "*-test.el"))))))))

(ert-deftest git-package-test--load-paths ()
  "Test `git-package--load-paths'."
  (should
   (equal
    (list (concat (file-name-as-directory (expand-file-name git-package-user-directory
                                                            user-emacs-directory))
                  "git-package"))
    (git-package--load-paths '(:dir "git-package")))))

;; TODO Test Info activation
;; TODO Test theme activation
(ert-deftest git-package-test--activate ()
  "Activate a package with `git-package-activate'."
  (let ((git-package-load-autoloads nil)
        git-package-alist load-path Info-directory-list custom-theme-load-path)
    (git-package-activate '(:name git-package :dir "git-package"))
    (should
     (equal git-package-alist '((git-package :name git-package :dir "git-package"))))
    (should
     (equal load-path
            (list (concat (file-name-as-directory
                           (expand-file-name git-package-user-directory user-emacs-directory))
                          "git-package"))))
    (should (equal Info-directory-list nil))
    (should (equal custom-theme-load-path nil))))


;;; git-package-test.el ends here
