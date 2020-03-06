;;; git-package.el --- Install Emacs packages via git -*- lexical-binding: t -*-

;; Copyright © 2020 Matthew Sojourner Newton
;;
;; Author: Matthew Sojourner Newton <matt@mnewton.com>
;; Created: 2020-01-31
;; Version: 0.3
;; Package-Requires: ((emacs "24.3"))
;; Homepage: https://github.com/mnewt/git-package
;; Keywords: config package git

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

;; Git Package is a package manager that uses git and runs asynchronously.

;; This file includes the minimum functionality necessary to load packages that
;; have already been installed.  That way, on startup, we don't do any
;; unnecessary work.

;; For more information, see [[file:README.org]].


;;; Code:


;;; Variables:

(defgroup git-package nil
  "Install Emacs packages using git."
  :group 'package
  :prefix "git-package-")

(defcustom git-package-user-directory (expand-file-name "git" user-emacs-directory)
  "Directory containing the user's git packages."
  :group 'git-package
  :type 'string)

(defcustom git-package-load-autoloads t
  "If non-nil, then load the autoloads file when activating a package.

Autoloads for packages are convenient but if we want to really
optimize startup time we can bypass them.  Once the package is on
`load-path' and/or `custom-theme-load-path', the package can be
required and autoloads can be added manually.  `use-package' can
require the package and/or generate autoloads using keywords such
as :bind, :commands, and :functions."
  :group 'git-package
  :type 'boolean)

(defcustom git-package-default-file-list
  '("*.el" "*.el.in" "dir"
    "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"))
  "List of files to include in the install process.

Stolen from MELPA.

This is part of the MELPA spec.  See:
https://github.com/melpa/melpa#recipe-format."
  :group 'git-package
  :type '(repeat string))

(defcustom git-package-recipe-functions '(git-package-recipe-custom
                                          git-package-recipe-cache
                                          git-package-recipe-melpa
                                          git-package-recipe-elpa
                                          git-package-recipe-emacsmirror)
  "List of functions to call in order to find recipes.

Recipes are Plists that have enough information in them to be
turned into normalized package configs.  Package configs are the
data structure used throughout `git-package'.  See
`git-package--normalize' for details."
  :group 'git-package
  :type '(repeat function))

(defcustom git-package-recipe-alist nil
  "Alist of recipes defined by the user.

It is a list where CAR of each element is a package name (symbol)
and CDR is a Plist.

It looks like this:

\((package-name :url \"https://to/package\"))"
  :group 'git-package
  :type '(repeat list))

(defcustom git-package-fetcher-alist
  '((github . "https://github.com/%s.git")
    (gitlab . "https://gitlab.com/%s.git")
    (bitbucket . "https://bitbucket.org/%s.git")
    (savannah . "https://git.savannah.gnu.org/git/%s.git")
    (orgmode . "https://code.orgmode.org/%s.git")
    (sourcehut . "https://git.sr.ht/~%s"))
  "Alist mapping fetchers to URL format strings."
  :group 'git-package
  :type '(repeat list))

(defvar git-package-alist nil
  "Alist specifying packages managed by `git-package'.

CAR is the package's name as a symbol.

CDR is a Plist that contains information about the package,
including what is needed to fetch the package via git.")

(defvar git-package-recipe-cache-file
  (expand-file-name "var/git-package--recipe-cache.el" user-emacs-directory)
  "File used to cache package repository recipe entries.")

(defvar git-package-recipe-cache nil
  "Cache of previously used recipes.")


;;; Functions:

(defun git-package--absolute-path (&rest paths)
  "Make PATHS into an absolute path starting with `git-package-user-directory'.

The first argument can also be a plist, in which case the :dir
attribute is used as the starting directory.  This is so it can
be passed a package config."
  (when (consp (car paths))
    (setcar paths (plist-get (car paths) :dir)))
  (when (car paths)
    (if (file-name-absolute-p (car paths))
        (car paths)
      (let ((new-path git-package-user-directory))
        (dolist (path paths)
          (setq new-path (concat (file-name-as-directory new-path) path)))
        new-path))))

;; TODO Don't check file system to see if the package is installed.  Do we need
;; to consult the autoload cache?
(defun git-package-installed-p (name-or-config)
  "Return non-nil if the package with NAME-OR-CONFIG is installed.

NAME-OR-CONFIG can be a proper package config or a symbol."
  (when-let ((config (if (consp name-or-config)
                         name-or-config
                       (alist-get name-or-config git-package-alist))))
    (when (file-exists-p (git-package--absolute-path config))
      config)))

(defun git-package-recipe-custom (name)
  "Get a user defined recipe for package NAME.

The recipe should be defined in the custom variable
`git-package-recipe-alist'."
  (alist-get name git-package-recipe-alist))

(defun git-package-recipe-cache (name)
  "Get a cached recipe for package NAME."
  (when (and (null git-package-recipe-cache)
             (file-exists-p git-package-recipe-cache-file))
    (with-temp-buffer
      (insert-file-contents-literally git-package-recipe-cache-file)
      (setq git-package-recipe-cache (read (current-buffer)))))
  (alist-get name git-package-recipe-cache))

(defun git-package-invalidate-cache ()
  "Invalidate the recipe cache."
  (setq git-package-recipe-cache nil)
  (delete-file git-package-recipe-cache-file))

;; Only load `git-package-tasks' if the package isn't already installed.
(autoload 'git-package-recipe-melpa "git-package-tasks")
(autoload 'git-package-recipe-elpa "git-package-tasks")
(autoload 'git-package-recipe-emacsmirror "git-package-tasks")

(defun git-package--find-recipe (name)
  "Find a recipe for package with NAME.

A recipe is a partial package config."
  (let ((fs git-package-recipe-functions)
        recipe)
    (while (and (null recipe) fs)
      (setq recipe (funcall (pop fs) name)))
    (when recipe
      (add-to-list 'git-package-recipe-cache (cons name recipe))
      (with-temp-file git-package-recipe-cache-file
        (prin1 git-package-recipe-cache (current-buffer))))
    recipe))

(defun git-package--resolve-url (config)
  "Resolve the URL for package with CONFIG."
  (or (plist-get config :url)
      (when-let ((repo (plist-get config :repo)))
        (format (alist-get (or (plist-get config :fetcher) 'github)
                           git-package-fetcher-alist)
                repo))))

(defun git-package--prepend-file (file dir)
  "Prepend DIR to FILE.

See `git-package--prepend-file-list'."
  (if (stringp file)
      (concat (file-name-as-directory dir) file)
    (if (eq (car file) :exclude)
        (cons :exclude (git-package--prepend-file-list (cdr file) dir)))))

(defun git-package--prepend-file-list (files dir)
  "Prepend DIR to FILES.

DIR is a string representing a subdirectory in a repository.

FILES is a file list in the format of:
https://github.com/melpa/melpa#recipe-format."
  (if dir
      (mapcar (lambda (file) (git-package--prepend-file file dir)) files)
    files))

(defun git-package--normalize (name &optional recipe)
  "Return a normalized package config using the RECIPE and NAME.

NAME the name of the package.  It is a symbol or nil.

A RECIPE is all the information we need to build a normalized
package config.

RECIPE is:
 - nil
 - symbol
 - string
 - Plist
 - Alist

MELPA recipes are supported as input They look like this:

\(<package-name>
 :fetcher [git|github|gitlab|hg|bitbucket]
 [:url \"<repo url>\"]
 [:repo \"github-gitlab-or-bitbucket-user/repo-name\"]
 [:commit \"commit\"]
 [:branch \"branch\"]
 [:version-regexp \"<regexp>\"]
 [:files (\"<file1>\" ...)])

Reference: https://github.com/melpa/melpa#recipe-format

A normalized package config is all the information we need to
obtain and set up a package.  The full normalized config looks
like:

\(:name <package-name>
 :url \"<repo url>\"
 :dir \"<repo-dir>\"
 ;; optional:
 :ref \"<commit-or-branch>\")
 :files (\"<file1>\" ...)
 :build \"command\")"
  (let* ((error-message
          (format "git-package can't make a valid config from name %s%s%S"
                  name " and recipe " recipe))
         (config
          (cond
           ;; `config' is nil so find the package by `name'.
           ((null recipe)
            (let ((c (git-package--find-recipe name)))
              (if (keywordp (car c)) c (cdr c))))
           ;; `config' is a symbol so override the package `name'.
           ((symbolp recipe)
            (setq name recipe)
            (git-package--find-recipe name))
           ;; `recipe' is a string so make that the `url'.
           ((stringp recipe) (list :url recipe))
           ;; The first element is a string. This is to support passing a
           ;; string to `git-package', since `git-package' wraps all its
           ;; arguments in a list.
           ((stringp (car recipe))
            (apply #'list :url (car recipe) (cdr recipe)))
           ;; It's a Plist so keep it as is.
           ((keywordp (car recipe))
            recipe)
           ;; We assume it's an Alist where car is `name' and cdr is a Plist.
           ((symbolp (car recipe))
            (setq name (car recipe))
            (or (cdr recipe) (git-package--find-recipe name)))))
         (url (or (plist-get config :url)
                  (git-package--resolve-url config)
                  ;; We need a valid URL before proceeding.
                  (user-error error-message)))
         (dir (or (plist-get config :dir)
                  (replace-regexp-in-string "\\.git\\'" ""
                                            (file-name-nondirectory url))))
         (subdir (plist-get config :subdir))
         (files (let ((globs (plist-get config :files)))
                  (git-package--prepend-file-list
                   (if (stringp globs) (list globs) globs)
                   subdir)))
         (ref (or (plist-get config :ref)
                  (plist-get config :branch)
                  (plist-get config :commit)))
         (name (or name (plist-get config :name) (intern dir))))
    ;; Validate and assemble the config plist.
    (if (and name
             (symbolp name)
             (stringp url)
             (stringp dir)
             (or (null subdir) (stringp subdir))
             (or (null files) (listp files))
             (or (null ref) (stringp ref)))
        (let ((new-config (list :name name
                                :url url
                                :dir dir)))
          (when subdir (setq new-config (append new-config (list :subdir subdir))))
          (when files (setq new-config (append new-config (list :files files))))
          (when ref (setq new-config (append new-config (list :ref ref))))
          new-config)
      (user-error error-message))))

;; TODO Revisit the logic.  Do we need to find all files?  We definitely need to
;; find all the .el files.  So is that what we should return?
;; 
;; TODO Redo this because it is messy and perhaps buggy.
(defun git-package--files (config &optional regexp)
  "Return an expanded list of files for package CONFIG.

Wildards are expanded.  Exclusions are removed.  File paths are
relative to :dir.

:files nil means use `git-package-default-file-list' for the file list.

If non-nil, only return files matching REGEXP."
  (let ((default-directory (git-package--absolute-path config))
        files exclude)
    (dolist (file (or (plist-get config :files) git-package-default-file-list))
      (if (and (consp file) (eq (car file) :exclude))
          (setq exclude (mapcan #'file-expand-wildcards (cdr file)))
        (when (or (not regexp) (string-match-p regexp file))
          (setq files (append files (file-expand-wildcards file))))))
    (dolist (file exclude)
      (setq files (delete file files)))
    ;; The main package file is likely to have the shortest file name so we
    ;; make it first in the list.  This was an optimization for
    ;; `git-package-subcommand-description' but it seemed generally useful.
    (sort files (lambda (a b) (< (length a) (length b))))))

(defun git-package--load-paths (config)
  "Return list of load paths for CONFIG."
  (let ((dir (plist-get config :dir))
        paths)
    (dolist (file (git-package--files config))
      (when (and (member (file-name-extension file t) load-suffixes))
        (let ((path (file-name-directory file)))
          (unless (member path paths)
            (push path paths)))))
    (mapcar (lambda (path) (git-package--absolute-path (concat path dir)))
            paths)))

(defun git-package-activate (config)
  "Activate the package described by CONFIG."
  (let* ((name (plist-get config :name))
         (name-string (symbol-name name))
         (dir (expand-file-name (plist-get config :dir) git-package-user-directory)))
    ;; Track the package
    (add-to-list 'git-package-alist (cons name config))
    ;; Add all directories specified by :files to the `load-path'.
    (dolist (path (git-package--load-paths config))
      (add-to-list 'load-path path))
    ;; Add any Info files found in the package root.
    (when (directory-files dir nil "*.info" t)
      (info-initialize)
      (add-to-list 'Info-directory-list dir))
    ;; KLUDGE All themes end in `-theme' or `theme.el', right?  Should we look
    ;; for files called "*-theme.el" instead?
    (when (string-match-p ".*-themes?\\(?:\.el\\)?$" name-string)
      (add-to-list 'custom-theme-load-path dir))
    ;; Load the autoloads file.
    (when git-package-load-autoloads
      (load (expand-file-name (concat name-string "-autoloads.el") dir) t t))))

(autoload 'git-package-do "git-package-tasks")

(defun git-package-ensure (config &optional task)
  "Ensure that a git package described by CONFIG is installed.

Perform TASK, or install if TASK is nil."
  (when config
    (if (git-package-installed-p config)
        (git-package-subtask-activate config)
      (git-package-do 'install config))
    config))


;;; Commands:

;;;###autoload
(defun git-package (&rest recipe)
  "Ensure that the git package described by RECIPE is installed.

Add the package to the `load-path', and, if it's a theme, to
`custom-theme-load-path'.

Load the package's autoloads if `git-package-autoloads' is non-nil.

CONFIG is a string or Plist with at least a :url key."
  (git-package-ensure (git-package--normalize nil recipe)))

(provide 'git-package)


;;; git-package.el ends here
