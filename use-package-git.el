;;; use-package-git.el --- Git extension for use-package -*- lexical-binding: t -*-

;; Author: Matthew Sojourner Newton
;; Maintainer: Matthew Sojourner Newton
;; Version: 0.1
;; Package-Requires: ((emacs "24.3") (use-package "2.0"))
;; Requires: ((git "1.7.2.3"))
;; Homepage: https://github.com/mnewt/dotemacs
;; Keywords: dotemacs config package git


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

;; This package adds support for installing packages directly from git
;; repositories.

;; When the :git keyword is specified and :ensure is non-nil, the given package
;; will be cloned, byte compiled, and added to `load-path'.

;; For more information, see README.org.

;;; Code:

(require 'seq)
(require 'cl-seq)
(require 'package)
(require 'use-package-ensure)


;;; Variables

(defcustom use-package-git-user-dir (expand-file-name "git" user-emacs-directory)
  "Directory containing the user's git packages."
  :group 'use-package-ensure
  :type 'string)

(defcustom use-package-git-load-autoloads nil
  "If non-nil, then load the autoloads file when activating a package.

This is not always necessary. Once the package is on `load-path'
and/or `custom-theme-load-path', `use-package' can require the
package and/or generate autoloads using keywords such as
`:commands'."
  :group 'use-package-ensure
  :type 'boolean)

(defvar use-package-git--packages nil
  "Alist specifying packages ensured by `use-package-git'.

CAR is the package's local name as a symbol.

CDR is a Plist that contains the information needed to fetch the
package via git.")

(defvar use-package-git--read-package-history nil
  "History for `use-package-git-upgrade' command.")

(defvar use-package-git--previous-ensure-function nil
  "The previous value of `use-package-ensure-function'.")

(defvar use-package-git--byte-compile-ignore '("^\\..*" ".*-\\(pkg\\|autoloads\\)\\.el\\'")
  "Ignore these files during byte compilation.

This is a list of regular expressions.")

(defvar use-package-git--buffer "*use-package-git*"
  "Activity log for `use-package-git'.")


;;; Functions

(defun use-package-git--shell-command (command)
  "Run COMMAND and format the output in `use-package-git-buffer'."
  (with-current-buffer use-package-git--buffer
    (insert " > " command)
    (let ((shell-command-dont-erase-buffer t))
      (shell-command command use-package-git--buffer))
    (insert "\n" (make-string 80 ?-) "\n")))

(defun use-package-git--absolute-dir (dir)
  "Ensure DIR is an absolute path."
  (if (file-name-absolute-p dir)
      dir
    (expand-file-name dir use-package-git-user-dir)))

(defun use-package-git--dirty-p (&optional dir)
  "Return non-nil if the git repo DIR is dirty."
  (not
   (= 0 (length (shell-command-to-string
                 (format "git -C '%s' status --porcelain"
                         (expand-file-name (or dir default-directory))))))))

(defun use-package-git--package-names ()
  "List the package names activated by `use-package-git'."
  (mapcar (lambda (p) (symbol-name (car p))) use-package-git--packages))

(defun use-package-git--byte-compile (dir files)
  "Byte compile FILES in DIR.

FILES is a list of relative paths to .el files. Wildcards will be
expanded."
  (let ((default-directory dir))
    (dolist (file (seq-mapcat #'file-expand-wildcards files))
      (unless (seq-some (lambda (re) (string-match-p re file))
                        use-package-git--byte-compile-ignore)
        (save-window-excursion
          (with-demoted-errors (byte-compile-file file t)))))))

(defun use-package-git--add-info-nodes (dir)
  "If they exist, add Info nodes from DIR.

This is the way `package.el' does it."
  (when (file-exists-p (expand-file-name "dir" dir))
    (require 'info)
    (info-initialize)
    (push dir Info-directory-list)))

(defun use-package-git--install (config)
  "Install the package for CONFIG."
  (let* ((name (plist-get config :name))
         (default-directory (use-package-git--absolute-dir (plist-get config :dir)))
         (wc (current-window-configuration))
         (pkg-desc (progn
                     (dired default-directory)
                     (with-demoted-errors (package-dir-info)))))
    (when (package-desc-p pkg-desc)
      ;; Download and install the dependencies using `package.el' if the above
      ;; step successfully created a `package-desc'.
      (let* ((requires (package-desc-reqs pkg-desc))
             (transaction (package-compute-transaction nil requires)))
        (package-download-transaction transaction)))
    (when-let ((command (plist-get config :command)))
      (compile command))
    (package-generate-autoloads name default-directory)
    (use-package-git--byte-compile default-directory (plist-get config :files))
    (use-package-git--add-info-nodes default-directory)
    (set-window-configuration wc)))

(defun use-package-git--activate (config)
  "Activate the package for CONFIG."
  (let* ((name (plist-get config :name))
         (name-string (symbol-name name))
         (dir (expand-file-name (plist-get config :dir) use-package-git-user-dir)))
    ;; Track the package
    (add-to-list 'use-package-git--packages (cons name config))
    ;; Add all directories specified by :files to the `load-path'.
    (dolist (file (plist-get config :files))
      (add-to-list 'load-path (file-name-directory (expand-file-name file dir))))
    ;; Add to `custom-theme-load-path' if we have a theme. All themes end in
    ;; `-theme' or `theme.el', right?
    (when (or (string-suffix-p "-theme" name-string)
              (string-suffix-p "-theme.el" name-string))
      (message "%s is a theme!" name-string)
      (add-to-list 'custom-theme-load-path dir))
    ;; Load autoloads if we are instructed to do so.
    (when use-package-git-load-autoloads
      (load (expand-file-name (concat name-string "-autoloads.el") dir) t t))))

(defun use-package--read-package (prompt)
  "PROMPT the user for a package name.

Return the symbol"
  (let ((package-names (mapcar (lambda (p) (symbol-name (plist-get p :name)))
                               use-package-git--packages)))
    (intern (completing-read
             (or prompt "Git package: ") package-names nil t
             ;; Pre-select the current project if it was installed with
             ;; `use-package-git'.
             (when-let* ((project-dir (locate-dominating-file default-directory ".git"))
                         (package (file-name-base (directory-file-name project-dir)))
                         (member-p (member package package-names)))
               package)
             use-package-git--read-package-history))))

(defun use-package-git--normalize (config &optional name)
  "Turn CONFIG into a normalized Plist.

CONFIG is a string or Plist.

NAME is a symbol."
  (let* ((config (cond
                  ((stringp config) (list :url config))
                  ((consp config) (if (not (keywordp (car config)))
                                      (progn (setq name (car config))
                                             (cdr config))
                                    config))))
         (url (plist-get config :url))
         (dir (or (plist-get config :dir) (file-name-base url)))
         (files (let ((d (plist-get config :files)))
                  (cond
                   ((stringp d) (list d))
                   ((consp d) d)
                   ((not d) '("*.el")))))
         (ref (or (plist-get config :ref) "master")))
    ;; Validate and assemble the config plist.
    (if (and (symbolp name)
             (stringp url)
             (stringp dir)
             (listp files)
             (stringp ref))
        (list :name name
              :url url
              :dir dir
              :files files
              :ref ref)
      (user-error "CONFIG is not a string or a Plist with a :url key"))))

(defun use-package-git--dispatch-ensure (name args state &optional no-refresh)
  "Dispatch package NAME to the git and elpa ensure functions.))))))

ARGS, STATE, and NO-REFRESH are passed through."
  (unless (plist-get state :ensured)
    (use-package-ensure-elpa name args state no-refresh)))

(defun use-package-normalize/:git (name keyword args)
  "Normalize the :git property list for package NAME.

KEYWORD is the keyword that caused this function to be called
so... it's :git.

ARGS is a list of forms following the KEYWORD--in this case a
list of one."
  (use-package-only-one (symbol-name keyword) args
    (lambda (_label config)
      (condition-case-unless-debug nil
          (use-package-git--normalize config name)
          (use-package-error ":git wants a string or a Plist with a :url key")))))

(defun use-package-handler/:git (name _keyword config rest state)
  "Simply return `body' of package NAME.

STATE is updated to tell `use-package-git--dispatch-ensure' that this
package is already ensured and does not need to dispatch to
`ensure'.

CONFIG is the PList containing the git configuration.

NAME and REST are passed to `use-package-process-keywords'."
  (let* ((state (plist-put state :ensured t))
         (body (use-package-process-keywords name rest state)))
    ;; We use the same logic as `use-package-handler/:ensure'.
    (if (bound-and-true-p byte-compile-current-file)
        ;; Eval when byte-compiling,
        (use-package-git-ensure config)
      ;; Or else wait until runtime.
      (push `(use-package-git-ensure ',config) body))
    body))


;;; Commands

;;;###autoload
(defun use-package-git-ensure (config)
  "Ensure that the git package NAME is cloned.

CONFIG is the Plist supplied as the value to the :git key, then
normalized with `use-package-normalize/:git'."
  (let* ((name (plist-get config :name))
         (dir (expand-file-name (plist-get config :dir) use-package-git-user-dir)))
    (unless (file-exists-p dir)
      (message "use-package-git is cloning package %s..." name)
      (shell-command (format "git -C '%s' clone '%s' '%s'"
                             use-package-git-user-dir
                             (plist-get config :url)
                             dir)
                     "*use-package-git*")
      ;; Check out the ref (should work for branch, hash, or tag).
      (when-let (ref (plist-get config :ref))
        (shell-command (format "git -C %s checkout %s" dir ref) "*use-package-git*"))
      (use-package-git--install config))
    (use-package-git--activate config)
    name))

(defun use-package-git-delete (config)
  "Delete package described by CONFIG."
  (interactive (list (completing-read "Delete package: "
                                      (use-package-git--package-names)
                                      nil t)))
  (delete-directory (expand-file-name (plist-get config :dir))))

;;;###autoload
(defun use-package-git-delete-unused ()
  "Delete unused packages in `use-package-git-user-dir'."
  (interactive)
  (let* ((packages (use-package-git--package-names))
         (directories (directory-files use-package-git-user-dir nil "^[^.]+.*" t))
         (unused (cl-set-difference directories packages :test #'string=)))
    (when (yes-or-no-p (format "Delete packages: %s? " unused))
      (dolist (dir unused)
        (delete-directory (expand-file-name dir use-package-git-user-dir) t t)))))

;;;###autoload
(defun use-package-git-reinstall (package)
  "Re-install PACKAGE.

You may want to re-install the package after you modify the source files."
  (interactive (list (use-package--read-package "Reinstall git package: ")))
  (use-package-git--install (alist-get package use-package-git--packages)))

;;;###autoload
(defun use-package-git-upgrade (package)
  "Upgrade PACKAGE.

PACKAGE is a symbol, which should be a key in `use-package-git--packages'.

Checkout the :ref, fetch changes, and reinstall the package."
  (interactive (list (use-package--read-package "Upgrade git package: ")))
  (let* ((config (alist-get package use-package-git--packages))
         (default-directory (expand-file-name (plist-get config :dir)
                                              use-package-git-user-dir)))
    (message "use-package-git upgrading package %s..." (plist-get config :name))
    ;; Delete automatically generated files so the repo doesn't appear dirty (at
    ;; least not because of `use-package-git').
    (shell-command "rm -f *.elc *-pkg.el *-autoloads.el" "*use-package-git*")
    (when (or (not (use-package-git--dirty-p))
              (while (cl-case (downcase
                               (read-key (concat
                                          "The package `"
                                          (symbol-name (plist-get config :name))
                                          "' with local repo at ["
                                          default-directory
                                          "] is dirty."
                                          " Choose an action:\n"
                                          "[R]eset to HEAD and continue"
                                          " (changes will be lost!)\n"
                                          "[S]kip fetching and continue\n"
                                          "[A]bort\n"
                                          "? ")))
                       (?r (unless (= 0 (shell-command "git reset HEAD --hard"))
                             (error "Resetting the repo at %s failed"
                                    default-directory)))
                       (?s nil)
                       (?a (user-error "Aborted package upgrade"))
                       (_ t))))
      (shell-command (format "git checkout %s" (plist-get config :ref))
                     "*use-package-git*")
      (shell-command "git fetch" "*use-package-git*"))
    (use-package-git--install config)))

;;;###autoload
(defun use-package-git-upgrade-all-packages ()
  "Upgrade all git ensured packages."
  (interactive)
  (message "Upgrading all use-package-git packages...")
  (dolist (package use-package-git--packages)
    (use-package-git-upgrade (car package)))
  (message "Upgrading all use-package-git packages...done."))

;;;###autoload
(defun use-package-git-enable ()
  "Enable `use-package-git'."
  (add-to-list 'use-package-keywords :git)
  (setq use-package-git--previous-ensure-function use-package-ensure-function)
  (setq use-package-ensure-function #'use-package-git--dispatch-ensure)
  (make-directory use-package-git-user-dir t))

(provide 'use-package-git)

;;; use-package-git.el ends here
