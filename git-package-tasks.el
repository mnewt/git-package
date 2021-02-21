;;; git-package-tasks.el --- Git Package Tasks -*- lexical-binding: t -*-

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

;; This package performs most of the actual work for Git Package.  This work is
;; divided into tasks and subtasks.
;;
;; TODO A better description of what this does.  Move some of the docstring info
;; to here?  Not sure about that.

;;; Code:

(require 'tabulated-list)

(require 'git-package)


;;; Variables:

(defcustom git-package-shallow-clone t
  "When non-nil, perform a shallow clone when cloning a new package.

Note that if a :ref is specified, a full clone will be performed
even if this option is non-nil.  This is so that we can be
assured that the correct :ref is available to checkout."
  :group 'git-package
  :type 'string)

(defcustom git-package-subtask-alist
  '((display
     :description "Starting"
     :function git-package-subtask-display-package-list)
    (clone
     :description "Cloning"
     :function git-package-subtask-clone)
    (pull
     :description "Pulling Changes"
     :function git-package-subtask-pull)
    (checkout
     :description "Checking out"
     :function git-package-subtask-checkout)
    (build
     :description "Building"
     :function git-package-subtask-build)
    (description
     :description "Generating Description"
     :function git-package-subtask-description)
    (dependencies
     :description "Ensuring Dependencies"
     :function git-package-subtask-dependencies)
    (wait
     :description "Waiting for Dependencies")
    (missing
     :description "Missing Dependency")
    (byte-compile
     :description "Byte Compiling Elisp"
     :function git-package-subtask-byte-compile)
    (autoloads
     :description "Generating Autoloads"
     :function git-package-subtask-autoloads)
    (info
     :description "Generating Info Files"
     :function git-package-subtask-makeinfo)
    (activate
     :description "Activating"
     :function git-package-subtask-activate)
    (notify
     :description "Notifying Dependents"
     :function git-package-subtask-notify))
  "Alist of tasks to perform on a package."
  :group 'git-package
  :type 'list)

(defcustom git-package-task-alist
  '((activate activate)
    (clone display clone)
    (install display clone checkout build description
     dependencies byte-compile autoloads info activate notify)
    (reinstall display build description dependencies)
    byte-compile autoloads info activate notify
    (upgrade display pull build description dependencies
     byte-compile autoloads info activate))
  "Alist of tasks and their associated subtasks.

CAR is the tasks.

CDR is a list of subtasks to perform one after another to
complete the task.

Tasks are meant to be things that the user wants to do.  Subtasks
are the steps required to perform that task and are not meant to
be invoked directly by the user.  Most subtasks are performed in
sub-processes so that they can be run in parallel."
  :group 'git-package
  :type 'list)

(defcustom git-package-generated-files
  '("*.elc" "*-pkg.el" "*-autoloads.el")
  "Files that `git-package' generates in a package directory."
  :group 'git-package
  :type '(repeat string))

(defcustom git-package-emacs-executable (car command-line-args)
  "The path to the Emacs executable to call for subcommands."
  :group 'git-package
  :type 'string)

(defvar git-package--queue nil
  "Alist of subtasks currently queued.

CAR is the package name.

CDR is an Alist where CAR is a subtask (see
`git-package-subtask-alist') and CDR is a list of queued
subtasks.  It looks like this:

\((foo-package clone ref build ...)
 (bar-package pull)
 (baz-package (wait foo-package bar-package))
 ...)

Each package progresses through its subtasks linearly.
Maintaining state in this way helps track status for display in
the *Git Packages* buffer, manage parallel package tasks, and
flexibly tailor tasks.  The idea is that each subtask can operate
independently and without knowledge of the other subtasks, but
there are exceptions.  There are subtasks, such as wait and
notify, that have to coordinate with other subtasks but that
should be kept to a minimum.")

(defvar git-package--read-package-history nil
  "History for `git-package-upgrade' command.")

(defvar git-package-recipe-elpa
  '(:name elpa
    :url "https://git.savannah.gnu.org/git/emacs/elpa.git"
    :dir "elpa")
  "The recipe for ELPA.")

(defvar git-package-recipe-melpa
  '(:name melpa
    :url "https://github.com/melpa/melpa.git"
    :dir "melpa")
  "The recipe for MELPA.")

(defvar git-package-recipe-emacsmirror
  '(:name epkgs
    :url "https://github.com/emacsmirror/epkgs.git"
    :dir "epkgs")
  "The recipe for Emacsmirror.")

(defgroup git-package-faces nil
  "Faces used by Git Package."
  :group 'git-package
  :group 'faces)

(defface git-package-entry '((t (:inherit link)))
  "Face for `git-package' buffer sections."
  :group 'git-package-faces)

(defface git-package-log-entry '((t (:inherit font-lock-type-face)))
  "Face for `git-package' log messages.")

(defface git-package-command '((t (:inherit org-verbatim)))
  "Face for `git-package' commands.")


;;; Functions:

(declare-function shell-mode "shell")

(defun git-package--log-buffer (&optional name)
  "Return the log buffer associated with NAME, creating it if necessary.

If NAME is non-nil, return the buffer for that NAME.

If NAME is nil, return the main log buffer."
  (let* ((name (cond ((null name) "")
                     ((stringp name) name)
                     ((symbolp name) (symbol-name name))))
         (buffer-name (concat "*git-package-" (when name (concat name "-")) "log*")))
    (or (get-buffer buffer-name)
        (let ((buffer (get-buffer-create buffer-name)))
          (with-current-buffer buffer
            (require 'shell)
            (shell-mode)
            (setq buffer-read-only t))
          buffer))))

(defun git-package--log (name &rest message)
  "Write a MESSAGE to the log buffer associated with NAME.

If NAME is non-nil, write the MESSAGE to the buffer associated
with NAME.

MESSAGE is a `format' string, followed by any format-string
arguments.

FACE is a super secret argument.  If the second argument is a
face, then the the MESSAGE will be printed in that face."
  (with-current-buffer (git-package--log-buffer name)
    (let ((face (if (facep (car message)) (pop message) 'git-package-log-entry))
          (inhibit-read-only t))
      ;; `princ' doesn't leave a trailing newline
      (unless (= 0 (current-column)) (insert "\n"))
      (insert (propertize (apply #'format message) 'face face)
              "\n"))))

(defun git-package--make-tabulated-list-entry (config)
  "Make a `tabulated-list' entry for CONFIG."
  (let ((name (plist-get config :name)))
    (vector
     ;; Name
     (cons (symbol-name name) '(action git-package-goto-log))
     ;; Status
     (or (plist-get git-package-subtask-alist
                    (car (assoc-default name git-package--queue)))
         (if (git-package-installed-p config) "Installed" "Error"))
     ;; Version
     (or (plist-get config :version) "--"))))

(defun git-package--update-tabulated-list-entry (name)
  "Update the entry in `tabulated-list-entries' for NAME."
  (with-current-buffer (git-package--list-buffer)
    (setcdr (assoc name tabulated-list-entries)
            (list (git-package--make-tabulated-list-entry
                   (alist-get name git-package-alist))))
    (tabulated-list-print t t)))

(defun git-package--update-tabulated-list-entries ()
  "Update `tabulated-list-entries' for *Git Packages* buffer."
  (with-current-buffer (git-package--list-buffer)
    (setq tabulated-list-entries
          (mapcar
           (lambda (p)
             (list (car p) (git-package--make-tabulated-list-entry (cdr p))))
           git-package-alist))
    (tabulated-list-print t t)))

(defun git-package--separate-result (string)
  "Separate result form from STRING and return them both."
  (let ((result ""))
     (when-let* ((d0-beg (string-match "\0\0" string))
                 (d0-end (match-end 0))
                 (d1-beg (or (string-match "\0\1" string d0-beg)
                             (length string)))
                 (d1-end (match-end 0)))
       (setq result (concat result (substring string d0-end d1-beg)))
       (setq string (concat (substring string 0 d0-beg)
                            (substring string d1-end))))
     (when-let ((d1-beg (string-match "\0\1" string))
                (d1-end (match-end 0)))
       (setq result (concat result (substring string 0 d1-beg)))
       (setq string (substring string d1-end)))
     (cons string result)))

(defun git-package--process-filter (process string)
  "Respond when PROCESS receives STRING.

Print log output to the PROCESS log buffer.

Accumulate a :result string which will later be passed to
:handler by `git-package--process-sentinel' will send those those
to the handler."
  (let* ((separated (git-package--separate-result string))
         (string (car separated))
         (result (cdr separated)))
    (process-put process :result (concat (process-get process :result) result))
    (when (buffer-live-p (process-buffer process))
      (with-current-buffer (process-buffer process)
        (let ((moving (= (point) (process-mark process))))
          (save-excursion
            ;; Insert the text, advancing the process marker.
            (goto-char (process-mark process))
            (let ((inhibit-read-only t))
              (insert string))
            (set-marker (process-mark process) (point)))
          (when moving (goto-char (process-mark process))))))))

(defun git-package--process-sentinel (process event)
  "Respond when PROCESS receives EVENT.

If a handler is specified in the process Plist, call it.

If the process is part of `git-package--queue', start the next
subtask."
  (let* ((coding-system-for-read 'utf-8-auto)
         (name (process-get process :name))
         (config (alist-get name git-package-alist))
         (description (format "Subtask %s for package %s "
                              (car (alist-get name git-package--queue))
                              name)))
    (cond
     ((and (eq 'exit (process-status process)) (= 0 (process-exit-status process)))
      ;; If there is a handler attached, pass it the result.
      (when-let ((handler (process-get process :handler)))
        (condition-case err
            ;; :result is populated by `git-package--process-filter'.
            (funcall handler (read (process-get process :result)))
          (error (git-package--log name 'error "error in handler: %s" err))))
      (git-package--log name (concat description event))
      (git-package--next-subtask config))
     ;; Something bad happened. Stop the task.  TODO Give the user a better
     ;; indication of what went wrong and what to do about it.
     (t (git-package--log name 'error (concat description event))))))

(defun git-package--async-process (command &rest properties)
  "Run COMMAND asynchronously, calling a sentinel when finished.

COMMAND is a list starting with the program file name, followed
by strings to give the program as arguments.

PROPERTIES are keyword/argument pairs, as in `make-process', for
which this is a wrapper.

Arguments which are different are:

:name NAME -- NAME is a name for the process, but is expected to
be a symbol.  A log stdout is created based on NAME, and if it's
nil then the main log stdout is used.

:handler HANDLER -- If non-nil and the process exits normally,
then call this function, whose single argument is a string which
is the output of the process.

:dir DIR -- If non-nil, Execute the process in DIR.

Additional properties are attached as properties to the process
so that the process sentinel can pick them up and use them as
context when the process updates or finishes."
  (let* ((name (plist-get properties :name))
         (buffer (git-package--log-buffer name))
         (process-name (concat "git-package-process" (when name (format "-%s" name))))
         (default-directory (if-let ((dir (plist-get properties :dir)))
                                (git-package--absolute-path dir)
                              default-directory))
         proc)
    (git-package--log name 'git-package-command (mapconcat #'identity command " "))
    (setq proc (make-process :name process-name
                             :buffer buffer
                             :command command
                             :connection-type 'pipe
                             :coding 'utf-8-auto
                             :filter #'git-package--process-filter
                             :sentinel #'git-package--process-sentinel))
    (set-process-plist proc (append properties (process-plist proc)))
    proc))

(defun git-package--async-funcall (function &rest properties)
  "Call FUNCTION in an inferior, asynchronous Emacs process.

PROPERTIES is a Plist.  It is used to store process state and for
interprocess communication.  Special PROPERTIES include:

:handler HANDLER -- A function which is called with the return
value as its argument.

:variables VARIABLES -- A list of variables to be injected into
the inferior Emacs process.

For more details, see `git-package--async-process'."
  (let ((print-level nil)
        (print-length nil)
        (print-escape-nonascii t)
        (print-circle t)
        (process
         (apply #'git-package--async-process
                `(,git-package-emacs-executable "--batch" "--quick"
                  "--eval" "(setq load-prefer-newer t)"
                  ,@(when-let* ((dir (plist-get properties :dir))
                                (dir (git-package--absolute-path dir)))
                     `("--chdir" ,dir "--directory" ,dir))
                  "--load" ,(locate-library "git-package")
                  "--load" ,(locate-library "git-package-async")
                  "--funcall" "git-package-async-batch-pipe")
                :name (plist-get properties :name)
                properties))
        (form `(let ,(mapcar
                      (lambda (sym) `(,sym ',(symbol-value sym)))
                      (plist-get properties :variables))
                (message "calling function...")
                (funcall ',function))))
    (process-send-string process (concat (prin1-to-string form) "\n"))
    (process-send-eof process)
    process))

(defun git-package--dirty-p (&optional dir)
  "Return non-nil if the git repo DIR is dirty."
  (not (= 0 (length (shell-command-to-string
                     (format "git -C '%s' status --porcelain"
                             (expand-file-name (or dir default-directory))))))))

(defun git-package--read-package (prompt)
  "PROMPT the user for a package name.

Return the symbol"
  (let ((package-names (mapcar (lambda (p) (symbol-name (plist-get (cdr p) :name)))
                               git-package-alist)))
    (intern (completing-read
             (or prompt "Git package: ") package-names nil t
             ;; Pre-select the current project if it was installed with
             ;; `git-package'.
             (when-let* ((project-dir (locate-dominating-file default-directory ".git"))
                         (package (file-name-base (directory-file-name project-dir)))
                         (member-p (member package package-names)))
               package)
             git-package--read-package-history))))

(defun git-package-recipe-elpa (name)
  "Get the recipe for package NAME from ELPA."
  (if (eq name 'elpa)
      git-package-recipe-elpa
    (unless (file-directory-p (git-package--absolute-path "elpa"))
      (git-package-do 'clone git-package-recipe-elpa 'normalized))
    ;; FIXME Queue the rest of the work.
    (let ((dir (git-package--absolute-path "elpa" "packages" (symbol-name name))))
      (when (file-directory-p dir)
        (append git-package-recipe-elpa (list :subdir (symbol-name name)))))))

(defun git-package-recipe-melpa (name)
  "Get the recipe for package NAME from MELPA."
  (if (eq name 'melpa)
      git-package-recipe-melpa
    (unless (file-directory-p (git-package--absolute-path "melpa"))
      (git-package-do 'clone git-package-recipe-melpa 'normalized))
    (let ((file (git-package--absolute-path "melpa" "recipes" (symbol-name name))))
      (when (file-exists-p file)
        (cons :name (car (with-temp-buffer
                           (insert-file-contents-literally file)
                           (read-from-string (buffer-string)))))))))

(defun git-package-recipe-emacsmirror (name)
  "Get the recipe for package NAME from EmacsMirror."
  (unless (file-directory-p (git-package--absolute-path "epkgs"))
    (git-package-do 'clone git-package-recipe-emacsmirror 'normalized))
  (let ((name-string (symbol-name name)))
    (when (file-directory-p (git-package--absolute-path "epkgs" "mirror" name-string))
      (list :name name
            :url (format "https://github.com/emacsmirror/%s.git" name-string)
            :dir name-string))))

(defun git-package-get (name-or-config property)
  "Get PROPERTY for package with NAME-OR-CONFIG."
  (let ((name (if (consp name-or-config)
                  (plist-get name-or-config :name)
                name-or-config)))
    (plist-get (alist-get name git-package-alist) property)))

(defun git-package-set (name-or-config property value)
  "Set PROPERTY to VALUE for package with NAME-OR-CONFIG."
  (let ((name (if (consp name-or-config)
                  (plist-get name-or-config :name)
                name-or-config)))
    (plist-put (alist-get name git-package-alist) property value)))

(defun git-package--next-subtask (config &optional current)
  "Start the next subtask for CONFIG.

If CURRENT is non-nil, call the current task rather than the next
one.

This function only has an effect if CONFIG has an associated
entry in `git-package--queue'."
  (when-let* ((name (plist-get config :name))
              (entry (assoc name git-package--queue)))
    (unless current
      ;; Pop the current subtask off the queue.
      (setcdr (assoc name git-package--queue)
              (cdr (alist-get name git-package--queue))))
    (if-let ((next-subtask (car (alist-get name git-package--queue))))
        ;; If there is a subtask in the queue, start it.
        (funcall
         (plist-get (alist-get next-subtask git-package-subtask-alist) :function)
         config)
      ;; If there are no more subtasks, the task is complete.
      (setq git-package--queue (assq-delete-all name git-package--queue))
      (git-package--log name "Task complete for %s." name))
    ;; Update the *Git Packages* buffer.
    (git-package--update-tabulated-list-entry name)))

(defun git-package-subtask-activate (config)
  "Activate the package described by CONFIG."
  (git-package-activate config)
  (git-package--next-subtask config))

(defun git-package-subtask-display-package-list (config)
  "Display the Git Packages list buffer, continue CONFIG queue."
  (pop-to-buffer (git-package--list-buffer))
  (git-package--next-subtask config))

(defun git-package-subtask-clone (config)
  "Clone the repository for CONFIG."
  (unless (git-package-installed-p config)
    (git-package--async-process
     `("git" "clone"
       ;; If no :ref is specified, we can do a shallow clone. If a :ref is
       ;; specified, we don't know whether it is a branch or commit so we have
       ;; to do a full clone, and that seems fine since asking for a :ref is a
       ;; decent heuristic for whether the user wants to interact with the
       ;; repository.
       ,@(when (and git-package-shallow-clone (plist-get config :ref))
          '("--depth" "1"))
       ,(plist-get config :url)
       ,(git-package--absolute-path config))
     :name (plist-get config :name))))

(defun git-package-subtask-pull (config)
  "Pull the repository for CONFIG.

Pull if the repo is clean.  We don't do any complicated
fetch/merge because if the user wants that they should do it
manually anyway.

Nor do we check that we have the appropraite :ref checked out.
If the user changed it then they know what they are doing and we
will leave them alone."
  (let ((dir (plist-get config :dir)))
    (if (git-package--dirty-p dir)
        (git-package--async-process
         `("git" "-C" ,(git-package--absolute-path config)
           "pull")
         :name (plist-get config :name))
      (git-package--log (plist-get config :name) "Repo is dirty so not pulling")
      (git-package--next-subtask config))))

;; TODO Should :ref, :branch, and :commit be treated the same?
(defun git-package-subtask-checkout (config)
  "If appropriate, check a git ref for CONFIG."
  (if-let (ref (plist-get config :ref))
      (git-package--async-process
       `("git" "-C" ,(git-package--absolute-path config)
         "checkout" ,ref)
       :name (plist-get config :name))
    (git-package--next-subtask config)))

(defun git-package-subtask-build (config)
  "Run build commands for CONFIG."
  (if-let ((command (plist-get config :build)))
      (git-package--async-process
       (split-string command " ")
       :name (plist-get config :name))
    (git-package--next-subtask config)))

(defun git-package-subtask-makeinfo (config)
  "Run build commands for CONFIG."
  (if-let ((files (mapcan
                   (lambda (dir)
                     (directory-files dir t "\\.texi\\(?:nfo\\)?\\'" t))
                   (git-package--load-paths config))))
      (git-package--async-funcall
       (lambda () (git-package-async-makeinfo config))
       :name (plist-get config :name))
    (git-package--next-subtask config)))

(defun git-package-subtask-description (config)
  "Read package headers and add description elements to CONFIG."
  (let ((name (plist-get config :name)))
    (git-package--async-funcall
     (lambda ()
       (git-package-async-description config))
     :name name
     :handler
     (lambda (plist)
       (while plist
         (let ((prop (pop plist))
               (val (pop plist)))
           (git-package-set name prop val)))
       (git-package--next-subtask config)))))

(defun git-package-subtask-dependencies (config)
  "Check dependencies for CONFIG and install them if possible.

When a dependency isn't installed, the 'wait subtask is pushed to
the front of the queue for CONFIG and an install task is started
for the dependency.  Once the dependency is finished installing,
the wait subtask is removed and the install of the original
package is resumed.

When a dependency is already installed but the version is too
low, the 'missing subtask is pushed to the front of the queue.
It can only be cleared by the user."
  (let ((name (plist-get config :name))
        waiting missing)
    ;; Find dependencies for which we are waiting or missing.
    (mapc (lambda (name-version)
            (let ((dep-name (car name-version))
                  (version (cdr name-version)))
              (cond
               ((eq 'emacs dep-name)
                (version< emacs-version version)
                (push name-version missing))
               ((git-package-installed-p dep-name)
                (when (version< (git-package-get dep-name :version) version)
                  (push name-version missing)))
               (t
                (push dep-name waiting)
                (git-package-do 'install (git-package--normalize dep-name))))))
          (plist-get config :dependencies))
    (cond
     (missing (git-package--log 'error "Package %s is missing dependencies %S"
                                name missing)
              (push 'missing (alist-get name git-package--queue))))
    (if (not waiting)
        (git-package--next-subtask config)
      (push 'wait (alist-get name git-package--queue))
      (git-package-set config :missing waiting))))

(defun git-package-subtask-byte-compile (config)
  "Compile the Emacs Lisp files for CONFIG."
  (git-package--async-funcall
   (lambda () (git-package-async-byte-compile config))
   :name (plist-get config :name)
   :variables '(load-path)))

(defun git-package-subtask-autoloads (config)
  "Generate the autoloads file for CONFIG."
  (git-package--async-funcall
   (lambda () (git-package-async-autoloads config))
   :name (plist-get config :name)))

(defun git-package-subtask-notify (config)
  "Notify any packages waiting on CONFIG to be installed."
  (let ((name (plist-get config :name)))
    (mapc
     (lambda (entry)
       (when (and (eq 'wait (cadr entry))
                  (let ((notified-package (alist-get (car entry) git-package-alist)))
                    (member name (mapcar #'car
                                         (plist-get notified-package :dependencies)))
                    (git-package--log name
                                      "Notifying package %s that %s is installed..."
                                      (car entry)
                                      name)
                    (git-package--next-subtask notified-package)))))
     git-package--queue)
    (git-package--next-subtask config)))

(defun git-package-list--id-predicate (a b)
  "Predicate to sort packages A and B using their name.

This is used for `tabulated-list-format' in `git-package-list-mode'."
  (string< (symbol-name (car a))
           (symbol-name (car b))))

(defun git-package-list--status-predicate (a b)
  "Predicate to sort packages A and B using their status.

This is used for `tabulated-list-format' in `git-package-list-mode'."
  (string< (elt (cdr a) 1)
           (elt (cdr b) 1)))

(defun git-package-list--version-predicate (a b)
  "Predicate to sort packages A and B using their version.

This is used for `tabulated-list-format' in `git-package-list-mode'."
  (string< (elt (cdr a) 2)
           (elt (cdr b) 2)))

(defun git-package--list-buffer ()
  "Return the Git Package List buffer, creating it if necessary."
  (or (get-buffer "*Git Packages*")
      (let ((buffer (get-buffer-create "*Git Packages*")))
        (with-current-buffer buffer
          (git-package-list-mode))
        buffer)))


;;; Commands:

(defun git-package-goto-log (name-or-marker)
  "Go to the log buffer for package with NAME-OR-MARKER."
  (interactive (list (intern (thing-at-point 'symbol))))
  (pop-to-buffer (git-package--log-buffer
                  (cond
                   ((markerp name-or-marker)
                    (let ((button (button-at (marker-position name-or-marker))))
                      (intern
                       (buffer-substring-no-properties (button-start button)
                                                       (button-end button)))))
                   ((symbolp name-or-marker)
                    name-or-marker)))))

(defun git-package-resume (package)
  "Resume processing the tasks in queue for PACKAGE."
  (interactive (list (intern (completing-read
                              "Resume package: "
                              (mapcar (lambda (e) (symbol-name (car e)))
                                      git-package--queue)
                              nil t))))
  (git-package--next-subtask (alist-get package git-package-alist) 'resume))

;; (defvar git-package-list-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (set-keymap-parent map tabulated-list-mode-map)
;;     (define-key map "\C-m" #'git-package-goto-log)
;;     map))

(defun git-package--imenu-prev-index-position-function ()
  "Move point to previous line in the Git Packages buffer.

This function is used as a value for
`imenu-prev-index-position-function'."
  (unless (bobp)
    (forward-line -1)))

(defun package--imenu-extract-index-name-function ()
  "Return imenu name for line at point.

This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line."
  (let ((config (assoc-default (tabulated-list-get-id) git-package-alist)))
    (symbol-name (plist-get config :name))))

(define-derived-mode git-package-list-mode tabulated-list-mode "Git Package"
  "Major mode for the Git Package Status buffer.
\\<git-package-status-mode-map>
\\{git-package-status-mode-map}"
  ;; TODO Update this when there are no more running processes.
  (setq mode-line-process '((git-package--queue ":Working"))
        tabulated-list-format
        `[("Package" 22 git-package-list--id-predicate)
          ("Status" 10 git-package-list--status-predicate)
          ("Version" 10 git-package-list-version-predicate)]
        tabulated-list-sort-key '("Status"))
  (tabulated-list-init-header)
  (setq imenu-prev-index-position-function
        #'git-package--imenu-prev-index-position-function)
  (setq imenu-extract-index-name-function
        #'package--imenu-extract-index-name-function))

;;;###autoload
(defun git-package-list ()
  "List the packages activated with Git Package."
  (interactive)
  (switch-to-buffer (git-package--list-buffer))
  (git-package--update-tabulated-list-entries))

;;;###autoload
(defun git-package-do (task config &optional normalized)
  "Perform TASK for the package described by CONFIG.

If NORMALIZED is non-nil, assume the config is already normalized
and don't do it again.

See `git-package-task-alist' for the possible tasks."
  (let ((config (if normalized config (git-package--normalize config)))
        (name (plist-get config :name))
        (task-list (alist-get task git-package-task-alist)))
    (add-to-list 'git-package-alist (cons name config))
    ;; Delete any currently queued tasks, since the most likely reason for that
    ;; is that something failed previously.  TODO Decide if this is the correct
    ;; action.  Maybe prompt the user if the queue isn't empty.
    (setq git-package--queue
          (delete (assoc name git-package--queue) git-package--queue))
    ;; Queue the tasks.
    (push (cons name task-list) git-package--queue)
    (git-package--update-tabulated-list-entries)
    (git-package--next-subtask config t)))

;; Create `git-package-do-*' from `git-package-task-alist'.
;; (mapc (lambda (task)
;;         (eval
;;          `(defun ,(intern (format "git-package-do-%s" task))
;;               (config &optional normalized)
;;            ,(format "Perform the %s task for the package described by CONFIG." task)
;;            (git-package-do ',task config normalized))))
;;       (mapcar #'car git-package-task-alist))

;; TODO Indicate if the package is dirty.
;;;###autoload
(defun git-package-delete (name)
  "Delete the package identified by NAME."
  (interactive (list (intern (completing-read
                              "Delete package: "
                              (mapcar (lambda (p) (symbol-name (car p)))
                                      git-package-alist)
                              nil t))))
  (let ((dir (plist-get (alist-get name git-package-alist) :dir)))
    (when (or (not (git-package--dirty-p dir)
                   (yes-or-no-p (format "Package %s is dirty, still delete?"
                                        (plist-get config :name)))))
      (delete-directory (git-package--absolute-path config) t)
      (setq git-package-alist
            (assq-delete-all (plist-get config :name) git-package-alist)))))

;;;###autoload
(defun git-package-delete-unused ()
  "Delete unused packages in `git-package-user-directory'.

Unused packages are defined as directories on disk in the
`git-package-user-directory' that have not been activated in the
current Emacs session using `git-package'."
  (interactive)
  (let* ((active (mapcar (lambda (p) (plist-get (cdr p) :dir))
                         git-package-alist))
         (on-disk (directory-files git-package-user-directory nil "^[^.]+.*" t))
         (unused (cl-set-difference on-disk active :test #'string=)))
    (when (yes-or-no-p (format "Delete packages: %s? " unused))
      (dolist (dir unused)
        (when (or (not (git-package--dirty-p dir))
                  (yes-or-no-p
                   (format "Repo for package %s is dirty, delete anyway? " dir)))
          (message "Deleting git package: %s..." dir)
          (delete-directory (expand-file-name dir git-package-user-directory) t t)))
      (message "Done."))))

;;;###autoload
(defun git-package-install (url)
  "Install a PACKAGE by name or git URL."
  (interactive "sInstall package (name or url): ")
  (let ((name (git-package url)))
    (message "Package %s is installed." (symbol-name name))))

;;;###autoload
(defun git-package-reinstall (package)
  "Re-install PACKAGE.

You may want to re-install the package after you modify the source files.

Note that this does not fetch changes from the git repository if
the package if it is already installed.  For that, use
`git-package-upgrade'."
  (interactive (list (git-package--read-package "Reinstall git package: ")))
  (if-let* ((package (if (stringp package) (intern package) package))
            (config (alist-get package git-package-alist)))
      (git-package-do 'reinstall config)
    (user-error "Package %s is not installed" package)))

;;;###autoload
;; TODO Re-do this to work asynchronously.
(defun git-package-upgrade (package)
  "Upgrade PACKAGE.

PACKAGE is a symbol, which should be a key in `git-package-alist'.

Checkout the :ref, fetch changes, and reinstall the package."
  (interactive (list (git-package--read-package "Upgrade git package: ")))
  (let* ((config (alist-get package git-package-alist))
         (default-directory (expand-file-name (plist-get config :dir)
                                              git-package-user-directory))
         (name (plist-get config :name)))
    ;; Delete automatically generated files so the repo doesn't appear dirty (at
    ;; least not because of `git-package').
    ;; TODO: Ignore these files instead of deleting them
    (git-package--async-process name `("rm" "-f" ,@git-package-generated-files))
    (when (or (not (git-package--dirty-p))
              (let ((key (downcase
                          (read-key
                           (concat "The package "
                                   (symbol-name (plist-get config :name))
                                   " with local repo at "
                                   default-directory
                                   " is dirty."
                                   " Choose an action:\n"
                                   "[R]eset to HEAD and continue"
                                   " (changes will be lost!)\n"
                                   "[S]kip fetching and continue\n"
                                   "[A]bort\n"
                                   "? ")))))
                (while (cond
                        ((eq key ?r) (git-package--async-process
                                      name
                                      '("git" "reset" "HEAD" "--hard")))
                        ((eq key ?s) nil)
                        ((eq key ?a) (user-error "Aborted package upgrade"))
                        (t t)))))
      (switch-to-buffer (git-package--list-buffer))
      (git-package--log (plist-get config :name) "git-package upgrading package %s...")
      (git-package-do 'upgrade config))))

;;;###autoload
(defun git-package-upgrade-all-packages ()
  "Upgrade all git ensured packages."
  (interactive)
  (message "Upgrading all git packages...")
  (dolist (package git-package-alist)
    (git-package-upgrade (car package)))
  (message "Upgrading all git packages...done."))


(provide 'git-package-tasks)

;;; git-package-tasks.el ends here
