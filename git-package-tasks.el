;;; git-package-tasks.el --- Git Package Tasks -*- lexical-binding: t -*-

;; Author: Matthew Sojourner Newton
;; Maintainer: Matthew Sojourner Newton


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
  '((clone
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
    (prepare
     :description "Preparing"
     :function git-package-subtask-prepare)
    (activate
     :description "Activating"
     :function git-package-subtask-activate)
    (notify
     :description "Notifying Dependents"
     :function git-package-subtask-notify))
  "Alist of possible states a package can be in."
  :group 'git-package
  :type 'list)

(defcustom git-package-task-alist
  '((clone clone)
    (install clone checkout build description dependencies prepare activate)
    (upgrade pull build dependencies info prepare activate))
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

(defgroup git-package-faces nil
  "Faces used by Git Package."
  :group 'git-package
  :group 'faces)

(defface git-package-entry'((t (:inherit link)))
  "Face for `git-package' buffer sections."
  :group 'git-package-faces)

(defface git-package-log-entry '((t (:inherit font-lock-keyword-face)))
  "Face for `git-package' log messages.")

(defface git-package-command '((t (:inherit org-verbatim)))
  "Face for `git-package' commands.")


;;; Functions:

(declare-function shell-mode "shell")

(defun git-package--log-buffer (&optional name)
  "Return a log buffer associated with NAME, creating it if necessary.

If NAME is non-nil, return the buffer for that NAME.

If NAME is nil, return the main log buffer."
  (let ((buffer-name (concat "*git-package-"
                             (when name (concat (symbol-name name) "-"))
                             "log*")))
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
     (cons (symbol-name name) '(action git-package-goto-log))
     (or (plist-get git-package-subtask-alist
                    (car (assoc-default name git-package--queue)))
         (if (git-package-installed-p config) "Installed" "Error")))))

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

;; WIP
(defun git-package--next-subtask (config)
  "Start the next subtask for CONFIG.

This function only has an effect if CONFIG has an associated
entry in `git-package--queue'."
  (when-let* ((name (plist-get config :name))
              (entry (assoc name git-package--queue)))
    ;; Pop the current subtask off the queue.
    (setcdr (assoc name git-package--queue)
            (cdr (alist-get name git-package--queue)))
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

;; WIP
(defun git-package--process-filter (process string)
  "Respond when PROCESS receives STRING.

Print log output to the PROCESS log buffer.

Accumulate values surrounded by null bytes.
`git-package--process-sentinel' will send those those to the
handler."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((moving (= (point) (process-mark process))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark process))
          (insert string)
          (set-marker (process-mark process) (point)))
        ;; TODO Extract a form enclosed by null bytes and attach it to the
        ;; process.
        (when-let ((beg (string-match "\0\0" string)))
          (process-put :result (concat (process-get :result))))
        (when moving (goto-char (process-mark process)))))))


(defun git-package--process-sentinel (process event)
  "Respond when PROCESS receives EVENT.

If a handler is specified in the process Plist, call it.

If the process is part of `git-package--queue', start the next
subtask."
  (let* ((name (process-get process :name))
         (config (alist-get name git-package-alist))
         (description (format "Subtask %s for package %s "
                              (car (alist-get name git-package--queue))
                              name)))
    (cond
     ((and (eq 'exit (process-status process)) (= 0 (process-exit-status process)))
      (git-package--log name (concat description event))
      (when-let ((handler (process-get process :handler)))
        ;; When `git-package--async-process' is called with a property for
        ;; :handler, that function is called with the process output in string
        ;; form.
        (with-current-buffer (process-buffer process)
          (let ((default-directory (git-package--absolute-path
                                    (or (plist-get config :dir)
                                        (process-get process :dir)
                                        git-package-user-dir))))
            (funcall handler
                     (buffer-substring-no-properties
                      (process-get process :start)
                      (marker-position (process-mark process)))))))
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
be a symbol.  A log buffer is created based on NAME, and if it's
nil then the main log buffer is used.

:sentinel SENTINEL -- If non-nil, override the default SENTINEL,
which is `git-package--process-sentinel'.

:handler HANDLER -- If non-nil and the process exits normally,
then call this function, whose single argument is a string which
is the output of the process.

:dir DIR -- If non-nil, Execute the process in DIR.

Additional properties are attached as properties to the process
so that the process sentinel can pick them up and use them as
context when the process updates or finishes."
  (let* ((name (plist-get properties :name))
         (buffer (git-package--log-buffer name))
         (process-name (concat "git-package-process"
                               (when name (format "-%s" name))))
         (default-directory (if-let ((dir (plist-get properties :dir)))
                                (git-package--absolute-path dir)
                              default-directory))
         proc start)
    (git-package--log name 'git-package-command (mapconcat #'identity command " "))
    ;; Save point so that later we know where the output for the process
    ;; started.
    ;; TODO Handle this with a filter instead.
    (with-current-buffer buffer (setq start (point)))
    (setq proc (make-process :name process-name
                             :buffer buffer :command command
                             :sentinel (or (plist-get properties :sentinel)
                                           #'git-package--process-sentinel)))
    (set-process-plist proc (append `(:start ,start)
                                    properties
                                    (process-plist proc)))
    proc))

(defun git-package--funcall-wrap-handler (handler)
  "Read a response as an Elisp object and call HANDLER.

HANDLER is given one argument: the object."
  (lambda (response)
    (funcall handler (read response))))

(defun git-package--async-funcall (form &rest properties)
  "Evaluate FORM in an inferior, asynchronous Emacs process.

PROPERTIES is a Plist.  It is used to store process state and for
interprocess communication.  Special PROPERTIES include:

:handler HANDLER -- A function which is called with the return
value as its argument.

:variables VARIABLES -- A list of variables to be injected into
the inferior Emacs process.

For more details, see `git-package--async-process'."
  (let ((process
         (apply #'git-package--async-process
                `(,git-package-emacs-executable "--batch" "--quick"
                  ,@(when-let ((dir (plist-get properties :dir)))
                     (list "--chdir" (git-package--absolute-path dir)))
                  "--load" ,(locate-library "git-package")
                  "--load" ,(locate-library "git-package-async")
                  "--funcall" "git-package-async-batch-pipe")
                :name (plist-get properties :name)
                :handler (when-let ((handler (plist-get properties :handler)))
                           (git-package--funcall-wrap-handler handler))
                properties))
        (wrapped-form `(progn
                         ,(mapcar (lambda (sym)
                                    `(setq ,sym ',(symbol-value sym)))
                           (plist-get properties :variables))
                         ,form)))
    (process-send-string process (concat (prin1-to-string wrapped-form) "\n"))
    process))

(defun git-package--dirty-p (&optional dir)
  "Return non-nil if the git repo DIR is dirty."
  (not (= 0 (length (shell-command-to-string
                     (format "git -C '%s' status --porcelain"
                             (expand-file-name (or dir default-directory))))))))

(defun git-package--package-names ()
  "List the package names activated by `git-package'."
  (mapcar (lambda (p) (symbol-name (car p))) git-package-alist))

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

(defun git-package--resolve-url (config)
  "Resolve the URL for package NAME and CONFIG."
  (cond
   ((plist-get config :url)
    (plist-get config :url))
   ((plist-get config :repo)
    (format
     (assoc-default (or (plist-get config :fetcher) 'github) git-package-fetcher-alist)
     (plist-get config :repo)))))

(defun git-package-recipe-melpa (name)
  "Get the recipe for package NAME from MELPA."
  (let ((config '(:name melpa
                  :url "https://github.com/melpa/melpa.git"
                  :dir "melpa"))
        (file (git-package--absolute-path "melpa" "recipes" (symbol-name name))))
    (unless (git-package-installed-p config)
      (git-package-do 'clone config))
    (when (file-exists-p file)
      (cons :name (car (with-temp-buffer
                         (insert-file-contents-literally file)
                         (read-from-string (buffer-string))))))))

(defun git-package-recipe-elpa (name)
  "Get the recipe for package NAME from ELPA."
  (let ((elpa-base '(:name elpa
                     :url "https://git.savannah.gnu.org/git/emacs/elpa.git"
                     :dir "elpa"))
        (dir (git-package--absolute-path "elpa" "packages" (symbol-name name))))
    (git-package-ensure elpa-base)
    (when (file-directory-p dir)
      (append elpa-base (list :subdir (symbol-name name))))))

(defun git-package-recipe-emacsmirror (name)
  "Get the recipe for package NAME from EmacsMirror."
  (git-package-ensure '(:name epkgs
                        :url "https://github.com/emacsmirror/epkgs.git"
                        :dir "epkgs" :files (nil)))
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

(defun git-package-subtask-clone (config)
  "Clone the repository for CONFIG."
  (git-package--async-process
   `("git" "clone"
     ;; If no :ref is specified, we can do a shallow clone. If a :ref is specified,
     ;; we don't know whether it is a branch or commit so we have to do a full
     ;; clone.
     ,@(when (and git-package-shallow-clone (plist-get config :ref))
        '("--depth" "1"))
     ,(plist-get config :url)
     ,(git-package--absolute-path (plist-get config :dir)))
   :name (plist-get config :name)))

(defun git-package-subtask-checkout (config)
  "If appropriate, check a git ref for CONFIG."
  (if-let (ref (plist-get config :ref))
      (git-package--async-process
       `("git"
         "-C" ,(git-package--absolute-path (plist-get config :dir))
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

(defun git-package-subtask-description (config)
  "Read package headers and add description elements to CONFIG."
  (let ((name (plist-get config :name)))
    (git-package--async-funcall
     `(git-package-async-description ',config)
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
                (git-package-do 'install (git-package-normalize dep-name))))))
          (plist-get config :dependencies))
    (cond
     (missing (git-package--log 'error "Package %s is missing dependencies %S"
                                name missing)
              (push 'missing (alist-get name git-package--queue))))
    (if (not waiting)
        (git-package--next-subtask config)
      (push 'wait (alist-get name git-package--queue))
      (git-package-set config :missing waiting))))

(defvar generated-autoload-file)
(defvar autoload-timestamps)
(defvar version-control)

(declare-function autoload-rubric "autoload" (file &optional type feature))

(defun git-package--generate-autoloads (config)
  "Generate autoloads for CONFIG.

Adapted from the one in `package.el'."
  (let* ((package-dir (plist-get config :dir))
         (auto-name (format "%s-autoloads.el" (plist-get config :name)))
         ;;(ignore-name (concat name "-pkg.el"))
         (generated-autoload-file (expand-file-name auto-name package-dir))
         ;; We don't need 'em, and this makes the output reproducible.
         (autoload-timestamps nil)
         (backup-inhibited t)
         (version-control 'never))
    (unless (file-exists-p generated-autoload-file)
      (require 'autoload)
      (write-region (autoload-rubric generated-autoload-file "package" nil)
                    nil generated-autoload-file nil 'silent))
    (update-directory-autoloads (git-package--absolute-path package-dir))
    (let ((buf (find-buffer-visiting generated-autoload-file)))
      (when buf (kill-buffer buf)))
    auto-name))

;; WIP
(defun git-package-subtask-compile (config)
  "Compile the Emacs Lisp files for CONFIG."
  (let ((name (plist-get config :name)))
    (git-package--async-funcall
     `(git-package-async-compile ',config)
     :name name
     ;; :variables '(load-path)
     :handler
     (lambda (_files)
       (git-package--next-subtask config)))))

;; TODO Break each task into separate functions:
;; X dependencies
;; - compile
;; - autoloads
;; - byte compile
;; - info
(defun git-package-subtask-prepare (config)
  "Install the package described by CONFIG."
  (save-window-excursion
    (let* ((default-directory (git-package--absolute-path (plist-get config :dir)))
           ;; Tell `update-directory-autoloads' to save to this file.
           (generated-autoload-file
            (expand-file-name (format "%s-autoloads.el" (plist-get config :name))
                              default-directory)))
      (git-package--ensure-dependencies config)
      (when-let ((command (plist-get config :command))) (compile command))
      (message "Writing autoload file: %s" generated-autoload-file)
      (apply #'update-directory-autoloads (git-package--load-paths config))
      (git-package--byte-compile config))))

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
        `[("Package" 24 git-package-list--id-predicate)
          ("Status" 10 git-package-list--status-predicate)]
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
(defun git-package-do (task config)
  "Perform TASK for the package described by CONFIG.

See `git-package-task-alist' for the possible tasks."
  (let ((name (plist-get config :name)))
    (add-to-list 'git-package-alist (cons name config))
    (setcdr (assoc name git-package--queue)
            (alist-get task git-package-task-alist))
    (git-package--update-tabulated-list-entries)
    (funcall (plist-get (alist-get (car (alist-get name git-package--queue))
                                   git-package-subtask-alist) :function)
             config)))

;; Create `git-package-do-*' from `git-package-task-alist'.
(mapc (lambda (task)
        (eval
         `(defun ,(intern (format "git-package-do-%s" task)) (config)
           (format "Perform the %s task for the package described by CONFIG." task)
           (git-package-do ',task config))))
      (mapcar #'car git-package-task-alist))

;;;###autoload
(defun git-package-delete (config)
  "Delete the package described by CONFIG."
  (interactive (list (completing-read "Delete package: "
                                      (git-package--package-names)
                                      nil t)))
  (delete-directory (expand-file-name (plist-get config :dir)))
  (setq git-package-alist
        (assq-delete-all (plist-get config :name) git-package-alist)))

;;;###autoload
(defun git-package-delete-unused ()
  "Delete unused packages in `git-package-user-dir'.

Unused packages are defined as directories on disk in the
`git-package-user-dir' that have not been activated in the
current Emacs session using `git-package'."
  (interactive)
  (require 'cl-seq)
  (let* ((active (mapcar (lambda (p) (plist-get (cdr p) :dir))
                         git-package-alist))
         (on-disk (directory-files git-package-user-dir nil "^[^.]+.*" t))
         (unused (cl-set-difference on-disk active :test #'string=)))
    (when (yes-or-no-p (format "Delete packages: %s? " unused))
      (dolist (dir unused)
        (when (or (not (git-package--dirty-p dir))
                  (yes-or-no-p
                   (format "Repo for package %s is dirty, delete anyway? " dir)))
          (message "Deleting git package: %s..." dir)
          (delete-directory (expand-file-name dir git-package-user-dir) t t)))
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
  (let* ((package (if (stringp package) (intern package) package))
         (config (alist-get package git-package-alist)))
    (git-package-subtask-prepare config)
    (git-package-subtask-activate config)))

;;;###autoload
;; TODO Re-do this to work asynchronously.
(defun git-package-upgrade (package)
  "Upgrade PACKAGE.

PACKAGE is a symbol, which should be a key in `git-package-alist'.

Checkout the :ref, fetch changes, and reinstall the package."
  (interactive (list (git-package--read-package "Upgrade git package: ")))
  (let* ((config (alist-get package git-package-alist))
         (default-directory (expand-file-name (plist-get config :dir)
                                              git-package-user-dir))
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
