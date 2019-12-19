;;; git-package-use-package.el --- Git support for use-package -*- lexical-binding: t -*-

;; Author: Matthew Sojourner Newton
;; Maintainer: Matthew Sojourner Newton
;; Version: 0.1
;; Package-Requires: ((emacs "24.3") (use-package "2.0"))
;; Requires: ((git "1.7.2.3"))
;; Homepage: https://github.com/mnewt/git-package
;; Keywords: config package git


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

;; This package adds support for installing git packages using `use-package'.

;; When the :git keyword is specified and :ensure is non-nil, the given package
;; will be cloned, byte compiled, and added to `load-path'.

;; For more information, see README.org.

;;; Code:

(require 'git-package)
(require 'use-package-ensure)



;;; Variables

(defvar git-package--previous-ensure-function nil
  "The previous value of `use-package-ensure-function'.")



;;; Functions

(defun git-package--dispatch-ensure (name args state &optional no-refresh)
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
          (git-package--normalize config name)
        (use-package-error ":git wants a string or a Plist with a :url key")))))

(defun use-package-handler/:git (name _keyword config rest state)
  "Simply return `body' of package NAME.

STATE is updated to tell `git-package--dispatch-ensure' that this
package is already ensured and does not need to dispatch to
`ensure'.

CONFIG is the PList containing the git configuration.

NAME and REST are passed to `use-package-process-keywords'."
  (let* ((state (plist-put state :ensured t))
         (body (use-package-process-keywords name rest state)))
    ;; We use the same logic as `use-package-handler/:ensure'.
    (if (bound-and-true-p byte-compile-current-file)
        ;; Eval when byte-compiling,
        (git-package-ensure config)
      ;; Or else wait until runtime.
      (push `(git-package-ensure ',config) body))
    body))

;;;###autoload
(defun git-package-setup-use-package ()
  "Enable the :git keyword in `use-package'."
  (add-to-list 'use-package-keywords :git)
  (setq git-package--previous-ensure-function use-package-ensure-function)
  (setq use-package-ensure-function #'git-package--dispatch-ensure)
  (make-directory git-package-user-dir t))

(provide 'git-package-use-package)

;;; git-package-use-package.el ends here
