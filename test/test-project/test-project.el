;;; test-project.el --- Install Emacs packages via git -*- lexical-binding: t -*-

;; Copyright © 2020 Matthew Newton
;;
;; Author: One Person <one@example.com>
;; Maintainer: Other Person <other@example.com>
;; Created: 2020-01-31
;; Version: 0.99
;; Package-Requires: ((emacs "24.3") (some-package "0.98"))
;; Homepage: https://example.com/test/test-project
;; Keywords: test project

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

;; This is a mock project for testing git-package.


;;; Code:

;;;###autoload
(defun test-project-function ()
  "It's an autoloaded function."
  (message "hi"))

(provide 'test-project)

;;; test-project.el ends here
