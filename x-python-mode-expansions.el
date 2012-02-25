;;; x-python-mode-expansions.el --- python-mode.el expansions for expand-region

;; Copyright (C) 2012 Ed McCardell

;; Author: Ed McCardell
;; Based on python-mode-expansions by: Ivan Andrus <darthandrus@gmail.com>
;; Keywords: marking region

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is for python-mode.el <https://launchpad.net/python-mode>,
;; not the built-in python.el.

;;; Code:

(defun er/add-x-python-mode-expansions ()
  "Adds Python-specific expansions for buffers in python-mode"
  (set (make-local-variable 'er/try-expand-list)
       (append
        er/try-expand-list
        '(py-mark-expression
          py-mark-statement
          py-mark-clause
          py-mark-def
          py-mark-block))))

(eval-after-load "python-mode"
  '(add-hook 'python-mode-hook 'er/add-x-python-mode-expansions))

(provide 'x-python-mode-expansions)

;; x-python-mode-expansions.el ends here
