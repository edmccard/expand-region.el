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

(defun er/mark-inside-x-python-quotes ()
  "Mark the inside of the current string, not including the quotation marks."
  (interactive)
  (let ((beg (py-in-string-p)))
    (when beg
      (goto-char beg)
      ;; Skipping quote characters one-by-one will do the wrong thing
      ;; if a triple-quoted string ends with an escaped quote, e.g.
      ;; """The last word is \"quoted.\""""
      (let ((q-skip (if (looking-at "\"\"\"\\|'''") 3 1)))
        (forward-sexp)
        (backward-char q-skip)
        (set-mark (point))
        (goto-char beg)
        (forward-char q-skip)))))

(defun er/mark-outside-x-python-quotes ()
  "Mark the current string, including the quotation marks."
  (interactive)
  (let ((beg (py-in-string-p)))
    (when beg
      (goto-char beg)
      (set-mark (point))
      (forward-sexp)
      (exchange-point-and-mark))))

(defun er/mark-x-python-compound-statement ()
  "Mark the current compound statement (if, while, for, try) and all clauses."
  (interactive)
  (let ((secondary-re
         (save-excursion
           (py-mark-block-or-clause)
           (cond ((looking-at "if\\|for\\|while\\|else\\|elif") "else\\|elif")
                 ((looking-at "try\\|except\\|finally") "except\\|finally"))))
        start-col)
    (when secondary-re
      (py-mark-block-or-clause)
      (setq start-col (current-column))
      (while (looking-at secondary-re)
        (previous-line) (back-to-indentation)
        (while (> (current-column) start-col)
          (previous-line) (back-to-indentation)))
      (set-mark (point))
      (py-goto-beyond-clause) (next-line) (back-to-indentation)
      (while (and (looking-at secondary-re)
                  (>= (current-column) start-col))
        (py-goto-beyond-clause) (next-line) (back-to-indentation))
      (previous-line) (end-of-line)
      (exchange-point-and-mark))))

(defun er/add-x-python-mode-expansions ()
  "Adds Python-specific expansions for buffers in python-mode"
  (set (make-local-variable 'er/try-expand-list)
       (append
        (remove 'er/mark-inside-quotes
                (remove 'er/mark-outside-quotes er/try-expand-list))
        '(py-mark-expression
          er/mark-inside-x-python-quotes
          er/mark-outside-x-python-quotes
          py-mark-statement
          py-mark-clause
          er/mark-x-python-compound-statement
          py-mark-def
          py-mark-block))))

(eval-after-load "python-mode"
  '(add-hook 'python-mode-hook 'er/add-x-python-mode-expansions))

(provide 'x-python-mode-expansions)

;; x-python-mode-expansions.el ends here
