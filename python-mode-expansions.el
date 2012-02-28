;;; python-mode-expansions.el --- Python-specific expansions for expand-region

;; Copyright (C) 2012 Ivan Andrus, Ed McCardell

;; Author: Ivan Andrus, Ed McCardell
;; Based on js-mode-expansions by: Magnar Sveen <magnars@gmail.com>
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

;; This provides expansions for both the built-in python.el
;; and for python-mode.el from <https://launchpad.net/python-mode>.

;; Feel free to contribute any other expansions for Python at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(require 'expand-region-core)

(if (fboundp 'python-beginning-of-string)
    ;; python.el
    (defun er--python-string-start-pos ()
      "Returns character address of start of string, nil if not inside. "
      (let ((pt (point)))
        (save-excursion
          ;; python-beginning-of-string returns the current
          ;; position if point is not in a string, so we have
          ;; to check if point moved, and if not, we check if
          ;; we weren't already on the first quote of a string
          (python-beginning-of-string)
          (cond ((/= pt (point)) (point))
                ((looking-at "\"\\|'") (point))
                (t nil)))))
  ;; python-mode.el
  (defalias 'er--python-string-start-pos 'py-in-string-p))

(defun er/mark-inside-python-quotes ()
  "Mark the inside of the current string, not including the quotation marks."
  (interactive)
  (let ((beg (er--python-string-start-pos)))
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

(defun er/mark-outside-python-quotes ()
  "Mark the current string, including the quotation marks."
  (interactive)
  (let ((beg (er--python-string-start-pos)))
    (when beg
      (goto-char beg)
      (set-mark (point))
      (forward-sexp)
      (exchange-point-and-mark))))

(defun er/mark-python-statement ()
  "Marks one Python statement, eg. x = 3"
  (interactive)
  (python-end-of-statement)
  (set-mark (point))
  (python-beginning-of-statement))

(when (fboundp 'py-mark-block-or-clause)
  ;; Duplicating this for python.el would require a bit of work.
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
        (exchange-point-and-mark)))))

(defun er/add-python-mode-expansions ()
  "Adds Python-specific expansions for buffers in python-mode"
  (set (make-local-variable 'er/try-expand-list)
       (remove 'er/mark-inside-quotes
               (remove 'er/mark-outside-quotes
                       (append
                        er/try-expand-list
                        (if (fboundp 'python-mark-block)
                            ;; python.el
                            '(er/mark-python-statement
                              er/mark-inside-python-quotes
                              er/mark-outside-python-quotes
                              python-mark-block)
                          ;; python-mode.el
                          '(py-mark-expression
                            er/mark-inside-python-quotes
                            er/mark-outside-python-quotes
                            py-mark-statement
                            py-mark-clause
                            er/mark-x-python-compound-statement
                            py-mark-def
                            py-mark-block)))))))

(add-hook 'python-mode-hook 'er/add-python-mode-expansions)

(provide 'python-mode-expansions)

;; python-mode-expansions.el ends here
