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

;; There is no need for a er/mark-python-defun since
;; er/mark-python-block will mark it

;; Feel free to contribute any other expansions for Python at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(require 'expand-region-core)

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

(defun er/add-python-mode-expansions ()
  "Adds Python-specific expansions for buffers in python-mode"
  (set (make-local-variable 'er/try-expand-list)
       (remove 'er/mark-inside-quotes
               (remove 'er/mark-outside-quotes
                       (append
                        er/try-expand-list
                        '(er/mark-python-statement
                          er/mark-inside-python-quotes
                          er/mark-outside-python-quotes
                          python-mark-block))))))

(add-hook 'python-mode-hook 'er/add-python-mode-expansions)

(provide 'python-mode-expansions)

;; python-mode-expansions.el ends here
