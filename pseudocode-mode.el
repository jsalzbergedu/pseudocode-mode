;;; pseudocode-mode.el --- A major mode for pseudocode -*- lexical-binding: t -*-

;; Copyright © 2019 Jacob Salzberg

;; Author: Jacob Salzberg <jssalzbe@ncsu.edu>
;; URL: https://github.com/jsalzbergedu/pseudocode-mode
;; Version: 0.1.0
;; Keywords: pseudocode
;; prefix: pseudocode-

;; This file is not a part of GNU Emacs

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

;; Example pseudocode syntax:

;; Algorithm doX(a, b)
;; Input a and b
;; Output does x
;; a <- 1
;; if b = 0 then
;;   doSomething(a, b)
;; return a * b

;; This program has a minor mode in which it matches blocks like this:
;;
;; /*
;;  * Algorithm doX(a, b)
;;  * Input a and b
;;  * Output does x
;;  * a <- 1
;;  * if b = 0 then
;;  *   doSomething(a, b)
;;  * return a * b
;;  */
;; That is, C style comments beginning with Algorithm.

;;; Code:

(defvar pseudocode-mode-highlights nil
  "Highlighted keywords in pseudocode mode.")

;; "[[:word:]]\\(?: <- [[:word:]]\\)"

(defconst pseudocode-keywords
  "Algorithm\\|Input\\|Output\\|<--\\|<-\\| if \\| then \\| else \\|NOT\\|AND\\| while \\| repeat \\| return "
  "Keywords in psuedocode.")

(defconst pseudocode-match-algorithm-name
  "\\(Algorithm \\)\\(.+\\)\\(?: ?(\\)"
  "Matches the algorithm name of the psuedocode")

(defconst pseudocode-match-algorithm-variable-declaration
  "\\(Algorithm [^(]+(\\)\\([^,]\\(, ?\\)\\)+\\([^)]\\))"
  "Matches variable names in the algoirhtm definition")

(defconst pseudocode-match-variable-declaration
  "\\(?:[^$_a-zA-Z]\\)\\([$_a-zA-Z][$_0-9a-zA-Z]*?\\) <--? .+$"
  "Matches a variable declaration elsewhere")

(setq pseudocode-mode-highlights
      `((,pseudocode-keywords . font-lock-keyword-face)
        (,pseudocode-match-algorithm-name 2 font-lock-function-name-face)
        (,pseudocode-match-algorithm-variable-declaration 2 font-lock-variable-name-face)
        (,pseudocode-match-algorithm-variable-declaration 4 font-lock-variable-name-face)
        (,pseudocode-match-variable-declaration 1 font-lock-variable-name-face)))

;;;###autoload
(define-derived-mode pseudocode-mode prog-mode "psuedocode"
  "A mode for editing and viewing psuedocode."
  (setq font-lock-defaults '(pseudocode-mode-highlights)))

;; code for finding comments and highlighting algorithms in them

(defconst pseudocode-algorithm-comment-matcher
  "/\\*[ \n\\*]*Algorithm [$_a-zA-Z][$_a-zA-Z0-9]*?(\\([$_a-zA-Z][$_a-zA-Z0-9]*,?\\)*)\\(.\\|\n\\)+?\\*/"
  "Matches comments with algorithms")

(defun pseudocode-re-noerr (regexp &optional bound count)
  (re-search-forward regexp bound t count))

(defun pseudocode-overlay-one-comment ()
  (with-silent-modifications
    (let ((case-fold-search nil))
      (when (pseudocode-re-noerr pseudocode-algorithm-comment-matcher nil)
        (let ((beg (match-beginning 0))
              (end (match-end 0))
              (varlist nil))
          (goto-char beg)
          (while (pseudocode-re-noerr pseudocode-keywords end)
            (let ((o (make-overlay (match-beginning 0) (match-end 0))))
              (overlay-put o 'pseudocode t)
              (overlay-put o 'face 'font-lock-keyword-face)))
          (goto-char beg)
          (when (pseudocode-re-noerr pseudocode-match-algorithm-name end)
            (let ((o (make-overlay (match-beginning 2) (match-end 2))))
              (overlay-put o 'pseudocode t)
              (overlay-put o 'face font-lock-function-name-face)))
          (goto-char beg)
          (pseudocode-re-noerr "Algorithm [^(]+?(" end)
          (let ((keep-going t))
            (while keep-going
              (let* ((param-list-beg (point))
                     (param-list-end param-list-beg))
                (while (not (or (= (char-after) ?,) (= (char-after) ?\))))
                  (forward-char)
                  (cl-incf param-list-end))
                (when (or (= (char-after) ?\)))
                  (setq keep-going nil))
                (forward-char)
                (when (not (equal param-list-beg param-list-end))
                  (let ((o (make-overlay param-list-beg param-list-end)))
                    (overlay-put o 'pseudocode t)
                    (overlay-put o 'face 'font-lock-variable-name-face))
                  (push (buffer-substring-no-properties param-list-beg param-list-end)
                        varlist)))))
          (goto-char beg)
          (while (pseudocode-re-noerr pseudocode-match-variable-declaration end)
            (let* ((var-beg (match-beginning 1))
                   (var-end (match-end 1))
                   (s (buffer-substring-no-properties var-beg var-end)))
              (when (not (member s varlist))
                (push s varlist)
                (let ((o (make-overlay (match-beginning 1) (match-end 1))))
                  (overlay-put o 'pseudocode t)
                  (overlay-put o 'face 'font-lock-variable-name-face)))))
          (goto-char end))
        t))))

(defun pseudocode--region (start end)
  ;; FIXME: This code goes over the whole buffer no matter what.
  ;; I couldn't figure out another way to do it.
  (setq start (point-min)
        end (point-max))
  (save-excursion
    (goto-char start)
    (while (and (< (point) end)
                (pseudocode-overlay-one-comment))
      t)))

;;;###autoload
(define-minor-mode pseudocode-comment-mode
  "A minor mode for highlighting algorithms in c style comments"
  nil
  "pseudocode-comments"
  nil
  (jit-lock-unregister #'pseudocode--region)
  (remove-overlays (point-min) (point-max) 'pseudocode t)
  (when pseudocode-comment-mode
    (jit-lock-register #'pseudocode--region t)
    (pseudocode--region (point-min) (point-max))))

(provide 'pseudocode-mode)
;;; pseudocode-mode.el ends here
