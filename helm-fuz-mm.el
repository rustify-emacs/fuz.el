;;; helm-fuz-mm.el --- Description  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Zhu Zihao 

;; Author: Zhu Zihao all_but_last@163.com
;; URL: 
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.0.50"))
;; Keywords: lisp

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'helm-fuz)

;;; Fuzzify multimatch

(defun helm-fuz--fuzzify-multimatch-get-score-data (pattern cand
                                                    &optional
                                                      use-real? basename?)
  "

Sign: (->* (Str Cand) (Bool Bool) (List Long Long))"
  (let* ((pats (helm-mm-split-pattern pattern))
         (raw-pats (thread-last pats
                     (cl-remove-if (lambda (it) (string-suffix-p "!" it)))
                     (mapcar (lambda (it) (string-remove-suffix
                                      "$"
                                      (string-remove-prefix "^" it))))
                     (mapcar (lambda (it) (helm-fuz--get-cand-str it
                                                             use-real? basename?)))))
         (realstr (helm-fuz--get-cand-str cand use-real? basename?)))
    (list (length realstr)
          (car (fuz-logand-compose-match #'helm-fuz--fuzzy-match raw-pats cand)))))

(defvar helm-fuz--fuzzy-regex-cache nil)
(defun helm-fuz--build-fuzzy-regex (pattern)
  "Build fuzzy regexp of PATTERN.

Sign: (-> Str (Cons Str Str))"
  (pcase-let ((`(,old-pat ,old-quick-re . ,old-full-re)
                helm-fuz--fuzzy-regex-cache))
    (if (string-prefix-p old-pat pattern)
        (cons old-quick-re
              (concat old-full-re
                      (helm--mapconcat-pattern (string-remove-prefix old-pat
                                                                     pattern))))
      (let ((re-cons (cons (helm--mapconcat-pattern (substring pattern 0 1))
                           (helm--mapconcat-pattern pattern))))
        (setq helm-fuz--fuzzy-regex-cache (cons pattern re-cons))
        re-cons))))

(defun helm-fuz--parse-mm-pattern (pattern)
  "Parse skim's style multimatch PATTERN.

Sign: (-> Str (U (Cons 'fuzzy (Cons Str Str)) (Cons 'inverse Str) Str))

Return value can be a regexp built from pattern , or (TYPE . REGEXP) to specify
match rule on pattern."
  (cond ((string-prefix-p "!" pattern)
         (cons 'inverse (regexp-quote (substring pattern 1))))
        ((string-prefix-p "^" pattern)
         (concat "^" (regexp-quote (substring pattern 1))))
        ((string-suffix-p "$" pattern)
         (concat (regexp-quote (substring pattern 0 (- (length pattern) 1)))
                 "$"))
        (t
         (cons 'fuzzy (helm-fuz--build-fuzzy-regex pattern)))))

(cl-defun helm-fuz-fuzzy-mm-match (str &optional (pattern helm-pattern))
  "

Sign: (-> Str Str Bool)"
  (let ((patterns (helm-mm-split-pattern pattern t)))
    (cl-every (lambda (it)
                (let ((pat (helm-fuz--parse-mm-pattern it)))
                  (pcase pat
                    (`(inverse . ,re)
                      (not (string-match-p re str)))
                    (`(fuzzy ,_quick-re . ,full-re)
                      (string-match-p full-re str))
                    (regex
                     (string-match-p regex str)))))
              patterns)))

(defun helm-fuz-fuzzy-mm-search (pattern &rest _)
  "In buffer search version of `helm-fuz-fuzzy-mm-match'

Sign: (-> Str &rest (Listof Any) Bool)"
  (let* ((parsed-pats (mapcar #'helm-fuz--parse-mm-pattern
                              (helm-mm-split-pattern pattern)))
         (pred (lambda (it) (eq (car-safe it) 'inverse)))
         (inverse-regexps (mapcar #'cdr (cl-remove-if-not pred parsed-pats)))
         (other-pats (cl-remove-if pred parsed-pats)))
    (cl-labels ((search-pat (pat bound quick?)
                  (pcase pat
                    (`(fuzzy ,quick-re . ,full-re)
                      (re-search-forward (if quick? quick-re full-re) bound t))
                    (regex
                     (re-search-forward regex bound t))))
                (rough-match (pat)
                  (search-pat pat nil t))
                (precise-match (pat bol eol)
                  (goto-char bol)
                  (search-pat pat eol nil)))
      (when (cl-some #'rough-match other-pats)
        (let* ((bol (point-at-bol))
               (eol (point-at-eol))
               (pred (lambda (it) (precise-match it bol eol))))
          (prog1 (if (cl-some pred inverse-regexps)
                     nil
                   (cl-every pred other-pats))
            (goto-char eol)))))))


(provide 'helm-fuz-mm)

;; Local Variables:
;; coding: utf-8
;; End:

;;; helm-fuz-mm.el ends here
