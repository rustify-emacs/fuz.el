;;; fuz-extra.el --- Extra convenient utils for Fuz  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Zhu Zihao

;; Author: Zhu Zihao <all_but_last@163.com>
;; URL:
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
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

;; This file provided a set of util functions which considered useful
;; but not the core part of `fuz'.

;;; Code:

(require 'cl-lib)
(require 'inline)
(require 'fuz)

(eval-when-compile
  (require 'pcase))

(defun fuz-highlighter (indices face str)
  "Put face on each position in INDICES of STR.

Sign: (-> (Listof Long) Sym Str Str)"
  (with-temp-buffer
    (insert (propertize str 'read-only nil))
    (goto-char (point-min))
    (dolist (it indices)
      (add-text-properties
       (+ 1 it) (+ 2 it) `(face ,face)))
    (buffer-string)))

(cl-defun fuz-logand-compose-match (fn patterns str)
  "

Sign: (-> (-> Str Str (Listof Long)) (Listof Str) Str (Option (Listof Long)))"
  (let ((total-score 0)
        all-indices)
    (dolist (it patterns)
      (pcase (funcall fn it str)
        (`(,score . ,indices)
          (setq all-indices (nconc all-indices indices))
          (setq total-score (+ total-score score)))
        (_
         (cl-return nil))))
    (cons total-score (sort (cl-delete-duplicates all-indices :test #'=) #'<))))

(define-inline fuz-sort-with-key! (list comp-fn key)
  "Sort LIST with COMP-FN, transfrom elem in LIST with KEY before comparison."
  (inline-letevals (key)
    (inline-quote
     (sort ,list (lambda (e1 e2)
                   (funcall ,comp-fn
                            (funcall ,key e1)
                            (funcall ,key e2)))))))

(defsubst fuz-memo-function (fn test)
  "Memoize the FN.

Sign: (All (I O) (-> (-> I O) (U 'eq 'eql 'equal) (-> I O)))

TEST can be one of `eq', `eql', `equal', which used as cache hash's test-fn."
  (let ((cache (make-hash-table :test test)))
    (lambda (input)
      (gethash input cache (puthash input (funcall fn input) cache)))))

(provide 'fuz-extra)

;; Local Variables:
;; coding: utf-8
;; End:

;;; fuz-extra.el ends here
