;;; fuz.el --- Description  -*- lexical-binding: t -*-

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

;;

;;; Code:

(require 'cl-lib)
(require 'fuz-core)

(eval-when-compile
  (require 'subr-x)

  ;; Backward compatibility for Emacs 25
  (unless (>= emacs-major-version 26)
    (unless (fboundp 'if-let*) (defalias 'if-let* #'if-let))
    (unless (fboundp 'when-let*) (defalias 'when-let* #'when-let))))

(defsubst fuz-fuzzy-match-skim (pattern str)
  "

Sign: (-> Str Str (Option (Listof Long)))"
  (if-let* ((total-score (fuz-core-calc-score-skim pattern str)))
      (cons total-score (fuz-core-find-indices-skim pattern str))
    nil))

(defsubst fuz-fuzzy-match-clangd (pattern str)
  "

Sign: (-> Str Str (Option (Listof Long)))"
  (if-let* ((total-score (fuz-core-calc-score-clangd pattern str)))
      (cons total-score (fuz-core-find-indices-clangd pattern str))
    nil))


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

(defsubst fuz-sort-with-key! (list comp-fn key)
  "Sort LIST with COMP-FN, transfrom elem in LIST with KEY before comparison."
  (sort list (lambda (e1 e2)
               (funcall comp-fn
                        (funcall key e1)
                        (funcall key e2)))))

;; (defmacro fuz-with-hash-cache-progn (cache key &rest body)
;;   "Eval BODY, cache it in hash-table CACHE with KEY.

;; If KEY in CACHE, return its value, otherwise evaluates BODY and store it
;; in CACHE with KEY."
;;   (declare (indent 2) (debug t))
;;   (macroexp-let2* macroexp-copyable-p ((cache cache)
;;                                        (key key))
;;     (let ((val (make-symbol "val"))
;;           (not-found-sym (make-symbol "not-found-sym")))
;;       `(let* ((,not-found-sym (make-symbol "not-found"))
;;               (,val (gethash ,key ,cache ,not-found-sym)))
;;          (if (eq ,val ,not-found-sym)
;;              (puthash ,key ,(macroexp-progn body) ,cache)
;;            ,val)))))

(defun fuz-memo-function (fn test)
  "Memoize the FN.

Sign: (All (I O) (-> (-> I O) (U 'eq 'eql 'equal) (-> I O)))

TEST can be one of `eq' `equal' `eql', which used to compare the input
of FN and decide whether to get cached value or not."
  (let ((cache (make-hash-table :test test))
        (not-found-sym (make-symbol "not-found")))
    (lambda (input)
      (let ((val (gethash input cache not-found-sym)))
        (if (eq val not-found-sym)
            (puthash input (funcall fn input) cache)
          val)))))

;;; Export function aliases

(defalias 'fuz-calc-score-clangd 'fuz-core-calc-score-clangd)
(defalias 'fuz-calc-score-skim 'fuz-core-calc-score-skim)
(defalias 'fuz-find-indices-clangd 'fuz-core-find-indices-clangd)
(defalias 'fuz-find-indices-skim 'fuz-core-find-indices-skim)

(provide 'fuz)

;; Local Variables:
;; coding: utf-8
;; End:

;;; fuz.el ends here
