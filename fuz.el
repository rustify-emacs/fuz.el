;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'macroexp)
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
  "Sort LIST with COMP-FN, transfrom elem with KEY first."
  (sort list (lambda (e1 e2)
               (funcall comp-fn
                        (funcall key e1)
                        (funcall key e2)))))

(defmacro fuz-with-hash-cache-progn (cache key &rest body)
  "Eval BODY, cache it in hash-table CACHE with KEY."
  (declare (indent 2) (debug t))
  (macroexp-let2* macroexp-copyable-p ((cache cache)
                                       (key key))
    (let ((val (make-symbol "val"))
          (not-found-sym (make-symbol "not-found-sym")))
      `(let* ((,not-found-sym (make-symbol "not-found"))
              (,val (gethash ,key ,cache ,not-found-sym)))
         (if (eq ,val ,not-found-sym)
             (puthash ,key ,(macroexp-progn body) ,cache)
           ,val)))))

;;; Export function aliases

(defalias 'fuz-calc-score-clangd 'fuz-core-calc-score-clangd)
(defalias 'fuz-calc-score-skim 'fuz-core-calc-score-skim)
(defalias 'fuz-find-indices-clangd 'fuz-core-find-indices-clangd)
(defalias 'fuz-find-indices-skim 'fuz-core-find-indices-skim)

(provide 'fuz)

;;; fuz.el ends here
