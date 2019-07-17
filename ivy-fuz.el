;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'fuz)
(require 'ivy)

(defalias 'ivy-fuz-regex-fuzzy 'ivy--regex-fuzzy)

;; (defun ivy-fuz--sort-advice (orig-fun name candidates)
;;   (if (not (= ))))

(provide 'ivy-fuz)
;;; ivy-fuz.el ends here
