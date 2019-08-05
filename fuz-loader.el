;;; fuz-loader.el --- Dynamic module loader of fuz  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Zhu Zihao

;; Author: Zhu Zihao <all_but_last@163.com>
;; URL: https://github.com/cireu/fuz.el

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

;; A file that will forward to real dynamic module.
;; If dynamic module not found, will try to compile it.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defun fuz-build-and-load-dymod! ()
  "Build and load dyamic module."
  (unless (executable-find "cargo")
    (error "Rust package manager \"cargo\" not found!"))
  (let* ((default-directory (file-name-directory (locate-library "fuz")))
         (dll-name (cl-case system-type
                     ((windows-nt ms-dos cygwin) "fuz_core.dll")
                     (darwin "libfuz_core.dylib")
                     (t "libfuz_core.so")))
         (target-name (cl-case system-type
                        ((windows-nt ms-dos cygwin) "fuz-core.dll")
                        (t "fuz-core.so")))
         (dll-path (expand-file-name (format "target/release/%s" dll-name)))
         (target-path (expand-file-name target-name))
         (buf (generate-new-buffer "*fuz compilation*"))
         (move-file-fn (cl-case system-type
                         ;; Need root permission to make symlink on Windows 10
                         (windows-nt #'copy-file)
                         (t #'make-symbolic-link))))
    (message "Compiling the dynamic module of `fuz', please wait.")
    (pop-to-buffer buf)
    (let ((errno (call-process "cargo" nil buf t "build" "--release")))
      (if (= errno 0)
          (progn
            (funcall move-file-fn dll-path target-path)
            (load target-path nil t)
            (message "Successfully build dynamic module."))
        (error "Failed to compile dynamic modules, check buffer \"%s\" for detailed information."
               (buffer-name buf))))))

;; Silence byte-compiler

(declare-function fuz-core-calc-score-clangd "fuz-core")
(declare-function fuz-core-calc-score-skim "fuz-core")
(declare-function fuz-core-find-indices-clangd "fuz-core")
(declare-function fuz-core-find-indices-skim "fuz-core")

(cl-eval-when '(load eval)
  (unless (require 'fuz-core nil t)
    (fuz-build-and-load-dymod!)))

(provide 'fuz-loader)

;; Local Variables:
;; coding: utf-8
;; End:

;;; fuz-loader.el ends here
