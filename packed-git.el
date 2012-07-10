;;; packed-git.el --- Utilities for Emacs packages living in Git repositories

;; Copyright (C) 2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20120624
;; Version: 0.1.0
;; Status: beta
;; Homepage: http://tarsius.github.com/packed
;; Keywords: compile, convenience, lisp

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Utilities for Emacs package living in Git repositories.  These are
;; variants of the functions defined in `packed.el' but specialized for
;; Git repositories.

;;; Code:

(require 'magit)
(require 'packed)

(defun packed-git-library-p (commit file &optional package raw)
  (with-temp-buffer
    (magit-git-insert (list "show" (concat commit ":" file)))
    (goto-char (point-min))
    (setq buffer-file-name file)
    (set-buffer-modified-p nil)
    (with-syntax-table emacs-lisp-mode-syntax-table
      (packed-library-p file package raw))))

(defun packed-git-libraries (repository commit &optional package raw)
  (let ((default-directory repository))
    (packed-git-libraries-1
     commit nil (or package (packed-directory-package repository)) raw)))

(defun packed-git-libraries-1 (commit directory package &optional raw)
  (let ((objects
         (mapcar
          (lambda (line)
            (string-match
             (concat "^[0-9]\\{6\\} \\([^ ]+\\) "
                     "[a-z0-9]\\{40\\}\t\\(.+\\)$") line)
            (list (match-string 2 line)
                  (intern (match-string 1 line))))
          (magit-git-lines "ls-tree" "--full-tree"
                           (concat commit ":" directory)))))
    (unless (assoc ".nosearch" objects)
      (mapcan
       (lambda (object)
           (destructuring-bind (file type) object
           (setq file (concat (and directory (file-name-as-directory directory))
                              file))
           (ecase type
             (blob (when (packed-git-library-p commit file package raw)
                     (list file)))
             (tree (unless (packed-ignore-directory-p file package)
                     (packed-git-libraries-1 commit file package raw)))
             (commit)
             )))
       objects))))

(provide 'packed-git)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; packed-git.el ends here
