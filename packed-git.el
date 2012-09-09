;;; packed-git.el --- Utilities for Emacs packages living in Git repositories

;; Copyright (C) 2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20120624
;; Version: 0.2.1
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

(eval-when-compile
  (require 'cl)) ; destructuring-bind ecase

(require 'magit)
(require 'packed)

(defun packed-git-library-p (commit file &optional package)
  "Return non-nil if FILE is an Emacs source library.
Actually provide the feature provided by FILE (which has to match
it's filename).

COMMIT has to be an existing commit in the current repository
and FILE has to exist in that commit."
  (with-temp-buffer
    (magit-git-insert (list "show" (concat commit ":" file)))
    (goto-char (point-min))
    (setq buffer-file-name file)
    (set-buffer-modified-p nil)
    (with-syntax-table emacs-lisp-mode-syntax-table
      (packed-library-p file package))))

(defun packed-git-libraries (repository commit &optional package)
  (let ((default-directory repository))
    (packed-git-libraries-1
     commit nil (or package (packed-filename repository)) t)))

(defun packed-git-libraries-1 (commit directory package &optional top-level)
  (let* ((regexp "^[0-9]\\{6\\} \\([^ ]+\\) [a-z0-9]\\{40\\}\t\\(.+\\)$")
         (objects
          (mapcar (lambda (line)
                    (string-match regexp line)
                    (list (match-string 2 line)
                          (intern (match-string 1 line))))
                  (magit-git-lines "ls-tree" "--full-tree"
                                   (concat commit ":" directory))))
         (searchp (not (assoc ".nosearch" objects)))
         files)
    (when (or top-level searchp)
      (dolist (object objects)
        (destructuring-bind (file type) object
          (setq file (concat (and directory (file-name-as-directory directory))
                             file))
          (ecase type
            (blob (and searchp
                       (packed-git-library-p commit file package)
                       (push file files)))
            (tree (unless (packed-ignore-directory-p file package)
                    (setq files (nconc files (packed-git-libraries-1
                                              commit file package)))))
            (commit))))
      (sort files 'string<))))

(provide 'packed-git)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; packed-git.el ends here
