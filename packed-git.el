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

(defmacro packed-with-blob (commit file &rest body)
  (declare (indent 2))
  (let ((f (make-symbol "file")))
    `(let ((,f ,file))
       (with-temp-buffer
         (magit-with-silent-modifications
          (magit-git-insert
           (list "cat-file" "-p" (concat ,commit ":" ,f))))
         (setq buffer-file-name ,f)
         (packed-set-coding-system ,f)
         (set-buffer-modified-p nil)
         (goto-char (point-min))
         ,@body))))

(defun packed-set-coding-system (file)
  (save-excursion
    (goto-char (point-min))
    (let ((buffer-undo-list t)
	  (coding
	   (or coding-system-for-read
	       (and set-auto-coding-function
		    (save-excursion
		      (funcall set-auto-coding-function
			       file (- (point-max) (point-min)))))
	       ;; for dos-w32.el; see archive-set-buffer-as-visiting-file
	       (let ((file-name-handler-alist
		      '(("" . archive-file-name-handler))))
		 (car (find-operation-coding-system
		       'insert-file-contents
		       (cons file (current-buffer)) t))))))
      (unless (or coding-system-for-read
                  enable-multibyte-characters)
        (setq coding
              (coding-system-change-text-conversion coding 'raw-text)))
      (unless (memq coding '(nil no-conversion))
        (decode-coding-region (point-min) (point-max) coding)
	(setq last-coding-system-used coding))
      (set-buffer-modified-p nil)
      (kill-local-variable 'buffer-file-coding-system)
      (after-insert-file-set-coding (- (point-max) (point-min)))
      coding)))

(defun packed-git-library-p (commit file &optional package)
  "Return non-nil if FILE is an Emacs source library and part of package.
Actually return the feature provided by FILE.  For anything else
including bundled libraries return nil.

COMMIT has to be an existing commit in the current repository (as
determined using `default-directory') and FILE has to exist in
that commit.

See function `packed-library-p' for more information."
  (and (packed-library-name-p file package)
       (packed-with-blob commit file
         (packed-library-feature file))))

(defun packed-git-libraries (repository commit &optional package)
  "Return a list of libraries that are part of PACKAGE located in REPOSITORY.
REPOSITORY has to be a git repository and is assumed to contain
the libraries belonging to a single package.  COMMIT has to be an
existing commit in that repository.

See function `packed-libraries' for more information."
  (let ((default-directory repository))
    (packed-git-libraries-1
     commit nil (or package (packed-filename repository)) t)))

(defun packed-git-libraries-1 (commit directory package &optional top-level)
  "Return a list of Emacs lisp files in the package directory DIRECTORY.
DIRECTORY is assumed to contain the libraries belonging to a
single package.

COMMIT has to be an existing commit in the current repository
and FILE has to exist in that commit.

See function `packed-libraries-1' for more information."
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
