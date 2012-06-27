;;; packed.el --- Emacs package utilities

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

;; This is a beta release.  Version numbers are inspired by how
;; Emacs is versioned - 1.1.0 will be the first stable version.

;; Packed provides some package manager agnostic utilities to work with
;; packages.  As far as Packed is concerned packages are collections of
;; Emacs Lisp libraries that are stored in a dedicated directory such as
;; a vcs repository.

;; TODO generally Packed still has to become much smarter
;; TODO assert that auto-save and backup files are always ignored
;; TODO handle disappearing files gracefully
;; TODO implement substitutes for `byte-recompile-directory'

;;; Code:

(require 'autoload)
(require 'bytecomp)


;;; Options.

(defgroup packed nil
  "Emacs package utilities."
  :group 'convenience
  :prefix 'packed)

(defcustom packed-loaddefs-filename
  '("loaddefs.el" ".loaddefs.el")
  "Name of the files used to store extracted autoload definitions.
Can also be a list of such names."
  :group 'packed
  :type '(choice file (repeat file)))


;;; Files.

(defun packed-el-suffixes (&optional nosuffix must-suffix)
  "Return a list of the valid suffixes of Emacs libraries.
Unlike `get-load-suffixes' don't return the suffixes for byte-compile
destinations just those of Emacs source files.

If NOSUFFIX is non-nil the `.el' part is omitted.
IF MUST-SUFFIX is non-nil all returned suffixes contain `.el'.
This uses the variables `load-suffixes' and `load-file-rep-suffixes'."
  (append (unless nosuffix
            (let ((load-suffixes (remove ".elc" load-suffixes)))
              (get-load-suffixes)))
          (unless must-suffix
            load-file-rep-suffixes)))

(defun packed-el-regexp ()
  "Return the valid suffixes of Emacs libraries as a regular expression."
  (concat (regexp-opt (packed-el-suffixes nil t)) "\\'"))

(defun packed-source-file-p (file)
  "Return non-nil if FILE is an Emacs source file.
More precisely return non-nil if FILE has an appropriate suffix.
Also see `packed-library-p' which is more restrictive."
  (and (save-match-data (string-match (packed-el-regexp) file))
       (not (backup-file-name-p file))
       (not (auto-save-file-name-p file))))

(defun packed-source-file (elc)
  "Return the Emacs source file for byte-compile destination ELC."
  (let ((standard (concat (file-name-sans-extension
                           (file-name-sans-extension elc)) ".el"))
        (suffixes (remove ".el" (packed-el-suffixes)))
        file)
    (while (and (not file) suffixes)
      (unless (file-exists-p (setq file (concat standard (pop suffixes))))
        (setq file nil)))
    (or file standard)))

(defvar packed-ignore-file-regexp
  "\\(^\\.\\|-pkg\\.el\\)")

(defun packed-ignore-file-p (file)
  (string-match packed-ignore-file-regexp
                (file-name-nondirectory file)))

(defvar packed-ignore-directory-regexp
  "\\(^\\.\\|^t\\(est\\(ing\\)?\\)?$\\)")

(defun packed-ignore-directory-p (directory)
  (string-match packed-ignore-directory-regexp
                (file-name-nondirectory
                 (directory-file-name directory))))


;;; Libraries.

;; FIXME return nil for hidden and backup files, and similar
(defun packed-library-p (file &optional raw)
  "Return non-nil if FILE is an Emacs source library."
  (and (packed-source-file-p file)
       (or raw (not (packed-ignore-file-p file)))))

(defun packed-locate-library (library &optional nosuffix path interactive-call)
  "Show the precise file name of Emacs library LIBRARY.
Unlike `locate-library' don't return the byte-compile destination
if it exists but always the Emacs source file.

LIBRARY should be a relative file name of the library, a string.
It can omit the suffix (a.k.a. file-name extension) if NOSUFFIX is
nil (which is the default, see below).
This command searches the directories in `load-path' like `\\[load-library]'
to find the file that `\\[load-library] RET LIBRARY RET' would load.
Optional second arg NOSUFFIX non-nil means don't add suffixes `load-suffixes'
to the specified name LIBRARY.

If the optional third arg PATH is specified, that list of directories
is used instead of `load-path'.

When called from a program, the file name is normally returned as a
string.  When run interactively, the argument INTERACTIVE-CALL is t,
and the file name is displayed in the echo area."
  (interactive (list (completing-read "Locate library: "
                                      (apply-partially
                                       'locate-file-completion-table
                                       load-path (get-load-suffixes)))
                     nil nil
                     t))
  (let ((file (locate-file (substitute-in-file-name library)
                           (or path load-path)
                           (packed-el-suffixes nosuffix))))
    (if interactive-call
        (if file
            (message "Library is file %s" (abbreviate-file-name file))
          (message "No library %s in search path" library)))
    file))

;; FIXME this still returns to much and generally has to be much smarter
(defun packed-libraries (directory &optional raw)
  "Return a list of libraries in the package directory DIRECTORY.
DIRECTORY is assumed to contain the libraries belonging to a single
package.  Some assumptions are made about what directories and what
files should be ignored."
  (let (libraries)
    (dolist (f (directory-files directory nil "^[^.]"))
      (cond ((file-directory-p f)
             (setq libraries (nconc (packed-libraries f raw))))
            ((packed-library-p f)
             (push f libraries))))
    (sort libraries 'string<)))

(defun packed-libraries-git (repository revision &optional raw)
  (let ((default-directory repository))
    (packed-libraries-git-1 revision raw)))

(defun packed-libraries-git-1 (revision &optional raw)
  (require 'magit)
  (mapcan
   (lambda (f)
     (when (with-temp-buffer
             (magit-git-insert (list "show" (concat revision ":" f)))
             (goto-char (point-min))
             (setq buffer-file-name f)
             (set-buffer-modified-p nil)
             (with-syntax-table emacs-lisp-mode-syntax-table
               (packed-library-p f raw)))
       (list f)))
   (magit-git-lines "ls-tree" "-r" "--name-only" revision)))

(defun packed-mainfile (directory &optional name noerror)
  (packed-mainfile-1 (or name (file-name-nondirectory
                               (directory-file-name directory)))
                     (packed-libraries directory)
                     noerror))

(defun packed-mainfile-1 (name libraries &optional noerror)
  (cond ((not (cdr libraries))
         (car libraries))
        ((packed-mainfile-2 name libraries))
        ((packed-mainfile-2
          (if (string-match "-mode$" name)
              (substring name 0 -5)
            (concat name "-mode"))
          libraries))
        (noerror
         nil)
        (t
         (error "Cannot determine mainfile of %s" name))))

(defun packed-mainfile-2 (name libraries)
  (car (member* (concat "^" (regexp-quote name) (packed-el-regexp) "$")
                libraries :test 'string-match :key 'file-name-nondirectory)))


;;; Load Path.

(defun packed-add-to-load-path (directory)
  (mapc (apply-partially 'add-to-list 'load-path)
        (packed-load-path directory)))

(defun packed-remove-from-load-path (directory)
  (mapc (lambda (path)
          (setq load-path (delete path load-path)))
        (packed-load-path directory)))

(defun packed-load-path (directory)
  (let (lp in-lp)
    (dolist (f (directory-files directory t))
      (cond ((member (file-name-nondirectory f) '("." "..")))
            ((file-regular-p f)
             (and (not in-lp)
                  (packed-library-p f)
                  (add-to-list 'lp (directory-file-name directory))
                  (setq in-lp t)))
            ((file-directory-p f)
             (and (not (packed-ignore-directory-p directory))
                  (setq lp (nconc (packed-load-path f) lp))))
            (t))) ; TODO don't just implicilty ignore strange files
    lp))


;;; TODO Byte Compile.
;;  TODO (defun packed-compile (directory))
;;  TODO (defun packed-recompile (directory &optional force))


;;; Autoloads.

;; FIXME the order in which files are tried can lead to surprising results;
;; either document implement something like locate-dominating-file*s"
(defun packed-loaddefs-file (&optional directory)
  (let ((candidates (if (listp packed-loaddefs-filename)
                        packed-loaddefs-filename
                      (list packed-loaddefs-filename)))
        found)
    (while (and (not found) candidates)
      (setq found (locate-dominating-file
                   (or directory default-directory)
                   (pop candidates))))
    found))

(defun packed-load-autoloads (&optional directory)
  (let ((file (packed-loaddefs-file directory)))
    (if file
        (load file)
      (message "Cannot locate loaddefs file for %s" directory))))

(defmacro packed-with-loaddefs (dest &rest body)
  (declare (indent 1))
  `(let ((generated-autoload-file dest)
         ;; Generating autoloads runs theses hooks; disable them.
         fundamental-mode-hook
         prog-mode-hook
         emacs-lisp-mode-hook)
     (prog2
         (unless (file-exists-p generated-autoload-file)
           (write-region
            (replace-regexp-in-string
             ";; no-byte-compile: t\n" ""
             (autoload-rubric generated-autoload-file))
            nil generated-autoload-file))
         (progn ,@body)
       (let (buf)
         (while (setq buf (find-buffer-visiting generated-autoload-file))
           (with-current-buffer buf
             (save-buffer)
             (kill-buffer)))))))

(defun packed-update-autoloads (dest path)
  (when (or dest (setq dest (packed-loaddefs-file)))
    (packed-with-loaddefs dest
      (update-directory-autoloads path)
      (byte-compile-file dest t))))

(defun packed-remove-autoloads (dest path)
  (when (or dest (setq dest (packed-loaddefs-file)))
    (packed-with-loaddefs dest
      ;; `autoload-find-destination' clears out autoloads associated
      ;; with a file if they are not found in the current buffer
      ;; anymore (which is the case here because it is empty).
      (with-temp-buffer
        (let ((autoload-modified-buffers (list (current-buffer))))
          (dolist (d path)
            (when (and (file-directory-p d)
                       (file-exists-p d))
              (dolist (f (directory-files d t (packed-el-regexp)))
                (autoload-find-destination f (autoload-file-load-name f)))))))
      (byte-compile-file dest t))))


;;; Features.

(defconst packed-provided-regexp "\
\(\\(?:cc-\\|silentcomp-\\)?provide[\s\t\n]+'\
\\([^(),\s\t\n]+\\)\\(?:[\s\t\n]+'\
\(\\([^(),]+\\))\\)?)")

(defun packed-provided ()
  (let (features)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward packed-provided-regexp nil t)
        (unless (save-match-data
                  (or (nth 3 (syntax-ppss))   ; in string
                      (nth 4 (syntax-ppss)))) ; in comment
          (dolist (feature (cons (match-string 1)
                                 (when (match-string 2)
                                   (split-string (match-string 2) " " t))))
            (add-to-list 'features (intern feature))))))
    features))

(defconst packed-required-regexp "\
\(\\(?:cc-\\)?require[\s\t\n]+'\
\\([^(),\s\t\n]+\\)\
\\(?:\\(?:[\s\t\n]+\\(?:nil\\|\".*\"\\)\\)\
\\(?:[\s\t\n]+\\(?:nil\\|\\(t\\)\\)\\)?\\)?)")

(defun packed-required ()
  (let (hard soft)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward packed-required-regexp nil t)
        (let ((feature (intern (match-string 1))))
          (cond ((save-match-data
                   (or (nth 3 (syntax-ppss))    ; in string
                       (nth 4 (syntax-ppss))))) ; in comment
                ((match-string 2)
                 (add-to-list 'soft feature))
                (t
                 (add-to-list 'hard feature))))))
    (list hard soft)))


;;; Info Pages.

(defun packed-info-path (directory)
  (let (ip in-ip)
    (dolist (f (directory-files directory t))
      (cond ((member (file-name-nondirectory f) '("." "..")))
            ((file-regular-p f)
             (and (not in-ip)
                  (string-match "\.info$" (file-name-nondirectory f))
                  (add-to-list 'ip (directory-file-name directory))
                  (setq in-ip t)))
            ((not (packed-ignore-directory-p directory))
             (setq ip (nconc (packed-info-path f) ip)))))
    ip))

(provide 'packed)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; packed.el ends here
