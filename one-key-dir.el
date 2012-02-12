;;; one-key-dir.el --- Functions for quickly navigating directory trees with one-key menus

;; Filename: one-key-dir.el
;; Description: Functions for quickly navigating directory trees with one-key menus
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyright (C) 2010, Joe Bloggs, all rights reserved.
;; Created: 2010-09-21 17:23:00
;; Version: 0.1
;; Last-Updated: 2010-09-21 17:23:00
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/one-key-dir.el
;; Keywords: one-key, directories
;; Compatibility: GNU Emacs 24.0.50.1
;;
;; Features that might be required by this library:
;;
;; one-key.el
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary: 
;; 
;; Functions for quickly navigating directory trees with one-key menus
;; 
;; `one-key-dir-visit' can be used to navigate a directory tree and apply functions 
;;  to files. It's not an interactive function, so you should write wrapping functions
;;  yourself. You can refer to `one-key-dir/find-file' as an example, which can be
;;  used to navigate to and open a file using one-key.
;;
;; NOTE: hidden files and directories are excluded from the one-key menus generated, 
;; as are backup files (files whose names begin with "~"). This is to keep the number of
;; items in the one-key menus small enough so that enough keys can be generated.
;; Also note that if the number of items is large they may not all fit in the viewing area
;; of the one-key menu. In this case you can use the UP/DOWN arrow keys to scroll the menu 
;; up and down to view the rest of the items.

;;; Installation:
;;
;; Put one-key-dir.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'one-key-dir)

;;; Customize:
;; There is nothing to customize.

;;; TODO:
;; Make it more flexible so it can handle hidden and backup files and directories.
;; Allow different methods for allocating keys to menu items (e.g. using first unused letter of each item).

;;; Change log:
;;	
;; 2010/09/21
;;      * First released.
;; 

;;; Require
(require 'one-key)

;;; Code:

(defvar one-key-dir/max-lisp-eval-depth 2000
  "The `max-lisp-eval-depth' when using one-key-dir.el.
Because one-key related functions don't exit until the one-key menu buffer is killed. 
Setting this to a large number can avoid error of `Lisp nesting exceeds max-lisp-eval-depth")

(defvar one-key-dir-current-filename nil
  "Current file's name which is visited by one-key.")

(defvar one-key-dir-current-dir nil
  "Current directory which is visited by one-key.")

(defgroup one-key-dir nil
  "Filesystem navigation using `one-key'."
  :group 'one-key)

(defcustom one-key-dir-back-to-topdir-key "^"
  "Keybinding that will be used to return to the parent directory."
  :group 'one-key-dir
  :type 'string)

(defcustom one-key-dir-current-directory-key "."
  "Keybinding that will be used to open the current directory."
  :group 'one-key-dir
  :type 'string)

(defcustom one-key-dir/topdir "~/"
  "The fixed top level dir that `one-key-dir-visit' can explore the subdirs of,
but can't go above this dir."
  :group 'one-key-dir
  :type 'directory)

(defconst one-key-dir/alphabets-and-numbers
  (let (alphabets-and-numbers)
    (dotimes (i 26)
      (push (- ?Z i) alphabets-and-numbers))
    (dotimes (i 26)
      (push (- ?z i) alphabets-and-numbers))
    (dotimes (i 10)
      (push (- ?9 i) alphabets-and-numbers))
    alphabets-and-numbers)
  "A list contains characters [0-9a-zA-Z].
This list will be used when generating keys in `one-key-dir/generate-key'")

(defun one-key-dir-visit (dir func &optional dirfunc filename-map-func)
  "Visit DIR using one-key.
For each sub-dir of DIR, the associated command will be `(one-key-dir-visit sub-dir func)',
for each file under DIR, the associated command will be `(funcall func)' (so func should not require any arguments).
In FUNC, `one-key-dir-current-filename' can be used to do operations on the current file.
The optional FILENAME-MAP-FUNC specifies a function to be called on each file name,
it has one argument (string), the original file name, and returns a string, the
new file name which will be displayed in the one-key menu.
DIR should either be `one-key-dir/topdir' or a directory under `one-key-dir/topdir' 
in the directory tree."
  (unless (file-directory-p dir)
    (error "one-key-dir-visit called with a non-directory"))
  (unless (functionp func)
    (error "one-key-dir-visit called with a non-function."))
  (unless (one-key-dir/legal-dir-p dir)
    (error "one-key-dir-visit called with an illegal directory."))
  (setq one-key-dir-current-dir dir)
  (let ((old-max-lisp-eval-depth max-lisp-eval-depth))
    (setq max-lisp-eval-depth one-key-dir/max-lisp-eval-depth)
    (unwind-protect
	(let* ((dir-name (file-name-as-directory (file-truename dir)))
	       (key-name-list (one-key-dir/build-key-name-list dir (not (one-key-dir/descendant-p dir))))
	       (one-key-menu-dir/dir-alist (one-key-dir/build-menu-alist
					    key-name-list
					    func
                                            dirfunc
                                            filename-map-func)))
          (one-key-menu (concat dir-name "\n") one-key-menu-dir/dir-alist)))
    (setq max-lisp-eval-depth old-max-lisp-eval-depth)))

(defun one-key-dir/build-menu-alist (key-name-list func &optional (dirfunc (lambda nil
                                                                             (interactive)
                                                                             (find-file one-key-dir-current-filename)))
                                                   filename-map-func)
  "Return the menu alist that will be used by one-key.
KEY-NAME-LIST is generated by `one-key-dir/build-key-name-list'. FUNC is the function that will be applied to normal files, and DIRFUNC is the function that will be applied to directories.
FILENAME-MAP-FUNC is a function that transforms filenames in KEY-NAME-LIST to menu item descriptions."
  (let (menu-alist)
    (dolist (key-name key-name-list)
      (case (fourth key-name)
        (1 (push (cons (cons (first key-name) (second key-name))
                       `(lambda nil
                          (interactive)
                          (one-key-dir-visit (third ',key-name) ,func ,dirfunc ,filename-map-func)))
                 menu-alist))
        (2 (push (cons (cons (first key-name)
                             (if filename-map-func
                                 (funcall filename-map-func (second key-name))
                               (second key-name)))
                       `(lambda nil
                          (interactive)
                          (setq one-key-dir-current-filename (concat (third ',key-name) (second ',key-name)))
                          (funcall ,func)))
                 menu-alist))
        (0 (push (cons (cons (first key-name) (second key-name))
                       `(lambda nil
                          (interactive)
                          (setq one-key-dir-current-filename (concat (third ',key-name) (second ',key-name)))
                          (funcall ,dirfunc)))
                 menu-alist))))
    menu-alist))

(defun one-key-dir/build-key-name-list (dir &optional dont-show-parent)
  "Build the key name list for directory DIR.
Each element of the returned list has the following form: (KEY FILE-NAME FULLPATH TYPE)
Where KEY is the key, FILE-NAME is the name to display in the menu, FULLPATH is the full path to the corresponding directory, and TYPE indicates the type of items: 0 for the current directory, 1 for other directories, and 2 for files.
If optional DONT-SHOW-PARENT is non-nil, there will not be a `one-key-dir-back-to-topdir-key' \"..\" item."
  (unless (file-directory-p dir)
    (error "one-key-dir/build-key-name-list called with a non-directory."))
  (let* ((dirname (file-name-as-directory (file-truename dir)))
	 (subdirs (mapcar #'file-name-nondirectory (one-key-dir/subdirs dir)))
	 (files (mapcar #'file-name-nondirectory (one-key-dir/subdirs dir t)))
	 (usedkeys (if dont-show-parent '("q") `(,one-key-dir-back-to-topdir-key "q")))
	 (key-name-list nil))
    ;; Push the `one-key-dir-back-to-topdir-key'
    (unless dont-show-parent
      (push `(,one-key-dir-back-to-topdir-key ".." ,(expand-file-name ".." dirname) 1) key-name-list))
    ;; Push menu item for opening current directory
    (push `(,one-key-dir-current-directory-key "." ,dirname 0) key-name-list)
    ;; build key for subdirs
    (dolist (subdir subdirs)
      (let ((key (one-key-dir/generate-key subdir usedkeys)))
	(push `(,key ,(concat subdir "/") ,(concat (file-name-as-directory dirname) subdir) 1) key-name-list)
	(push key usedkeys)))
    ;; build key for files
    (dolist (file files)
      (let ((key (one-key-dir/generate-key file usedkeys)))
	(push `(,key ,(file-name-nondirectory (file-truename (concat dirname file))) ,dirname 2) key-name-list)
	(push key usedkeys)))
    key-name-list))

(defun one-key-dir/generate-key (file-name usedkeys)
  "Return the generated key for file named FILE-NAME.
The generated key will be used in the one-key menu. FILE-NAME is a string.
USEDKEYS contains all the already used keys."
  (or (dolist (element one-key-dir/alphabets-and-numbers)
        (let ((normal-key (char-to-string element)))
          (when (one-key-dir/key-not-used normal-key usedkeys)
            (return normal-key))))
      (error "Can not generate a unique key for file : %s" file-name)))

(defun one-key-dir/key-not-used (key key-name-list)
  "Return t if KEY is not used in KEY-NAME-LIST."
  (dolist (key-name key-name-list t)
    (if (string= key key-name)
	(return nil))))

(defun one-key-dir/subdirs (directory &optional file?)
  "Return subdirs or files of DIRECTORY according to FILE?.
If file? is t then return files, otherwise return directories.
Hidden and backup files and directories are not included."
  (remove-if (lambda (file)
               (or (string-match "^\\." (file-name-nondirectory file))
                   (string-match "~$" (file-name-nondirectory file))
                   (if file? (file-directory-p file)
                     (not (file-directory-p file)))))
             (directory-files directory t)))

(defun one-key-dir/legal-dir-p (dir)
  "Return t if DIR is `one-key-dir/topdir' or a descendant of `one-key-dir/topdir'."
  (or (string= (file-name-as-directory (file-truename dir))
	       (file-name-as-directory (file-truename one-key-dir/topdir)))
      (one-key-dir/descendant-p dir)))

(defun one-key-dir/descendant-p (dir)
  "Return t if DIR is a descendant of `one-key-dir/topdir'."
  (let ((topdir-name (file-name-as-directory (file-truename one-key-dir/topdir)))
	(dir-name (file-name-as-directory (file-truename dir))))
    (and (not (string= topdir-name dir-name))
	 (= (- (abs (compare-strings topdir-name 0 nil dir-name 0 nil)) 1) (length topdir-name)))))
;	 (string-prefix-p topdir-name dir-name))))

;; Here is an example of how to use one-key-dir-visit:
(defun one-key-dir/find-file (topdir)
  "Use one-key-dir-visit to navigate directories and then visit the selected file."
  (interactive (list (read-directory-name "Directory for root of tree: " default-directory)))
  (one-key-dir-visit topdir (lambda () (interactive) (find-file one-key-dir-current-filename))))


(provide 'one-key-dir)
;;; one-key-dir.el ends here
