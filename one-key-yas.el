;;; one-key-yas.el --- functions for using one-key menus to access yasnippets

;; Filename: one-key-yas.el
;; Description: functions for using one-key menus to access yasnippets
;; Author:  Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyright (C) 2010, , all rights reserved.
;; Created: 2010-09-21 14:31:52
;; Version: 0.1
;; Last-Updated: 2010-09-21 14:31:52
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/one-key-yas.el
;; Keywords: yasnippet one-key snippet
;; Compatibility: GNU Emacs 24.0.50.1
;;
;; Features that might be required by this library:
;;
;; one-key.el (tested on version 0.6.9) yasnippet.el (tested on version 0.6.1c)
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
;; Functions for using one-key menus to access yasnippets.
;;
;; You can quickly access your yasnippets using one-key by binding a 
;; key-combo to `one-key-yas/show-mode-nr' or to one of the following 
;; commands:
;; `one-key-yas/show-dir' can be used to show a directory using
;; one-key. You can navigate the subdirs by pressing keys indicated 
;; in the menus. Pressing a key corresponding to a snippet file will
;; expand that snippet at point.
;; `one-key-yas/show-yas-root-directory' can be used to navigate
;; the `yas/root-directory' using one-key, and expand snippets
;; (it calls `one-key-yas/show-dir' on `yas/root-directory').
;; `one-key-yas/show-modes' can be used to show all the loaded
;; modes' snippet definitions using one-key.
;; `one-key-yas/show-mode' shows a one-key menu for snippets in the 
;; yasnippet directory of the current major mode and it's parents, 
;; but doesn't show any subdirs of that directory.
;; `one-key-yas/show-mode-nr' works like `one-key-yas/show-mode' but 
;; also shows menu items to navigate subdirs of the yasnippet directory
 
;;; Installation:
;;
;; Put one-key-yas.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add.
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'one-key-yas)

;;; Customize: 
;;
;; You may need to change the value of `yas/root-directory'
;; so that it matches the location of your snippets files.
;; This can be done by:
;;      M-x customize-variable RET yas/root-directory RET
;;

;;; Change log:
;;	
;; 2010/09/21
;;      * First released.
;; 

;;; Require
(require 'one-key)
(require 'yasnippet)

;;; Code:

(defvar one-key-yas/max-lisp-eval-depth 2000
  "The `max-lisp-eval-depth' when using one-key-yas.el.
Because one-key related functions don't exit until the one-key menu buffer is killed,
either because a snippet is inserted or an unknown keystroke was entered, setting this 
to a large number can avoid the error of `Lisp nesting exceeds max-lisp-eval-depth")

(defconst one-key-yas/alphabets-and-numbers
  (let (alphabets-and-numbers)
    (dotimes (i 26)
      (push (+ ?a i) alphabets-and-numbers)
      (push (+ ?A i) alphabets-and-numbers))
    (dotimes (i 10)
      (push (+ i ?0) alphabets-and-numbers))
    alphabets-and-numbers)
  "A list contains characters [a-zA-Z0-9].
This list is used when generating keys in `one-key-yas/generate-key'")

(defun one-key-yas/show-yas-root-directory ()
  "Show `yas/root-directory' using `one-key'.
If `yas/root-directory' is set to a list, only show the first directory"
  (interactive)
  (unless yas/root-directory
    (error "yas/root-directory is not set"))
  
  (if (listp yas/root-directory)
      (one-key-yas/show-dir (car yas/root-directory))
    (one-key-yas/show-dir yas/root-directory)))
    
(defun one-key-yas/show-dir (dir)
  "Show DIR's content in a one-key menu.
For directories, entering the corresponding menu key will navigate to that directory recursively.
For snippet files entering the corresponding menu key will expand the snippet."
  (interactive (list (ido-read-directory-name "Directory for root of tree: " default-directory)))  
  (unless (file-directory-p dir)
    (error "one-key-yas/show-dir called with a non-directory"))
  
  (let ((old-max-lisp-eval-depth max-lisp-eval-depth))
    (setq max-lisp-eval-depth one-key-yas/max-lisp-eval-depth)
    (unwind-protect
	(let* ((dir-name (file-name-as-directory (file-truename dir)))
	       (key-name-list (one-key-yas/build-key-name-list dir))
	       (one-key-menu-yas/dir-alist (one-key-yas/build-menu-alist key-name-list)))
	  (flet ((one-key-menu-yas-func ()
					(one-key-menu dir-name
						      one-key-menu-yas/dir-alist)))
	    (one-key-menu-yas-func)))
      (setq max-lisp-eval-depth old-max-lisp-eval-depth))
    (setq max-lisp-eval-depth old-max-lisp-eval-depth)))

(defun one-key-yas/show-modes ()
  "Show all the major modes that have snippets definition using `one-key'."
  (interactive)
  (let ((old-max-lisp-eval-depth max-lisp-eval-depth))
    (setq max-lisp-eval-depth one-key-yas/max-lisp-eval-depth)
    (unwind-protect
	(let* (mode-names
	       (dummy (maphash #'(lambda (key value)
				   (when (yas/real-mode? key)
				     (push key mode-names))) yas/snippet-tables))
	       (keys '("C-b" "q"))
	       (menu-alist nil))
	  
	  ;; build the key and menu alist
	  (dolist (mode-name mode-names)
	    (let ((key (one-key-yas/generate-key (symbol-name mode-name) keys)))
	      (push key keys)
	      (push (cons (cons key (symbol-name mode-name))
			  `(lambda () (interactive)
			     (one-key-yas/show-mode ',mode-name 'has-parent))) menu-alist)))
	  
	  (flet ((one-key-menu-yas-func ()
					(one-key-menu "All loaded modes in Yasnippet"
						      menu-alist)))
	    (one-key-menu-yas-func)))
      ;; if an error happens, restore the orignial value
      (setq max-lisp-eval-depth old-max-lisp-eval-depth))
    (setq max-lisp-eval-depth old-max-lisp-eval-depth)))

(defun one-key-yas/show-mode (&optional mode has-parent)
  "Show all the applicable snippets for mode MODE.
If optional MODE is nil, current buffer's major mode will be used.
If optional HAS-PARENT is non-nil, a menu item with key `C-b' will be added for showing
all snippet directories."
  (interactive)
  (or mode (setq mode major-mode))  
  (let* ((templates (yas/all-templates (one-key-yas/get-snippet-tables mode)))
	 (file-names (mapcar #'(lambda (template) (file-name-nondirectory (yas/template-file template)))
			     templates))
	 (keys '("C-b" "q"))
	 (key-name-list nil))
    
    (mapcar* #'(lambda (file-name template)
		 (let ((key (one-key-yas/generate-key file-name keys))
		       (snippet-def (yas/template-content template))
		       )
		   (push key keys)
		   (push `(,key ,(yas/template-name template) nil nil ,snippet-def) key-name-list)))
	     file-names templates)

    (let ((one-key-menu-yas/mode-alist (one-key-yas/build-menu-alist key-name-list)))      
      (when has-parent
	;; using "C-b" to return to top level.
	(setq one-key-menu-yas/mode-alist
	      (append one-key-menu-yas/mode-alist
		      (list (cons (cons "C-b" "Show all loaded snippets in modes")
				  (lambda () (interactive)
				    (one-key-yas/show-modes)))))))

      (flet ((one-key-menu-yas-func ()
				    (one-key-menu mode
						  one-key-menu-yas/mode-alist)))
	(one-key-menu-yas-func)))))

(defun one-key-yas/show-mode-nr (&optional mode)
 "Show the applicable snippets for mode MODE, and it's subdirectories.
If optional MODE is nil, current buffer's major mode will be used.
MODE's parent mode's snippets are also shown in the one-key menu."
  (interactive)
  (or mode (setq mode major-mode))
  (if (one-key-yas/get-snippet-tables mode)
      (let* ((templates (yas/all-templates (one-key-yas/get-snippet-tables mode)))
	     (full-file-names (mapcar #'(lambda (template) (yas/template-file template)) templates))
	     (mode-snippets-path (let ((path ""))
				   (dolist (file-name full-file-names)
				     (when (string-match (regexp-opt (list (concat "/" (symbol-name mode) "/"))) file-name)
				       (setq path (substring file-name 0 (match-beginning 0)))
				       (return)))
			       (if (equal path "") 
				   (concat yas/root-directory "/text-mode")
				 path)))
	     (parent-templates (remove nil
				       (mapcar #'(lambda (template)
						   (unless (string-match (concat "/" (regexp-opt `(,(symbol-name mode))) "/")
									 (yas/template-file template))
						     template))
					       templates)))
	     (parent-file-names (mapcar #'(lambda (template) (file-name-nondirectory (yas/template-file template)))
					parent-templates))
	     (key-name-list (one-key-yas/build-key-name-list
			     (file-name-as-directory (file-truename (concat mode-snippets-path
									    "/" (symbol-name mode) "/")))
			     'dont-show-parent-dir))
	     (keys (mapcar #'(lambda (key-name)
			       (car key-name))
			   key-name-list)))
	
	;; The parent mode's key-name-list
	(mapcar* #'(lambda (file-name template)
		     (let ((key (one-key-yas/generate-key file-name keys))
			   (snippet-def (yas/template-content template)))
		       (push key keys)
		       (push `(,key ,(yas/template-name template) nil nil ,snippet-def) key-name-list)))
		 parent-file-names parent-templates)

	(let ((one-key-menu-yas/mode-alist (one-key-yas/build-menu-alist key-name-list)))      
	  (flet ((one-key-menu-yas-func ()
					(one-key-menu (symbol-name mode)
						      one-key-menu-yas/mode-alist)))
	    (one-key-menu-yas-func))))
    (one-key-yas/show-modes)))

;;; Function that return an alist that can be used by one-key
;;; ((("c" . "c-mode/") . (lambda () (interactive) (one-key-yas/show-dir "")))
;;;  (("+" . "c++-mode/") . (lambda () (interactive) (one-key-yas/show-dir "")))
;;;  (("t" . "time") . (lambda () (interactive) (yas/expand-snippet ""))))
(defun one-key-yas/build-menu-alist (key-name-list)
  "Return the menu alist that will be used by one-key.
KEY-NAME-LIST is generated by `one-key-yas/build-key-name-list'"
  (let (menu-alist)
    (dolist (key-name key-name-list)
      (when (fourth key-name)		; A directory
	(push (cons (cons (first key-name) (second key-name))
		    `(lambda () (interactive)
		       (one-key-yas/show-dir (third ',key-name))))
	      menu-alist))
      
      (unless (fourth key-name)		; A snippet file
	(push (cons (cons (first key-name) (second key-name))
		    `(lambda () (interactive)
		       (yas/expand-snippet ,(fifth key-name))))
	      menu-alist)))
    menu-alist))

(defun one-key-yas/build-key-name-list (dir &optional dont-show-parent)
  "Build the key name list for directory DIR.
Each element of the returned list has the following form:
 (KEY DIR-NAME FULLPATH DIRP) or
 (KEY SNIPPET-NAME FULLPATH DIRP SNIPPET-TEMPLATE)
If optional DONT-SHOW-PARENT is non-nil, there will not be a
\"C-b\" \"Back to parent directory \" item."
  (unless (file-directory-p dir)
    (error "one-key-yas/build-key-name-list called with a non-directory"))
  
  (let* ((dir-name (file-name-as-directory (file-truename dir)))
	 (sub-dirs (mapcar #'file-name-nondirectory (yas/subdirs dir)))
	 (files (mapcar #'file-name-nondirectory (yas/subdirs dir t)))
	 (keys (if dont-show-parent '("q") '("C-b" "q"))) ; Initially, it contains some already used keybindings
	 (key-name-list nil))
    
    ;; build key for sub-dirs
    (dolist (sub-dir sub-dirs)
      (let ((key (one-key-yas/generate-key sub-dir keys)))
	    (push key keys)
	    (push `(,key ,(concat sub-dir "/")
			 ,(concat (file-name-as-directory dir-name) sub-dir)
			 t)
		  key-name-list)))

    ;; build keys for snippet files
    ;; If it is, instead of the file name, we use the snippet name
    ;; as the NAME part of the key-name-list
    ;; FIXME : Should we check whether this file is a snippet file? How?    
    (with-temp-buffer
      (dolist (file files)
	(let ((full-file (file-truename (concat dir-name file))))
	  (when (file-readable-p full-file)
	    (let ((key (one-key-yas/generate-key file keys)))	      
	      (push key keys)
	      (insert-file-contents full-file nil nil nil t)
	      (let ((snippet-def (yas/parse-template full-file)))
		(push `(,key ,(third snippet-def) ,dir-name nil ,(second snippet-def))
		      key-name-list)))))))
    
    ;;; Here we push the "C-b"
    ;; FIXME : for this we should only expand to the yas/root-directory
    ;; Or we use a variable `one-key-yas/restricted-to-root-directory'
    ;; By default it is t, which means don't go up to the root directory
    ;; otherwise, expand it until /
    (unless dont-show-parent
      (push `("C-b" ,(concat "Back to parent directory: "
			     (file-name-nondirectory (expand-file-name ".." dir)))
	      ,(expand-file-name ".." dir-name) t)
	    key-name-list))
    key-name-list))

;; The key part is how to compute the key from a name, the algorithm is
;; as follows :
;; "C-b" is left for returning to parent level,
;; "q" is left for quiting the one-key menu

;; For a name N (a string), we check it char by char
;; 1. If current char is not in key-name-list, we use this char as current
;; file/directory name's key.
;; 2. If all the characters are used for keys, then we re-search this name,
;; and use the revert-case of each character. If it is not used as a key,
;; use it. Otherwise,
;; 3. We add a prefix "C-", and use the downcase character as the key.
;; 4. We add a prefix "C-", and use the upcase character as the key.
;; 5. We add a prefix "M-", and use the downcase character as the key.
;; 6. We add a prefix "M-", and use the upcase character as the key.
;; 7. We use the characters [a-zA-Z0-9] as the key
;; 8. "C-" + [a-zA-Z0-9]
;; 9. "M-" + [a-zA-Z0-9]

;; Hopefully, the above 9 steps will generate unique keys for all the 
;; files/directories under a directory. If not, issue an error.

;; FIXME :
;; 1. Extract the generating key strategies as simple functions
;; 2. This function is not only used for the file-name, but also for the mode-name.
;;    So the argument name should be changed
(defun one-key-yas/generate-key (file-name keys)
  "Return the generated key for file named FILE-NAME.
The generated key will be used in the one-key menu.  FILE-NAME is a string.
KEYS contains all the already used keys."
  (let (key)
    (dotimes (idx (length file-name))
      (let ((char-key (char-to-string (aref file-name idx))))
	(when (one-key-yas/key-not-used char-key keys)
	  (setq key char-key)
	  (return))))
    
    (unless key
      ;; using character in original file isn't OK
      ;; Now try to revert the case
      ;; lower case=>upper case
      ;; upper case=>lower case
      (dotimes (idx (length file-name))
	(let ((case-char-key (char-to-string
			      (one-key-yas/revert-char-case
			       (aref file-name idx)))))
	  (when (one-key-yas/key-not-used case-char-key keys)
	    (setq key case-char-key)
	    (return)))))
    
    (unless key
      ;; using one character in FILE-NAME doesn't work, now try to use
      ;; "C-" as prefix (lower case file-name)
      (let ((l-file-name (downcase file-name)))
	(dotimes (idx (length l-file-name))
	  (let ((C-prefix-key (concat "C-"
				      (char-to-string (aref l-file-name idx)))))
	    (when (one-key-yas/key-not-used C-prefix-key keys)
	      (setq key C-prefix-key)
	      (return))))))
    
    ;; try "C-" as prefix and upcase characters
    (unless key
      (let ((u-file-name (upcase file-name)))
	(dotimes (idx (length u-file-name))
	  (let ((C-prefix-key (concat "C-"
				      (char-to-string (aref u-file-name idx)))))
	    (when (one-key-yas/key-not-used C-prefix-key keys)
	      (setq key C-prefix-key)
	      (return))))))

    ;; try "M-" as prefix and lower case characters
    (unless key
      (let ((l-file-name (downcase file-name)))
	(dotimes (idx (length l-file-name))
	  (let ((C-prefix-key (concat "M-"
				      (char-to-string (aref l-file-name idx)))))
	    (when (one-key-yas/key-not-used C-prefix-key keys)
	      (setq key C-prefix-key)
	      (return))))))

    ;; try "M-" as prefix and upcase characters
    (unless key
      (let ((l-file-name (upcase file-name)))
	(dotimes (idx (length l-file-name))
	  (let ((M-prefix-key (concat "M-"
				      (char-to-string (aref l-file-name idx)))))
	    (when (one-key-yas/key-not-used M-prefix-key keys)
	      (setq key M-prefix-key)
	      (return))))))
    
    ;; try using characters in one-key-yas/alphabets-and-numbers
    (unless key
      (dolist (element one-key-yas/alphabets-and-numbers)
	(let ((normal-key (char-to-string element)))
	  (when (one-key-yas/key-not-used normal-key keys)
	    (setq key normal-key)
	    (return)))))
    
    ;; try using "C-" + characters in one-key-yas/alphabets-and-numbers
    (unless key
      (dolist (element one-key-yas/alphabets-and-numbers)
	(let ((C-prefix-key (concat "C-" (char-to-string element))))
	  (when (one-key-yas/key-not-used C-prefix-key keys)
	    (setq key C-prefix-key)
	    (return)))))
    
    ;; try using "M-" + characters in one-key-yas/alphabets-and-numbers
    (unless key
      (dolist (element one-key-yas/alphabets-and-numbers)
	(let ((M-prefix-key (concat "M-" (char-to-string element))))
	  (when (one-key-yas/key-not-used M-prefix-key keys)
	    (setq key M-prefix-key)
	    (return)))))
    
    (unless key
      (error "Can not generate a unique key for file : %s" file-name))
    key))

(defun one-key-yas/key-not-used (key key-name-list)
  "Return t if KEY is not used in KEY-NAME-LIST."
  (dolist (key-name key-name-list t)
    (if (string= key key-name)
	(return nil))))

(defun one-key-yas/revert-char-case (char)
  "Revert character CHAR's case if it is in alphabet."
  (cond ((and (<= char ?z) (>= char ?a)) (upcase char))
	((and (<= char ?Z) (>= char ?A)) (downcase char))
	(t char)))

;;; Unlike the original version in Yasnippet `yas/get-snippet-tables'
;;; this function only uses mode-symbol to index the loaded tables
(defun one-key-yas/get-snippet-tables (mode-symbol)
  "Get snippet tables for mode MODE-SYMBOL.
Return a list of 'yas/snippet-table' objects indexed by mode."
  (let ((mode-tables
         (mapcar #'(lambda (mode)
                     (gethash mode yas/snippet-tables))
		 (list mode-symbol)))
        (all-tables))
    
    (dolist (table (remove nil mode-tables))
      (push table all-tables)
      (nconc all-tables (yas/snippet-table-get-all-parents table)))
    (remove-duplicates all-tables)))

(provide 'one-key-yas)
;;; one-key-yas.el ends here
