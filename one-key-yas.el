;;; one-key-yas.el --- functions for using one-key menus to access yasnippets

;; Filename: one-key-yas.el
;; Description: functions for using one-key menus to access yasnippets
;; Author:  Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyright (C) 2010, , all rights reserved.
;; Created: 2010-09-21 14:31:52
;; Version: 1.0
;; Last-Updated: 2012-08-02 18:30:52
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/one-key-yas.el
;; Keywords: yasnippet one-key snippet
;; Compatibility: GNU Emacs 24.1.1
;;
;; Features that might be required by this library:
;;
;; one-key.el (tested on version 1.0) yasnippet.el (tested on version 0.6.1b)
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
;; key-combo to `one-key-yas/get-mode-menu-alist-and-name-nr' or to one of the following 
;; commands:
;; `one-key-yas/show-dir' can be used to show a directory using
;; one-key. You can navigate the subdirs by pressing keys indicated 
;; in the menus. Pressing a key corresponding to a snippet file will
;; expand that snippet at point.
;; `one-key-yas/show-yas-root-directory' can be used to navigate
;; the `yas/root-directory' using one-key, and expand snippets
;; (it calls `one-key-yas/show-dir' on `yas/root-directory').
;; `one-key-yas/get-allmodes-menu-alist-and-name' can be used to show all the loaded
;; modes' snippet definitions using one-key.
;; `one-key-yas/get-mode-menu-alist-and-name' shows a one-key menu for snippets in the 
;; yasnippet directory of the current major mode and it's parents, 
;; but doesn't show any subdirs of that directory.
;; `one-key-yas/get-mode-menu-alist-and-name-nr' works like `one-key-yas/get-mode-menu-alist-and-name' but 
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

;;; TODO
;;
;; Add a one-key menu type for this type of menu

;;; Require
(require 'one-key)
(require 'yasnippet)

;;; Code:

(defvar one-key-yas/toplevel-item-description
  #("Show top level snippets directory" 0 32 (face (:background "cyan" :foreground "black")))
  "The item description for the toplevel item in one-key-yas menus.")

(defvar one-key-yas/allmodes-item-description
  #("Show all major-mode snippets menus" 0 33 (face (:background "cyan" :foreground "black")))
  "The item description for the allmodes item in one-key-yas menus.")

(defvar one-key-yas/updir-item-description
  #(".." 0 1 (face (:background "cyan" :foreground "black")))
  "The item description for navigating up directories in one-key-yas menus.")

(defvar one-key-yas/up-key "^"
  "Key description of the key to use for toplevel or allmodes items in one-key-yas menus.")

(defun one-key-yas/show-yas-root-directory nil
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
  (unless (file-directory-p dir)
    (error "one-key-yas/show-dir called with a non-directory"))
  (let* ((dir-name (file-name-as-directory (file-truename dir)))
         (rootdir (if (listp yas/root-directory) (car yas/root-directory)
                    yas/root-directory))
         (rootdirname (file-name-as-directory (file-truename rootdir)))
         (isroot (equal dir-name rootdirname))
         (key-name-list (one-key-yas/build-key-name-list dir isroot))
         (menu-alist (one-key-yas/build-menu-alist key-name-list))
         (menuname (if isroot "yasnippet:rootdir"
                     (concat "yasnippet:" (file-name-nondirectory (substring dir-name 0 -1))))))
      (one-key-open-submenu menuname menu-alist)))

(defun one-key-yas/show-rootdir nil
  "Return a name and one-key menu-alist for the `yas/root-directory'.
The name returned will be set to \"yasnippet:rootdir\" and the return value will be in the form
 (\"yasnippet:rootdir\". menu-alist)."
  (unless yas/root-directory
    (error "yas/root-directory is not set"))
  (one-key-yas/show-dir yas/root-directory))
  
(defun one-key-yas/get-allmodes-menu-alist-and-name nil
  "Return a name and one-key menu-alist of submenus for major-modes which have associated snippets.
The name returned will be set to \"yasnippet:allmodes\" and the return value will be in the form
 (\"yasnippet:allmodes\". menu-alist)."
  (let* ((keys (list one-key-yas/up-key))
         modes menu-alist)
    (maphash #'(lambda (key value)
                 (when (yas/real-mode? key)
                   (push key modes))) yas/snippet-tables)
    ;; build the key and menu alist
    (dolist (mode modes)
      (let* ((modename (symbol-name mode))
             (key (one-key-generate-key modename keys)))
        (push key keys)
        (push (cons (cons key modename)
                    `(lambda nil (interactive)
                       (let ((modepair (one-key-yas/get-mode-menu-alist-and-name-nr ',mode)))
                         (one-key-open-submenu (car modepair) (cdr modepair))))) menu-alist)))
    (push (cons (cons one-key-yas/up-key one-key-yas/toplevel-item-description)
                (lambda nil (interactive)
                  (one-key-yas/show-rootdir)))
          menu-alist)
  (cons "yasnippet:allmodes" menu-alist)))

(defun one-key-yas/get-mode-menu-alist-and-name (mode)
  "Return a name and one-key menu-alist of snippets for major-mode MODE (a major-mode symbol).
Result is returned as a cons cell in the form (name . menu-alist)."
  (or mode (setq mode major-mode))  
  (let* ((templates (yas/all-templates (one-key-yas/get-snippet-tables mode)))
	 (file-names (mapcar #'(lambda (template) (file-name-nondirectory (yas/template-file template)))
			     templates))
	 (keys (list one-key-yas/up-key))
	 key-name-list one-key-menu-yas/mode-alist)
    (mapcar* #'(lambda (file-name template)
		 (let ((key (one-key-generate-key file-name keys))
		       (snippet-def (yas/template-content template)))
		   (push key keys)
		   (push `(,key ,(yas/template-name template) nil nil ,snippet-def) key-name-list)))
	     file-names templates)
    (setq one-key-menu-yas/mode-alist (one-key-yas/build-menu-alist key-name-list))
    ;; using one-key-yas/up-key to return to top level.
    (push (cons (cons one-key-yas/up-key one-key-yas/allmodes-item-description)
                (lambda nil (interactive)
                  (let ((modespair (one-key-yas/get-allmodes-menu-alist-and-name)))
                    (one-key-open-submenu (car modespair) (cdr modespair)))))
          one-key-menu-yas/mode-alist)
    (cons (concat "yasnippet:" (symbol-name mode)) one-key-menu-yas/mode-alist)))

(defun one-key-yas/get-mode-menu-alist-and-name-nr (mode &optional has-parent)
 "Return a name and one-key menu-alist of snippets for major-mode MODE.
MODE's parent mode's snippets are also shown in the one-key menu.
Result is returned as a cons cell in the form (name . menu-alist).
If optional HAS-PARENT is non-nil, a menu item with key `^' will be added for showing all snippet mode directories.

If there are no snippets associated with the current major-mode a menu of menus for all supported
major-modes will be returned."
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
	     (parent-templates
              (remove nil
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
	     (keys (mapcar #'(lambda (key-name) (car key-name)) key-name-list))
             menu-alist)
	;; The parent mode's key-name-list
	(mapcar* #'(lambda (file-name template)
		     (let ((key (one-key-generate-key file-name keys))
			   (snippet-def (yas/template-content template)))
		       (push key keys)
		       (push `(,key ,(yas/template-name template) nil nil ,snippet-def) key-name-list)))
		 parent-file-names parent-templates)
        (setq menu-alist (one-key-yas/build-menu-alist key-name-list))
        (push (cons (cons one-key-yas/up-key one-key-yas/allmodes-item-description)
                    (lambda nil (interactive)
                      (let ((modespair (one-key-yas/get-allmodes-menu-alist-and-name)))
                        (one-key-open-submenu (car modespair) (cdr modespair))))) menu-alist)
        (cons (concat "yasnippet:" (symbol-name mode))
              menu-alist))
    (one-key-yas/get-allmodes-menu-alist-and-name)))

;;; Function that return an alist that can be used by one-key
;;; ((("c" . "c-mode/") . (lambda nil (interactive) (one-key-yas/show-dir "")))
;;;  (("+" . "c++-mode/") . (lambda nil (interactive) (one-key-yas/show-dir "")))
;;;  (("t" . "time") . (lambda nil (interactive) (yas/expand-snippet ""))))
(defun one-key-yas/build-menu-alist (key-name-list)
  "Return the menu alist that will be used by one-key.
KEY-NAME-LIST is generated by `one-key-yas/build-key-name-list'"
  (let (menu-alist)
    (dolist (key-name key-name-list)
      (when (fourth key-name)		; A directory
	(push (cons (cons (first key-name) (second key-name))
		    `(lambda nil (interactive)
		       (one-key-yas/show-dir ,(third key-name))))
	      menu-alist))
      (unless (fourth key-name)		; A snippet file
	(push (cons (cons (first key-name) (second key-name))
		    `(lambda nil (interactive)
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
	 (keys (if dont-show-parent nil (list one-key-yas/up-key))) ; Initially, it contains some already used keybindings
	 (key-name-list nil))
    (unless dont-show-parent
      (setq key-name-list (list `(,one-key-yas/up-key ,one-key-yas/updir-item-description
                                                      ,(expand-file-name ".." dir-name) t))))
    ;; build key for sub-dirs
    (dolist (sub-dir sub-dirs)
      (let ((key (one-key-generate-key sub-dir keys)))
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
	    (let ((key (one-key-generate-key file keys)))	      
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
    key-name-list))

;;; Unlike the original version in Yasnippet `yas/get-snippet-tables'
;;; this function only uses mode-symbol to index the loaded tables
(defun one-key-yas/get-snippet-tables (mode-symbol)
  "Get snippet tables for mode MODE-SYMBOL.
Return a list of 'yas/snippet-table' objects indexed by mode."
  (let ((mode-tables (list (gethash mode-symbol yas/snippet-tables)))
        all-tables)
    (dolist (table (remove nil mode-tables))
      (push table all-tables)
      (nconc all-tables (yas/snippet-table-get-all-parents table)))
    (remove-duplicates all-tables)))

(defun one-key-yas-get-mode-dir (mode)
  "Given major-mode symbol MODE, return the directory containing snippets for that mode.
If there is no snippets directory associated with that mode return `yas/root-directory'."
  (let* ((templates (yas/all-templates (one-key-yas/get-snippet-tables mode)))
         (full-file-names (mapcar #'(lambda (template) (yas/template-file template)) templates))
         (regex (regexp-opt (list (concat "/" (symbol-name mode) "/")))))
    (or (dolist (file-name full-file-names)
          (when (string-match regex file-name)
            (return (substring file-name 0 (match-end 0)))))
        yas/root-directory)))

(defun one-key-yas-filefunc (file)
  (let (snippet)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (setq snippet
            (buffer-substring-no-properties 
             (re-search-forward "# --.*\n") 
             (point-max))))
    (yas/expand-snippet snippet)))

(defun one-key-yas-filename-map-func (file)
  "Return name for menu item corresponding to file or dir FILE in yasnippet one-key menus."
  (if (file-directory-p file) (file-name-nondirectory file)
    (with-temp-buffer
      (let ((full-file (file-truename file)))
        (if (file-readable-p full-file)
            (progn (insert-file-contents full-file nil nil nil t)
                   (third (yas/parse-template full-file)))
          file)))))

(defun one-key-yas-get-menu (name)
  "Return cons cell in form (menu-name . menu-alist) for yasnippet one-key menus with name NAME."
  (let* ((modename (if (> (length name) 10) (substring name 10)
                     (symbol-name (with-selected-window (previous-window) major-mode))))
         (mode (intern-soft modename))
         (modedir (one-key-yas-get-mode-dir mode))
         (menuname (concat "yasnippet:" modename)))
    (cons menuname (car (one-key-dir/build-menu-alist
                         modedir
                         :filefunc 'one-key-yas-filefunc
                         :filename-map-func 'one-key-yas-filename-map-func
                         :exclude-regex "^\\.\\|~$"
                         :topdir yas/root-directory)))))

;; Set the menu-alist, title string format and special keybindings for `yasnippet' menus
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "yasnippet"
                            (lambda (name) (string-match "^yasnippet" name))
                            'one-key-yas-get-menu
                            one-key-default-title-func
                            'one-key-default-special-keybindings) t)

(provide 'one-key-yas)
;;; one-key-yas.el ends here





