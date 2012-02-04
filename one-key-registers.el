;;; one-key-regs.el --- Code for fast, simple and flexible handling of registers, macros and bookmarks

;; Filename: one-key-regs.el
;; Description: Code for fast, simple and flexible handling of registers, macros and bookmarks
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2012, Joe Bloggs, all rites reversed.
;; Created: 2012-01-22 16:27:08
;; Version: 0.1
;; Last-Updated: 2012-01-22 16:27:08
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/one-key-regs.el
;; Keywords: macros, registers, one-key, utility
;; Compatibility: GNU Emacs 24.0.50.2
;;
;; Features that might be required by this library:
;;
;; one-key.el, better-registers.el, cl.el, ido.el
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
;; Code for fast, simple and flexible handling of registers, macros and bookmarks
;; This library provides code for easy access and storage of different kinds of registers
;; (including keyboard macros and bookmarks). Register sets can be defined, saved,
;; and loaded on demand,
;; or associated with particular buffers or major-modes.
;; A one-key menu is created for the currently loaded register set which allows
;; fast access and labelling of the registers so you can remember what they do.

;;; Installation:
;;
;; Put one-key-regs.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'one-key-regs)
;;
;; The following elisp packages are also required: better-registers.el and one-key.el
;; These are available from the emacswiki: http://www.emacswiki.org


;;; Customize:
;;
;; 
;;
;; All of the above can customized by:
;;      M-x customize-group RET one-key-regs RET
;;

;;; Change log:
;;	
;; 2012/01/22
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; 
;;

;;; TODO
;; Make registers for: bookmarks, workgroups, elisp commands, arbitrary elisp code, prefix keys, register menus (see next
;; item), copy another register, swap with another register, run external program, run-assoc on file.
;; Code for viewing saved registers set as one-key menu without setting registers to keys.
;; Check require statements - do I really need better-registers?

;;; Require
(require 'one-key)
(eval-when-compile (require 'cl))
;;; Code:

(defgroup one-key-regs nil
  "Convenient access and backup of many different kinds of registers using one-key menus, and keybindings."
  :group 'one-key)

(defvar one-key-regs-reserved-register-types
  (list '(buffer-marker point-to-register
                        (lambda (reg)
                          (format "Pos %d in %s" (marker-position reg) (buffer-name (marker-buffer reg))))
                        (lambda (val) (or (markerp val) (and (listp val) (eq (car val) 'buffer-marker))))
                        (lambda (val)
                          (cond ((and (markerp val) (marker-buffer val))
                                 (switch-to-buffer (marker-buffer val)) (goto-char val))
                                ((eq (car val) 'buffer-marker) (switch-to-buffer (cadr val)) (goto-char (caddr val)))
                                (t (error "That register's buffer no longer exists"))))
                        (lambda (char contents)
                          (let* ((buf (marker-buffer contents))
                                 (file (buffer-file-name buf))
                                 (pos (marker-position contents)))
                            (if file
                                `(set-register ,char (quote (file-query ,file ,pos)))
                              `(set-register ,char (quote (buffer-marker ,(buffer-name buf) ,pos)))))))
        '(file-marker (lambda (char) (set-register char (list 'file-query buffer-file-name (point))))
                      (lambda (reg) (format "Pos %d in %s" (caddr reg) (file-name-nondirectory (cadr reg))))
                      (lambda (val) (and (consp val) (eq (car val) 'file-query)))
                      (lambda (val) (find-file (cadr val)) (goto-char (caddr val)))
                      (lambda (char contents) `(set-register ,char (quote ,contents))))
        '(text (lambda (char) (if (use-region-p)
                                    (progn (copy-to-register char (region-beginning) (region-end))
                                           (deactivate-mark))
                                (set-register char (read-string "Text: "))))
                 (lambda (reg) (format "Text: %s" (substitute ?\C-l ?\n reg)))
                 (lambda (val) (stringp val))
                 (lambda (val) (insert val))
                 (lambda (char contents) `(set-register ,char ,contents)))
        '(cut-text (lambda (char) (if (use-region-p)
                                        (progn (copy-to-register char (region-beginning) (region-end) t)
                                               (deactivate-mark))
                                    (set-register char (read-string "Text: "))))
                     (lambda (reg) (format "Text: %s" (substitute ?\C-l ?\n reg)))
                     (lambda (val) (stringp val))
                     (lambda (val) (insert val))
                     (lambda (char contents) `(set-register ,char ,contents)))
        '(rectangle (lambda (char) (if (use-region-p)
                                       (progn (copy-rectangle-to-register char (region-beginning) (region-end))
                                              (deactivate-mark))
                                     (set-register char (string-split (read-string "Rectangle: ") "\n"))))
                    (lambda (reg) (format "Rect: %s" (mapconcat 'identity reg "")))
                    (lambda (val) (and (consp val) (stringp (car val))))
                    (lambda (val) (insert-rectangle val))
                    (lambda (char contents) `(set-register ,char (quote ,contents))))
        '(cut-rectangle (lambda (char) (if (use-region-p)
                                           (progn (copy-rectangle-to-register char (region-beginning) (region-end) t)
                                                  (deactivate-mark))
                                         (set-register char (string-split (read-string "Rectangle: ") "\n"))))
                        (lambda (reg) (format "Rect: %s" (mapconcat 'identity reg "")))
                        (lambda (val) (and (consp val) (stringp (car val))))
                        (lambda (val) (insert-rectangle val))
                        (lambda (char contents) `(set-register ,char (quote ,contents))))
        '(number (lambda (char) (number-to-register nil char))
                 (lambda (reg) (format "Number: %d" reg))
                 (lambda (val) (numberp val))
                 (lambda (val) (princ val (current-buffer)))
                 (lambda (char contents)
                   `(set-register ,char ,contents)))
        '(window-config window-configuration-to-register
                        (lambda (reg) "Window config")
                        (lambda (val) (and (consp val) (window-configuration-p (car val))))
                        (lambda (val) (set-window-configuration (car val))
                          (goto-char (cadr val)))
                        nil)
        '(frame-config frame-configuration-to-register
                       (lambda (reg) "Frame config")
                       (lambda (val) (and (consp val) (frame-configuration-p (car val))))
                       (lambda (val) (set-frame-configuration (car val))
                         (goto-char (cadr val)))
                       nil))
  "A list of reserved register types and associated functions/code.
Each item in the list contains (in this order):

  1) A symbol name for the register type.

  2) A function which takes a single char argument and stores the appropriate register type in that char.

  3) A function which takes the contents of a register of this type as argument and returns a default label
     for the register.

  4) A predicate function which takes a single register argument and returns true if it is the correct type.

  5) A function which takes a single register argument and performs the appropriate actions on it for
     registers of this type (e.g. jump to marker, insert text, etc).

  6) Either nil or a function which takes two arguments: the char and contents of a register (in that order), and returns
     an elisp form which will recreate the register when evalled, e.g. (set-register ?a '(file-query \"foo.txt\" 1234))
     This form is used for saving the state of the register to a file with the `one-key-regs-save-registers' function.
     If this element is nil then registers of this type will not be saved.

This variable should not be altered by the user.
Instead you may add your own types to `one-key-regs-custom-register-types'.
Any new register type defined in `one-key-regs-custom-register-types' cannot share the same name as one of these reserved types.")

(defcustom one-key-regs-custom-register-types
  '((macro
     `(let ((old-macro last-kbd-macro))
        (setq last-kbd-macro ,last-kbd-macro)
        (call-last-kbd-macro)
        (setq last-kbd-macro old-macro))
     (lambda (reg) (caddr (nth 4 (nth 9 register-alist)))))
    (buffer
     `(switch-to-buffer ,(buffer-name))
     (lambda (reg) (format "Buffer: %s" (cadddr reg))))
    (file-or-dir
     `(let ((buf (or ,(buffer-file-name) ,dired-directory)))
        (if buf (find-file buf) (message "No file associated with buffer!")))
     (lambda (reg) (format "File: %s" (let* ((path (or (car (cdadar (caddr reg)))
                                                       (cadr (cdadar (caddr reg)))))
                                             (file (file-name-nondirectory path)))
                                        (if (equal file "") path file)))))
    (bookmark
     `(bookmark-jump ,(bookmark-completing-read "Jump to bookmark" (bmkp-default-bookmark-name)))
     (lambda (reg) (format "Bookmark: %s" (caddr reg))))
    (bookmarks-one-key-menu
     `(one-key-regs-bookmarks-menu (lambda (bmk)
                                          (let ((bmktags (bmkp-get-tags bmk)))
                                            (catch 'bmkp-b-mu-b-t-an
                                              (dolist (tag (quote ,(bmkp-read-tags-completing)))
                                                (unless (assoc-default tag (bmkp-get-tags bmk) nil t)
                                                  (throw 'bmkp-b-mu-b-t-an nil)))
                                              t))))
     (lambda (reg) (format "Bookmarks tagged %S" (cadadr (cadar (cddadr (cdaddr (caddr reg))))))))
    (browse-url
     `(browse-url ,(read-string "URL: " "http://"))
     (lambda (reg) (format "URL: %s" (caddr reg))))
    (open-externally
     `(run-associated-program ,(if (featurep 'ido) (ido-read-file-name "File: " nil nil t)
                                 (read-file-name "File: " nil nil t)))
     (lambda (reg) (format "Open externally: %s" (file-name-nondirectory (caddr reg)))))
    (emacs-command
     `(call-interactively ',(read-command "Command: "))
     (lambda (reg) (format "M-x %S" (car (cdaddr reg)))))
    )
  "A list of different types of registers for use with one-key-regs.
Each type contains three elements in this order:

  1) A symbol name to identify the type.

  2) An sexp which is evaluated when the register is created.
     It should return another sexp which will be stored in the register,
     and evaluated when the register is executed.

  3) A function which takes the contents of a register of this type as argument and returns a default label
     for the register. It is called when the register is created.

You can use the backquote to evaluate parts of an elisp form at register creation time instead of execution time.
This allows you to include information about the current marker position, region, etc.
E.g:
                   `(insert ,(buffer-substring (region-beginning) (region-end)))

See Info node `(elisp)Backquote' for more details.

Note: when saving custom registers the sexp stored in the register contents will be saved. This may or may not have
the intended effect when loaded and executed in a new emacs session (bear this in mind when creating custom types)."
  :group 'one-key-regs
  :type '(repeat (list (symbol :tag "Name" :help-echo "A name to identify the type."
                               :match (lambda (w name)
                                        (not (memq name (mapcar 'car one-key-regs-reserved-register-types)))))
                       (sexp :tag "Creation sexp" :help-echo "An sexp which evaluates to another sexp when the register is created.
The second sexp is stored in the register and will be evaluated when the register is executed.")
                       (sexp :tag "Label sexp" :help-echo "An sexp which evaluates to a default label for the register when the register is created."))))

(defvar one-key-regs-labels-alist nil
  "Alist of (char . label) pairs for items in one-key menu show registers.
The chars are the chars associated with each register, and the labels are strings that
will be displayed with each char in `one-key-regs-menu-alist' for the one-key menu.")

(defvar one-key-regs-currently-loaded-file one-key-regs-default-file
  "The currently loaded registers file, or nil if none have been loaded.
This variable is used by one-key-regs when saving changes to the current register list or their labels.")

(defcustom one-key-regs-prompt-for-label nil
  "If set to t then you will be prompted for a label when creating a new one-key-register.
If you select the register type from a prompt (by using a non-numeric prefix key with the one-key-regs-function)
you will be prompted for a label regardless of the value of this variable.
Otherwise a default label for the register will be created using the appropriate function in `one-key-regs-reserved-register-types' or `one-key-regs-custom-register-types'."
  :group 'one-key-regs
  :type '(boolean))

(defcustom one-key-regs-prefix-key-associations nil
  "An alist associating prefix keys with register types.
Each element of the alist is of the form (number . name) where number is the numeric prefix arg, and name is an elisp
symbol for the name of the associated register type.
When a one-key-regs-function is called with a numeric prefix key it will check this list to see what kind of
register to create. If a normal C-u prefix is used or the numeric prefix doesn't occur in the list then the user
will be prompted for a register type to create."
  :group 'one-key-regs
  :type '(alist :key-type (integer :tag "Numeric prefix arg"
                                   :help-echo "A numeric prefix arg to associate with the register type")
                :value-type (symbol :tag "Register type"
                                    :help-echo "The name of the register type (a lisp symbol)"
                                    :match (lambda (w name)
                                             (or (memq name (mapcar 'car one-key-regs-custom-register-types))
                                                 (memq name (mapcar 'car one-key-regs-reserved-register-types)))))))

(defcustom one-key-regs-default-register-type 'buffer-marker
  "The default register type to create when filling an empty register.
If the quick key corresponding to an empty register is pressed, and no prefix key was pressed beforehand then a new
register of this type will be created."
  :group 'one-key-regs
  :type '(symbol :tag "Register type" :help-echo "The name of an existing register type (a lisp symbol)"
                 :match (lambda (w name)
                          (or (memq name (mapcar 'car one-key-regs-custom-register-types))
                              (memq name (mapcar 'car one-key-regs-reserved-register-types))))))

(defcustom one-key-regs-default-region-register-type 'region
  "The default register type to create when the region is active.
If a register quick key is pressed when the region is active, then a register of this type will be created,
unless a prefix key was pressed beforehand in which case the associated register type in `one-key-regs-prefix-key-associations' will be created instead."
  :group 'one-key-regs
  :type '(symbol :tag "Register type" :help-echo "The name of an existing register type (a lisp symbol)"
                 :match (lambda (w name)
                          (or (memq name (mapcar 'car one-key-regs-custom-register-types))
                              (memq name (mapcar 'car one-key-regs-reserved-register-types))))))

(defcustom one-key-regs-save-items nil
  "A list of items to save when saving register sets.
Can include the following items: `one-key-regs-default-register-type',
`one-key-regs-default-region-register-type', and `one-key-regs-prefix-key-associations'."
  :group 'one-key-regs
  :type '(set (const :tag "Default register type" one-key-regs-default-register-type)
              (const :tag "Default region register type" one-key-regs-default-region-register-type)
              (const :tag "Prefix key associations" one-key-regs-prefix-key-associations)))

(defcustom one-key-regs-max-label-length 55
  "The maximum number of chars allowed in the register labels (see `one-key-regs-labels-alist')."
  :group 'one-key-regs
  :type '(number))

(defcustom one-key-regs-default-directory "~/.emacs.d/"
  "The default directory in which to store one-key-regs sets.
Should end with a \"/\"."
  :group 'one-key-regs
  :type '(directory))

(defcustom one-key-regs-default-file (concat one-key-regs-default-directory "default_registers.el")
  "The default file in which to save one-key-regs.
This file will be loaded when this library is loaded."
  :group 'one-key-regs
  :type '(file))

(defcustom one-key-regs-merge-conflicts 'prompt
  "What method to use to handle conflicts when loading new registers.
If a new register uses the same char as a currently loaded register this variable is used to decide how to resolve
the conflict."
  :group 'one-key-regs
  :type '(choice (const :tag "Prompt" prompt)
                 (const :tag "Overwrite old registers which conflict" overwriteold)
                 (const :tag "Discard new registers which conflict" discardnew)
                 (const :tag "Change char of old registers which conflict" changeold)
                 (const :tag "Change char of new registers which conflict" changenew)
                 (const :tag "Completely replace old register set with new one" replace)))

(defcustom one-key-regs-prefix-key "C-s"
  "The default prefix for `one-key-regs' keys.
Keys formed from this key joined with the keys defined by `one-key-regs-quick-keys' will be bound to
the `one-key-regs-function-'s by the `one-key-regs-make-keybindings' function."
  :group 'one-key-regs
  :type '(string))

(defcustom one-key-regs-menu-key "<menu>"
  "The default key for the `one-key-regs-menu'.
This key will be concatenated with `one-key-regs-prefix-key' and bound to `one-key-regs-menu' which
opens the main menu for `one-key-regs'."
  :group 'one-key-regs
  :type '(string))

(defcustom one-key-regs-quick-keys (number-sequence 33 126)
  "List of keys to use with `one-key-regs-make-keybindings'.
These keys will be used with the prefix key supplied to that function to define
shortcut keys to `one-key-regs-functions' which copy/insert registers."
  :group 'one-key-regs
  :type '(repeat character))

(defun one-key-regs-get-type (reg)
  "Return the register type of register contents REG.
The register type is one of the symbols listed in the cars of the elements of `one-key-regs-reserved-register-types',
 and `one-key-regs-custom-register-types'."
  (or (loop for type in one-key-regs-reserved-register-types
            if (funcall (cadddr type) reg) do (return (car type)))
      (and (consp reg) (symbolp (car reg)) (car reg))))

(defun one-key-regs-make-function (char)
  "Function to create function with name one-key-regs-function-<CHAR> (where <CHAR> is (char-to-string CHAR)).
The one-key-regs-function-<CHAR> function either executes the register associated with char <CHAR>, or creates
a new register depending on whether a region is active and if a prefix arg is supplied."
  (eval
   `(defun ,(intern (concat "one-key-regs-function-" (char-to-string char))) (prefixarg &optional regtype)
      ,(format "Create new register and place in char '%c', or execute existing register in char '%c'.
If region is active and no prefix is used, create a register of type `one-key-regs-default-region-register-type'.
If region is not active an no prefix arg is used then execute the register in '%c' or create a new one of type
`one-key-regs-default-register-type' if such a register already exists.
If a numeric prefix arg is used then create a register of the appropriate type according to `one-key-regs-prefix-key-associations', and use the default label for the one-key-regs menu.
If a non-numeric prefix arg is used then prompt the user for the type of register to create and associated label."
               char char char)
      (interactive "P")
      (if (or prefixarg regtype (use-region-p) (not (assq ,char register-alist)))
          (let* ((knownprefix (memq prefixarg (mapcar 'car one-key-regs-prefix-key-associations)))
                 (unknownprefix (and prefixarg (not knownprefix)))
                 (regtype1 (cond (regtype regtype)
                                 ((and (use-region-p) (not prefixarg)) one-key-regs-default-region-register-type)
                                 ((not prefixarg) one-key-regs-default-register-type)
                                 (knownprefix (cdr (assq prefixarg one-key-regs-prefix-key-associations)))
                                 (unknownprefix (let* ((custtypes (mapcar (lambda (x) (symbol-name (car x)))
                                                                          one-key-regs-custom-register-types))
                                                       (restypes (mapcar (lambda (x) (symbol-name (car x)))
                                                                         one-key-regs-reserved-register-types))
                                                       (alltypes (append restypes custtypes)))
                                                  (intern (if (featurep 'ido)
                                                              (ido-completing-read "Register type: " alltypes nil t)
                                                            (completing-read "Register type: " alltypes nil t)))))))
                 (restype (assq regtype1 one-key-regs-reserved-register-types))
                 (custype (assq regtype1 one-key-regs-custom-register-types)))
            (if restype (funcall (cadr restype) ,char)
              (if custype (set-register ,char (cons regtype1 (eval (cadr custype))))
                (error "Invalid register type: %S" regtype1)))
            (let* ((contents (cdr (assq ,char register-alist)))
                   (str (cond ((and restype (caddr restype)) (funcall (caddr restype) contents))
                              ((and custype (caddr custype)) (funcall (caddr custype) contents))
                              (t (symbol-name regtype1))))
                   (defaultlabel (substring str 0 (min one-key-regs-max-label-length (length str))))
                   (label (if (or unknownprefix one-key-regs-prompt-for-label)
                              (let* ((str (read-string (format "Label for register (default is \"%s\"): " defaultlabel)
                                                       nil nil defaultlabel))
                                     (minlen (min (length str) one-key-regs-max-label-length)))
                                (substring (mapconcat 'identity (split-string str "\n") "\\n") 0 minlen))
                            defaultlabel)))
              (add-to-list 'one-key-regs-labels-alist (cons ,char label))))
        (one-key-regs-execute-register ,char))) ;; end of one-key-regs-function-char
   ))

(defun one-key-regs-execute-register (char)
  "Execute the register stored in char CHAR.
If the register stored in char CHAR is a reserved type, then execute the associated function in
`one-key-regs-reserved-register-types', othewise if the register is a custom type, execute the associated
elisp form in `one-key-regs-custom-register-types'." 
  (interactive "cJump to register: ")
  (let* ((val (get-register char)))
    (if (not (loop for res in one-key-regs-reserved-register-types
                   if (funcall (cadddr res) val) do (funcall (cadddr (cdr res)) val) and do (return t)))
        (if (assq (car val) one-key-regs-custom-register-types)
            (eval (cdr val))
          (error "Invalid register: %S" val)))))

(defun one-key-regs-make-keybindings (prefixkey)
  "Bind keys with prefix PREFIXKEY to functions with names of the form one-key-regs-function-<CHAR>.
The <CHAR>'s are those stored in `one-key-regs-quick-keys', and the one-key-regs-function's are created by the
`one-key-regs-make-function' function."
  (interactive (list (read-string "Prefix key (default = C-s): " nil nil "C-s")))
  (dolist (char one-key-regs-quick-keys)
    (one-key-regs-make-function char)
    (if (and (< char 91) (> char 64))
        (eval `(global-set-key
                (kbd ,(concat prefixkey "-S-" (char-to-string (+ char 32))))
                ',(intern (concat "one-key-regs-function-" (char-to-string char)))))
      (eval `(global-set-key
              (kbd ,(concat prefixkey "-" (char-to-string char)))
              ',(intern (concat "one-key-regs-function-" (char-to-string char))))))))

(defvar one-key-regs-menu-alist nil
  "The `one-key' menu alist for one-key-regs-functions.")

(defun one-key-regs-menu nil
  "The `one-key' menu for one-key-regs."
  (interactive)
  (setq one-key-regs-menu-alist
      (loop for (x . y) in register-alist
            for char = (char-to-string x)
            for label = (or (cdr (assq x one-key-regs-labels-alist))
                            (symbol-name (one-key-regs-get-type y)))
            for func = `(lambda nil (interactive) (one-key-regs-execute-register ,x))
            collect (cons (cons char label) func)))
  (add-to-list 'one-key-regs-menu-alist '(("SPC" . "Register functions") . one-key-regs-extra-menu))
  (one-key-menu "show-registers" one-key-regs-menu-alist))

(defvar one-key-regs-extra-menu-alist nil
  "The `one-key' menu alist for one-key-regs extra functions.")
      
(setq one-key-regs-extra-menu-alist
      '((("h" . "Show `one-key-regs' help") . one-key-regs-show-help)
        (("e" . "Edit registers file.") . one-key-regs-open-registers-file)
        (("p" . "Show current prefix associations") . one-key-regs-show-prefix-key-associations)
        (("m" . "Merge with another register set") . one-key-regs-merge-registers)
        (("l" . "Load new register set (and replace old ones)") . one-key-regs-replace-registers)
        (("S" . "Save current register set") . one-key-regs-save-registers)
        (("s" . "Save current register set (prompt for file)") . (lambda nil (interactive)
                                                                   (one-key-regs-save-registers nil t)))
        (("C" . "Clear current registers from memory") . one-key-regs-clear-registers)
        (("c" . "Customize `one-key-regs' settings.") . (lambda nil (interactive)
                                                                          (customize-group "one-key-regs")))
        (("C-b" . "back to registers menu") . one-key-regs-menu)))

(defun one-key-regs-extra-menu ()
  "The `one-key' menu for one-key-regs extra functions."
  (interactive)
  (one-key-menu "Register functions" one-key-regs-extra-menu-alist))

(defun one-key-regs-show-help nil
  "Show help file explaining how to use `one-key-regs'."
  (interactive)
  (let* ((bufname "*one-key-regs help*")
         (helpbuf (get-buffer bufname)))
    (if helpbuf (display-buffer helpbuf)
      (with-current-buffer (generate-new-buffer bufname)
        (insert (format "Hi %S
%S" one-key-regs-prefix-key one-key-regs-prefix-key-associations)))
      (display-buffer (get-buffer bufname)))))

(defun one-key-regs-show-prefix-key-associations nil
  "Show current default registers and prefix key associations.
In other words show the values of variables `one-key-regs-default-register-type',
`one-key-regs-default-region-register-type', and `one-key-regs-prefix-key-associations'."
  (interactive)
  (let* ((lines (loop for (arg . name) in one-key-regs-prefix-key-associations
                      collect (format "Numeric prefix %d\t\t: %S" arg name)))
         (str (mapconcat 'identity lines "\n")))
    (message (format "Default register type\t\t: %S
Default region register type\t: %S
%s" one-key-regs-default-register-type one-key-regs-default-region-register-type str))))

(defun one-key-regs-open-registers-file nil
  "Open the file associated with the current register set, or prompt for a file is non is associated.
The file associated with the current register set is `one-key-regs-currently-loaded-file'."
  (interactive)
  (let ((file (or one-key-regs-currently-loaded-file
                  (if (featurep 'ido)
                      (ido-read-file-name "Open registers file: " one-key-regs-default-directory nil t)
                    (read-file-name "Open registers file: " one-key-regs-default-directory nil t)))))
    (find-file file)))

(defun one-key-regs-save-registers (&optional filename queryp)
  "Save the contents of all registers and associated labels in file FILENAME as loadable data (elisp forms).
Cannot save window/frame configurations, but it works with markers, text, rectangles, numbers,
and any custom register types defined in `one-key-regs-custom-register-types'.

When called interactively set FILENAME to `one-key-regs-default-file' unless this is nil or QUERYP is non-nil
in which case query the user for the filename.
When called non-interactively if either FILENAME is nil or QUERYP is non-nil then query the user for the FILENAME.
In both cases when querying for a filename, use FILENAME as default if non-nil (i.e. use `one-key-regs-default-file'
in the interactive case).
If there are no errors then `one-key-regs-default-file' will be set to the value of FILENAME when the function
has finished saving to it.

Note: for custom register types the sexp stored in the register contents will be saved. This may or may not have
the intended effect when loaded and executed in a new emacs session (bear this in mind when creating custom types)."
  (interactive (list one-key-regs-default-file current-prefix-arg))
  (when (or queryp (not filename))
    (setq filename (read-file-name (if filename
                                       (format "Save to file (%s): " filename)
                                     "Save to file: ")
                                   (file-name-directory one-key-regs-default-file)
                                   one-key-regs-default-file)))
  (let ((print-level nil) (print-length nil)) ; let us write anything
    (with-temp-file filename
      (loop for (char . contents) in register-alist
            for type = (one-key-regs-get-type contents)
            for resfunc = (nth 5 (assq type one-key-regs-reserved-register-types))
            for custype = (assq type one-key-regs-custom-register-types)
            for label = (assq char one-key-regs-labels-alist)
            if resfunc do (insert (format "%S\n" (funcall resfunc char contents)))
            else if custype do (insert (format "(set-register %d (quote %S))\n" char contents)))
      (insert (format "\n(setq one-key-regs-labels-alist '%S)\n\n" one-key-regs-labels-alist))
      (loop for var in one-key-regs-save-items
            do (insert (format "(setq %S '%S)\n" var (eval var)))))
    (setq one-key-regs-default-file filename)))

(defun one-key-regs-change-key (reg label &optional prompt key)
  "Change the key associated with register REG and label LABEL.
The key will be changed to an unassigned key, or KEY if supplied and not
already used. If PROMPT is non-nil prompt the user for an unused key.
REG should be an element of a `register-alist' and LABEL an element of `one-key-regs-labels-alist'."
  (let ((unused-keys (set-difference one-key-regs-quick-keys (mapcar 'car register-alist))))
    (while (not (memq key unused-keys))
      (setq key (if prompt (read-key (format "Enter new key for \"%s\"
Available keys: %s" (cdr label) (mapcar 'char-to-string unused-keys)))
                  (car unused-keys)))))
  (setcar reg key)
  (if label (setcar label key)))

;(defun one-key-regs-swap-keys ???)

(defun one-key-regs-merge-registers (file mergemethod)
  "Load registers stored in a one-key-regs file.
If there are any conflicts between the chars used in the new registers with those used in the already loaded
registers the conflicts will be resolved according to the value of MERGEMETHOD which is set equal
to `one-key-regs-merge-conflicts' if called interactively, unless a prefix arg is supplied.
If a prefix arg is supplied and `one-key-regs-merge-conflicts' is set to 'prompt then MERGEMETHOD will be
set to 'replace, otherwise it will be set to 'prompt."
  (interactive
   (list (if (featurep 'ido)
             (ido-read-file-name "one-key-regs file: " one-key-regs-default-directory nil t)
           (read-file-name "one-key-regs file: " one-key-regs-default-directory nil t))
         (if (not current-prefix-arg) one-key-regs-merge-conflicts
           (if (eq one-key-regs-merge-conflicts 'prompt) 'replace 'prompt))))
  (let ((old-register-alist (copy-alist register-alist))
        (old-one-key-regs-labels-alist (copy-alist one-key-regs-labels-alist)))
    (setq register-alist nil one-key-regs-labels-alist nil)
    (load-file file)
    (if (eq mergemethod 'replace) (setq one-key-regs-currently-loaded-file file))
    (macrolet ((keepold nil '(progn (setcdr newreg (cdr oldreg))
                                    (if newlabel (setcdr newlabel (cdr oldlabel))
                                      (if oldlabel
                                          (add-to-list 'one-key-regs-labels-alist oldlabel)))))
               (addold nil '(progn (add-to-list 'register-alist oldreg)
                                   (if oldlabel (add-to-list 'one-key-regs-labels-alist oldlabel))))
               (changekey (r l p) `(one-key-regs-change-key ,r ,l ,p)))
      (loop named regloop for oldreg in old-register-alist
            for char = (car oldreg)
            for oldlabel = (or (assq char old-one-key-regs-labels-alist)
                               (cons char (symbol-name (one-key-regs-get-type (cdr oldreg)))))
            for newreg = (assq char register-alist)
            for newlabel = (or (assq char one-key-regs-labels-alist)
                               (cons char (symbol-name (one-key-regs-get-type (cdr newreg)))))
            if newreg do (case mergemethod
                           (prompt (while (not (case (read-key (format "\"%c\" key is currently used for \"%s\" but new register set assigns it to \"%s\", what shall we do?
1 - overwrite old register with new one
2 - keep old register and discard new one
3 - change char of new register
4 - change char of old register
5 - overwrite all further conflicting old registers
6 - discard all further conflicting new registers
7 - change char of all conflicting new registers (to next unassigned char)
8 - change char of all conflicting old registers (to next unassigned char)" char (cdr oldlabel) (cdr newlabel)))
                                                 (?1 t)
                                                 (?2 (keepold) t)
                                                 (?3 (changekey newreg newlabel t) (addold) t)
                                                 (?4 (changekey oldreg oldlabel t) (addold) t)
                                                 (?5 (setq mergemethod 'overwriteold) t)
                                                 (?6 (keepold) (setq mergemethod 'discardnew) t)
                                                 (?7 (changekey newreg newlabel nil) (addold)
                                                     (setq mergemethod 'changenew) t)
                                                 (?8 (changekey oldreg oldlabel nil) (addold)
                                                     (setq mergemethod 'changeold) t)
                                                 (?\a (setq register-alist old-register-alist)
                                                      (setq one-key-regs-labels-alist old-one-key-regs-labels-alist)
                                                      (return-from regloop))))))
                           (overwriteold t)
                           (discardnew (keepold))
                           (changeold (changekey oldreg oldlabel nil) (addold))
                           (changenew (changekey newreg newlabel nil) (addold)))
            else if (not (eq mergemethod 'replace)) do (addold)))))

(defun one-key-regs-replace-registers (file)
  "Replace the currently loaded register set with that saved in file FILE."
  (interactive
   (list (if (featurep 'ido)
             (ido-read-file-name "one-key-regs file: " one-key-regs-default-directory nil t)
           (read-file-name "one-key-regs file: " one-key-regs-default-directory nil t))))
  (one-key-regs-merge-registers file 'replace))

(defun one-key-regs-clear-registers (&optional noprompt)
  "Delete all registers from memory.
Unless NOPROMPT is non-nil the user will be prompted to check if they want to continue."
  (interactive "P")
  (if (or noprompt (y-or-n-p "Are you sure you want to clear all registers? "))
      (setq register-alist nil one-key-regs-labels-alist nil
            one-key-regs-currently-loaded-file nil)))

(defvar one-key-regs-bookmarks-alist nil
  "The `one-key' menu alist for current-bookmarks.")

(defun one-key-regs-bookmarks-menu (filterp)
  "Create a `one-key' menu of bookmarks which are verified by predicate FILTERP.
FILTERP should be a function which takes a single bookmark as argument and returns non-nil if the
bookmark should be added to the `one-key' menu."
  (interactive)
  (setq one-key-regs-bookmarks-alist
        (let ((usedchars nil)
              (minfree 48))
          (loop for bmk in (bmkp-remove-if-not filterp bookmark-alist)
                for name = (car bmk)
                for char = (let ((i 0)
                                 (chars (append name nil))
                                 chosenchar)
                             (while (and (< i (length chars)) (member (elt chars i) usedchars))
                               (setq i (+ i 1)))
                             (if (equal i (length chars))
                                 (progn 
                                   (while (member minfree usedchars)
                                     (setq minfree (+ 1 minfree)))
                                   (setq chosenchar minfree)) ;(substring-no-properties bmk 0)
                               (setq chosenchar (elt chars i)))
                             (add-to-list 'usedchars chosenchar)
                             chosenchar)
                for func = `(lambda nil (interactive) (bookmark-jump ,name))
                collect (cons (cons (char-to-string char) name) func))))
  (add-to-list 'one-key-regs-bookmarks-alist
               '(("SPC" . "Anything bookmarks (C-x p e)") .
                 (lambda nil (interactive) (anything-other-buffer
                                            '(anything-c-source-bmkp
                                              anything-c-source-bmkp-filter)
                                            (car anything-current-bmkp)))))
  (add-to-list 'one-key-regs-bookmarks-alist '(("<return>" . "Bookmarks list") . edit-bookmarks))
  (one-key-menu "filtered-bookmarks" one-key-regs-bookmarks-alist))

(one-key-regs-make-keybindings one-key-regs-prefix-key)
(global-set-key (eval `(kbd ,(concat one-key-regs-prefix-key "-"
                                     one-key-regs-menu-key))) 'one-key-regs-menu)

(if (file-readable-p one-key-regs-default-file)
    (load one-key-regs-default-file))

(provide 'one-key-regs)

;;; one-key-regs.el ends here
