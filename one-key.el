;;; one-key.el --- One key

;; Filename: one-key.el
;; Description: One key
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;;         rubikitch <rubikitch@ruby-lang.org>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2010, Joe Bloggs, all rites reversed.
;; Copyright (C) 2008, 2009, 2010 Andy Stewart, all rights reserved.
;; Copyright (C) 2009, rubikitch, all rights reserved.
;; Created: 2008-12-22 21:54:30
;; Version: 0.7.1
;; Last-Updated: 7/12/2010 20:22:00
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/one-key.el
;; Keywords: one-key
;; Compatibility: GNU Emacs 22 ~ 23
;;
;; Features that might be required by this library:
;;
;; `cl'
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
;; along with this program.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; With so many Emacs extensions, you have a lot of keystrokes to remember, and you probably forget most of them.
;;
;; This package fixes this problem.
;;
;; One Key provides a TOP keystroke that when pressed presents you with
;; a menu of choices in a popup window for commands to execute with a further keystroke.
;;
;; Just type one of the listed keystrokes to execute the corresponding command.
;;
;; You can also associate different menus with different major modes so that the menu presented depends on the
;; current major mode.
;;
;; * Quick use example:
;;
;; Add the variables and functions below to your ~/.emacs
;;
;; (defvar one-key-menu-emms-alist nil
;;   "`One-Key' menu list for EMMS.")
;;
;; (setq one-key-menu-emms-alist
;;       '(
;;         (("g" . "Playlist Go") . emms-playlist-mode-go)
;;         (("d" . "Play Directory Tree") . emms-play-directory-tree)
;;         (("f" . "Play File") . emms-play-file)
;;         (("i" . "Play Playlist") . emms-play-playlist)
;;         (("t" . "Add Directory Tree") . emms-add-directory-tree)
;;         (("c" . "Toggle Repeat Track") . emms-toggle-repeat-track)
;;         (("w" . "Toggle Repeat Playlist") . emms-toggle-repeat-playlist)
;;         (("u" . "Play Now") . emms-play-now)
;;         (("z" . "Show") . emms-show)
;;         (("s" . "Emms Streams") . emms-streams)
;;         (("b" . "Emms Browser") . emms-browser)))
;;
;; (defun one-key-menu-emms ()
;;   "`One-Key' menu for EMMS."
;;   (interactive)
;;   (one-key-menu "emms" one-key-menu-emms-alist t))
;;
;; Add an item to `one-key-toplevel-alist' in the customization buffer for one-key
;; (M-x customize-group RET one-key RET). The first item should be the key (e.g. m), the second item
;; should be a description (e.g. "Emacs multimedia system"), and the third item should be the command:
;; `one-key-menu-emms'. Then bind `one-key-menu-toplevel' to any key you want E.g:
;;
;;  (global-set-key (kbd "C-M-s-SPC") 'one-key-menu-toplevel)
;;
;; Alternatively you can ignore the toplevel menu and just bind `one-key-menu-emms' to a key,
;; E.g:
;;
;;      (global-set-key (kbd "C-c p") 'one-key-menu-emms)
;;
;; Now when you type the key, a one-key menu will popup at the bottom of the window.
;; Then you just type a keystroke listed in the menu to execute the corresponding command.
;;
;; You can also associate menus with major-modes using the customizable `one-key-mode-alist' variable, 
;; and the `one-key-get-menu' command. When this command is run it will open the menu associated with the 
;; current major-mode, or the toplevel menu if there is no associated menu.
;; You can bind this to a global key, e.g:
;;
;;     (global-set-key (kbd "C-s-SPC") 'one-key-get-menu)
;;
;; Now you don't need to remember so many keystrokes, just remembering one keystroke is enough!
;;
;; ** The format of the menu list:
;;
;; (("KEYSTROKE" . "DESCRIBE") . COMMAND)
;;
;; Example:
;;
;; (defvar example-menu-alist
;;      '(
;;        (("Keystroke-A" . "Describe-A") . Command-A)
;;        (("Keystroke-B" . "Describe-B") . Command-B)
;;        (("Keystroke-C" . "Describe-C") . Command-C)
;;        ))
;;
;; Make sure COMMAND is `interactive', otherwise it will throw an error.
;;
;; ** The format of menu function:
;;
;; (one-key-menu "MENU-NAME" MENU-ALIST)
;;
;; Example:
;;
;; (defun example-menu ()
;;   (interactive)
;;   (one-key-menu "example" example-menu-alist)
;;
;; ** The arguments of the function `one-key-menu':
;;
;; `title' is the title of menu, any string you like.
;; `info-alist' is a special list that contains KEY, DESCRIPTION
;;      and COMMAND.  see above description about `example-menu-alist'.
;; `miss-match-exit-p' set to t means the popup window will exit when you
;;      type a KEY that can't match in menu.
;; `recursion-p' is whether or not recursion will execute `one-key-menu' on self
;;      when no KEY matchs in the menu.
;; `protect-function' is a protect function that is called last in `one-key-menu',
;;      make sure this function is an `interactive' function.
;; `alternate-function' is an alternate function to execute last.
;; `execute-last-command-when-miss-match' whether to execute the last input command
;; when keystroke is not matched.
;;
;; Creating menus for keymaps:
;;
;; You can use `one-key-insert-template' to insert template code for a special keymap,
;; or `one-key-show-template' to create a special buffer called "One-Key-Template" containing the template code.
;; For example, after you run `one-key-insert-template', you will get a Keymap prompt:
;; "Keymap to One-Key: ", in which you enter the name of a keymap or a prefix key with an associated keymap.
;; After entering the keymap/prefix key you are prompted for a title for the menu, and then code for the menu
;; will be automatically generated.
;; E.g. if you type "C-x r", and then enter the title "bookmark" then it will generate template code
;; like the code shown below:
;;
;; (defvar one-key-menu-bookmark-alist nil
;;   "The `one-key' menu list for BOOKMARK.")
;;
;; (setq one-key-menu-bookmark-alist
;;    '(
;;      (("C-@" . "point-to-register") . point-to-register)
;;      (("SPC" . "point-to-register") . point-to-register)
;;      (("+" . "increment-register") . increment-register)
;;      (("b" . "bookmark-jump") . bookmark-jump)
;;      (("c" . "clear-rectangle") . clear-rectangle)
;;      (("d" . "delete-rectangle") . delete-rectangle)
;;      (("f" . "frame-configuration-to-register") . frame-configuration-to-register)
;;      (("g" . "insert-register") . insert-register)
;;      (("i" . "insert-register") . insert-register)
;;      (("j" . "jump-to-register") . jump-to-register)
;;      (("k" . "kill-rectangle") . kill-rectangle)
;;      (("l" . "bookmark-bmenu-list") . bookmark-bmenu-list)
;;      (("m" . "bookmark-set") . bookmark-set)
;;      (("n" . "number-to-register") . number-to-register)
;;      (("o" . "open-rectangle") . open-rectangle)
;;      (("r" . "copy-rectangle-to-register") . copy-rectangle-to-register)
;;      (("s" . "copy-to-register") . copy-to-register)
;;      (("t" . "string-rectangle") . string-rectangle)
;;      (("w" . "window-configuration-to-register") . window-configuration-to-register)
;;      (("x" . "copy-to-register") . copy-to-register)
;;      (("y" . "yank-rectangle") . yank-rectangle)
;;      (("C-SPC" . "point-to-register") . point-to-register)
;;      ))
;;
;; (defun one-key-menu-bookmark ()
;;   (interactive)
;;   (one-key-menu "BOOKMARK" one-key-menu-bookmark-alist))
;;
;; If you used `one-key-show-template' the code is placed in the special buffer "One-Key-Template"
;; which has it's own one-key menu and keybindings bound to special helper functions to help you edit the
;; menu. Type M-x one-key-get-menu to see a menu of commands/keybindings for this buffer
;; (or use one-key-menu-one-key-template if it is not listed in one-key-mode-alist).
;; For example you can move items in the menu up/down using "M-<up>" or "M-<down>".
;; You can sort the items in the currently active region alphabetically by description/key binding/command
;; by pressing "C-c C-s" followed by d/k/c.
;; You can quickly test your menu by pressing "C-c C-t".
;;
;; Fixed menu keys:
;;
;; Some keys are available for all menus for performing tasks such as showing help or sorting the menu items,
;; they can be configured with the `one-key-special-keybindings' variable.
;;
;; Auto-load one-key menus:
;;
;; If you set `one-key-auto-load-menus' to t (in the customization group for one-key), then any files
;; in the directory specified by `one-key-menus-location' that match the regexp `one-key-menus-regexp'
;; will automatically be loaded on startup.

;;; Installation:
;;
;; Put one-key.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'one-key)
;;
;; Because this library uses a special implementation,
;; sometimes a `max-lisp-eval-depth' or `max-specpdl-size' error can occur.
;;
;; So making the above two variables larger will reduce the probability that an error occurs.
;; E.g:
;;
;; (setq max-lisp-eval-depth 10000)
;; (setq max-specpdl-size 10000)
;;

;;; Customize:
;;
;; `one-key-buffer-name' : the buffer name of the popup menu.
;; `one-key-menu-window-max-height' : the maximal height use in popup window.
;; `one-key-items-per-line' : number of items in one line, if this option is `nil', will be calculated by `window-width'.
;; `one-key-keystroke-face' : face for highlighting keystroke
;; `one-key-auto-load-menus' : if t then automatically load one-key menus from `one-key-menus-location'
;; `one-key-menus-location' : location in which one-key menus will be stored
;; `one-key-menus-regexp' : regexp to match filenames of one-key menus
;; `one-key-mode-alist' : An alist of major-mode, one-key menu pairs to set the default menu for each major-mode.
;; `one-key-toplevel-alist' : A list of key items for the toplevel menu.
;; `one-key-popup-window' : whether to popup window when first time run, default is `t'.
;; `one-key-prompt-face' : face for highlighting prompt
;; `one-key-template-buffer-name' : the buffer name of the template code.
;; `one-key-title-face' : face for highlighting title
;; `one-key-special-keybindings' : special keybindings and associated descriptions and functions that apply to
;;                                 all `one-key' menus

;; All above options can be customized through:
;;      M-x customize-group RET one-key RET
;;

;;; Change log:
;; 2010/12/07
;;    * Joe Bloggs
;;       * Added key-binding ("C-/" by default) to jump to source file of current one-key menu for editing.
;;       * Made fixed menu keys configurable with variables `one-key-key-hide' `one-key-key-quit' `one-key-key-up'
;;         `one-key-key-down' `one-key-key-pgup' `one-key-key-pgdown' `one-key-key-help' `one-key-key-edit'
;;         (they are called one-key-key-??? instead of one-key-???-key so that they will group together in the
;;          customization buffer).
;;       * Deleted `one-key-highlight-prompt' function since this is not used anywhere.
;;       * Added new variable `one-key-column-major-order', and altered `one-key-menu-format' function so that
;;         now you can choose whether items should be listed column first or row first.
;;
;; 2010/11/27
;;    * Joe Bloggs
;;       * Quick fix to one-key-template-write so that it remains in one-key-template-mode after writing
;;       
;; 2010/11/23
;;    * Joe Bloggs
;;       * Added `one-key-template-group-key-items-by-regexps', `one-key-template-describe-command',
;;         and associated keybindings and menu items.
;;
;; 2010/11/20
;;    * Joe Bloggs
;;       * Added `one-key-template-write' function for saving *One-Key-Template* buffer in `one-key-menus-location',
;;         and added keybinding `one-key-template-mode' and item to `one-key-menu-one-key-template-alist'.
;;       
;; 2010/11/18
;;    * Joe Bloggs
;;       * Added new major mode for editing one-key-menus in *One-Key-Template* buffer
;;       * Added following functions to aid editing menus in *One-Key-Template* buffer:
;;          `one-key-template-mode', `one-key-template-move-line-region', `one-key-template-move-line-region-up'
;;          `one-key-template-move-line-region-down', `one-key-template-test-menu', `one-key-template-mark-key-items'
;;          `one-key-template-sort-key-items-by-command-alphabetically',
;;          `one-key-template-sort-key-items-by-description-alphabetically',
;;          `one-key-template-sort-key-items-by-key-alphabetically',
;;          `one-key-menu-one-key-template', `one-key-menu-one-key'
;;       * Added keybindings for `one-key-template-mode'.
;;       * Altered `one-key-menu-format' function so that the keys are ordered by column instead of by row.
;;       * Added `one-key-toplevel-alist' customizable variable and `one-key-menu-toplevel' function.
;;       * Added `one-key-mode-alist' customizable variable and `one-key-get-menu' function.
;;       * Alterend `one-key-insert-template' and `one-key-show-template' functions so that they also add
;;         optional (commented) code to add items to `one-key-mode-alist' and `one-key-toplevel-alist'
;;       * Added customization variables `one-key-menus-location', `one-key-menus-regexp' and
;;         `one-key-auto-load-menus', and function `one-key-load-files'.
;;         Added code to automatically load menus if `one-key-auto-load-menus' is set to t.
;;       * Fixed spelling mistakes in documentation and added documentation for new features.
;;
;; 2010/09/27
;;    * Joe Bloggs
;;       * Altered one-key-make-template so that it adds the original keys to the descriptions of each item.
;;       
;; 2010/09/21
;;    * Joe Bloggs
;;       * Fixed a problems with one-key-make-template so it should work with more keymaps
;;       * Added ability to get help on one-key-menu items by pressing C-? followed by item key
;;       * Altered header text of menu
;;       * Fixed bug in one-key-menu so that window pops up if one-key-popup-window is t
;;         (this was also fixed independently by Andy, but I'm keeping my fix since it works fine)
;;
;; 2009/03/09
;;   * Andy Stewart:
;;      * Add `char-valid-p' for compatibility Emacs 22.
;;
;; 2009/02/25
;;   * Andy Stewart:
;;      * Fix a bug of `one-key-menu'.
;;
;; 2009/02/19
;;   * Andy Stewart:
;;      * Just show help message when first call function `one-key-menu',
;;        don't overwritten message from command.
;;      * Remove function `one-key-menu-quit' and
;;        option `one-key-show-quit-message', unnecessary now.
;;
;; 2009/02/10
;;   * rubikitch
;;      * Fix bug.
;;      * PageUp and PageDown are scroll page keys now.
;;      * Add new option `one-key-show-quit-message'.
;;
;; 2009/01/28
;;   * Andy Stewart:
;;      * Capitalize describe in variable `one-key-menu-*-alist'.
;;
;; 2009/01/27
;;   * rubikitch
;;      * Fix doc.
;;
;; 2009/01/26
;;   * rubikitch
;;      * Improve code.
;;
;; 2009/01/25
;;   * Andy Stewart:
;;      * Applied rubikitch's patch for generate
;;        template code automatically, very nice!
;;
;; 2009/01/22
;;   * rubikitch:
;;      * Add new option `one-key-items-per-line'.
;;      * Refactory code make it more clear.
;;      * Fix bug.
;;   * Andy Stewart:
;;      * Applied rubikitch's patch. Thanks!
;;      * Modified code make build-in keystroke
;;        can be overridden.
;;      * Fix doc.
;;
;; 2009/01/20
;;   * Andy Stewart:
;;      * Add new option `execute-last-command-when-miss-match'
;;        to function `one-key-menu', make user can execute
;;        last input command when miss match key alist.
;;
;; 2009/01/15
;;   * rubikitch:
;;      * Fix bug of `one-key-menu'.
;;      * Add recursion execute support for `one-key-menu'.*
;;        Thanks rubikitch patched for this! ;)
;;
;; 2009/01/04
;;   * Andy Stewart:
;;      * Add `alternate-function' argument with function `one-key-menu'.
;;
;; 2008/12/22
;;   * Andy Stewart:
;;      * First released.
;;

;;; Acknowledgements:
;;
;;      rubikitch <rubikitch@ruby-lang.org>
;;              For send many patches.
;;

;;; TODO
;;
;; Add configurable colourization of menu items.
;; Could have alist of alists, called e.g. `one-key-colours-regexp-alist',
;; the keys to the list would be symbols for the one-key menu alists (e.g. 'one-key-menu-bookmark-alist)
;; and each value would be an alist of regexp/colour pairs.
;; Then when a menu is formatted, any items matching a regexp in the associated colours-regexp alist
;; would be coloured with the associated colour. E.g. could make items that are themselves one-key menus
;; all the same colour, and other items a different colour.
;;
;; Option to automatically split menu when creating templates based on prefix keys.
;;
;; Function to split items matching regexp into seperate menu in when editing menu in `one-key-template-mode'.
;;
;; Automatically generate one-key menus for common keybindings and store them in memory. This is already implemented
;; to a certain extent but I think it could be improved. Needs further investigation.
;;
;;; Require
(eval-when-compile (require 'cl))

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup one-key nil
  "One key."
  :group 'editing)

(defcustom one-key-popup-window t
  "Whether to popup window when `one-key-menu' is run for the first time."
  :type 'boolean
  :group 'one-key)

(defcustom one-key-buffer-name "*One-Key*"
  "The buffer name of the popup menu window."
  :type 'string
  :group 'one-key)

(defcustom one-key-template-buffer-name "*One-Key-Template*"
  "The name of the template buffer."
  :type 'string
  :group 'one-key)

(defcustom one-key-items-per-line nil
  "Number of items in one line.
If nil, it is calculated by `window-width'."
  :type 'int
  :group 'one-key)

(defcustom one-key-column-major-order t
  "If true then menu items are displayed in column major order (i.e. items will fill first column,
then second, etc.). Otherwise menu items are displayed in row major order."
  :type 'boolean
  :group 'one-key)

(defcustom one-key-menu-window-max-height nil
  "The max height of popup menu window."
  :type 'int
  :set (lambda (symbol value)
         (set symbol value)
         ;; Default is half height of frame.
         (unless value
           (set symbol (/ (frame-height) 2))))
  :group 'one-key)

(defcustom one-key-menus-location "~/.emacs.d"
  "Location in which one-key menus will be stored."
  :type 'directory
  :group 'one-key)

(defcustom one-key-menus-regexp "one-key-menu.*\\.el$"
  "Regexp to match filenames of one-key menus.
If you byte-compile your one-key menus, remember to change the ending of the regexp to elc instead of el."
  :type 'regexp
  :group 'one-key)

(defcustom one-key-auto-load-menus nil
  "If t then automatically load any files in `one-key-menus-location' with names matching `one-key-menus-regexp'."
  :type 'boolean
  :group 'one-key)

(defcustom one-key-mode-alist '((one-key-template-mode . one-key-menu-one-key-template))
  "List of modes and associated one-key menus."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'one-key)

(defcustom one-key-toplevel-alist '((("k" . "one-key") . one-key-menu-one-key))
  "The `one-key' top-level alist.
Contains list of key items for toplevel one-key menu.
Each item contains a key, description and command, in that order.
The key should be entered in the same format as that returned by `describe-key'."
  :type '(alist :key-type (cons string string) :value-type function)
  :group 'one-key)

(defcustom one-key-sort-method-alist '((key . (lambda (a b) (string< (caar a) (caar b))))
                                       (description . (lambda (a b) (string< (cdar a) (cdar b))))
                                       (command . (lambda (a b) (string< (prin1-to-string (cdr a))
                                                                             (prin1-to-string (cdr b))))))
  "An alist of sorting methods to use on the `one-key' menu items.
Each element is a cons cell of the form (NAME . PREDICATE) where NAME is a symbol for the name of the sort method,
and PREDICATE is a function which takes two items from the `one-key' menu alist as arguments and returns non-nil if
the first item should come before the second in the menu."
  :type '(alist :key-type (symbol :help-echo "Name for sort method (a symbol)")
                :value-type (function :help-echo "Predicate that returns non-nil if 1st item comes before 2nd"))
  :group 'one-key)

(defvar one-key-special-keybindings
  '(("q" "Quit one-key" (lambda (self) (keyboard-quit)))
    ("?" "Toggle display of one-key menu" (lambda (self)
                                            (one-key-menu-window-toggle title info-alist)
                                            (funcall self)))
    ("<up>" "Scroll menu up by one line" (lambda (self)
                                           (one-key-menu-window-scroll-down-line)
                                           (funcall self)))
    ("<down>" "Scroll menu down by one line" (lambda (self)
                                               (one-key-menu-window-scroll-up-line)
                                               (funcall self)))
    ("<prior>" "Scroll menu down one page" (lambda (self)
                                             (one-key-menu-window-scroll-down)
                                             (funcall self)))
    ("<next>" "Scroll menu up one page" (lambda (self)
                                          (one-key-menu-window-scroll-up)
                                          (funcall self)))
    ("C-h" "Show help for menu item on next keypress" (lambda (self)
                                                        (setq one-key-menu-show-key-help t)
                                                        (funcall self)))
    ("C-/" "Edit this one-key menu" (lambda (self) (one-key-edit-menu title)))
    ("C-s" "Save current state of this menu" (lambda (self) (one-key-save-menu title)))
    ("<f1>" "Show this help buffer" (lambda (self) (one-key-show-help) (funcall self)))
    ("<f2>" "Toggle sort ordering of items in menu"
     (lambda (self) (let ((info-alist (sort (copy-list info-alist) (one-key-get-next-sort-predicate)))
                          (origvar (intern-soft (concat "one-key-menu-" title "-alist"))))
                      (if origvar (eval `(setq ,origvar (copy-list info-alist))))
                      (setq one-key-menu-call-first-time t)
                      (one-key-menu-window-close)
                      (setq one-key-menu-title-format-string
                            (format "Here's a list of <%s> keystrokes, sorted by %s. Press <f1> for further help.\n\n"
                                    "%s" one-key-current-sort-method))
                      (funcall self))))
    ("<f3>" "Limit menu items to those matching regular expression"
     (lambda (self) (let ((filter-regex (read-regexp "Regular expression")))
                      (setq one-key-menu-call-first-time t)
                      (one-key-menu-window-close)
                      (funcall self))))
    ("<f4>" "Highlight menu items matching regular expression"
     (lambda (self) (let ((colour-regex (read-regexp "Regular expression"))
                          (bgcolour (read-color "Colour: ")))
                      (setq one-key-menu-call-first-time t)
                      (one-key-menu-window-close)
                      (loop for item in-ref info-alist
                            for str = (cdar item)
                            if (string-match colour-regex str) do
                            (setf (cdar item)
                                  (propertize str 'face (list :background bgcolour))))
                      (funcall self))))
    )

  "An list of special keys, their labels and associated functions that apply to all `one-key' menus.
Each item in the list contains (in this order):

  1) A string representation of the key (as returned by `single-key-description').

  2) A short description of the associated action.
     This description will be displayed in the *One-Key* buffer.

  3) A function for performing the action. The function will be passed a single argument which is a function
     which will recreate the current `one-key' menu when called using `funcall' (e.g. (funcall arg)).
     This function may also make use of the arguments passed to the `one-key-menu' function.

These keys and functions apply to all `one-key' menus and are not displayed with the menu specific keys.
They are for general tasks such as displaying command help, scrolling the window, sorting menu items, etc.

You may wish to temporarily alter this list when creating your own types of `one-key' menus.")

(defun one-key-save-menu (title)
  (let* ((varname (concat "one-key-menu-" title "-alist"))
         (var (intern-soft varname))
         (file (find-lisp-object-file-name var 'defvar)))
    (if file
        (with-current-buffer (find-file-noselect file)
          (goto-char (point-min))
          (if (not (search-forward (concat "(setq " varname) nil t))
              (message "Can't find menu in associated file!")
            (beginning-of-line)
            (mark-sexp)
            (kill-region (point) (marker-position (mark-marker)))
            (deactivate-mark)
            (insert (concat "(setq " varname "\n      '"
                            (replace-regexp-in-string
                             ") ((" ")\n        ((" (eval `(prin1-to-string ,var))) ")"))
            (save-buffer)))
      (message "Can't find associated source file!"))))

(defun one-key-edit-menu (title)
  (let* ((varname (concat "one-key-menu-" title "-alist"))
         (file (find-lisp-object-file-name (intern-soft varname) 'defvar)))
    (if file
        (progn (find-file-other-window file)
               (one-key-template-mode)
               (goto-char (point-min))
               (search-forward varname nil t)
               (setq one-key-menu-window-configuration nil))
      (message "Can't find associated source file!"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Faces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface one-key-title
  '((t (:foreground "Gold")))
  "Face for highlighting title."
  :group 'one-key)

(defface one-key-keystroke
  '((t (:foreground "DarkRed")))
  "Face for highlighting keystroke."
  :group 'one-key)

(defface one-key-prompt
  '((t (:foreground "khaki3")))
  "Face for highlighting prompt."
  :group 'one-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-window-configuration nil
  "The window configuration that records the current window configuration before the popup menu window.")

(defvar one-key-menu-call-first-time t
  "t if `one-key-menu' has been called non-recursively.")

(defvar one-key-menu-show-key-help nil
  "If true show help for function associated with next keystroke, when it is pressed in the one-key-menu.")

(defvar one-key-menu-title-format-string "Here's a list of <%s> keystrokes. Press <f1> for further help.\n\n"
  
  "A format string that will be displayed at the top of the `one-key' menu.
It may contain a reference to a single string (using %s) which is the title of the `one-key' menu,
i.e. the first arg to the `one-key-menu' function.
You may wish to alter this temporarily for some `one-key' menus.")

(defvar one-key-current-sort-method nil
  "The current method used to sort the items in the list")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Major Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-derived-mode one-key-template-mode emacs-lisp-mode "one-key"
  "Major mode for editing one-key menus produced by `one-key-show-template'.
\\{one-key-template-mode-map}"
  :syntax-table nil
  :abbrev-table nil
  )

(defun one-key-template-move-line-region (start end n)
  "Move the current region up or down by N lines.
If region is not active then move the current line instead."
  (interactive "r\np")
  (let ((reg (region-active-p)))
    (if (not reg)
        (progn (beginning-of-line) (setq start (point))
               (end-of-line) (forward-char) (setq end (point))))
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      (let ((start (point)))
        (insert line-text)
        (if reg (progn (setq deactivate-mark nil) (set-mark start))
          (forward-line -1))))))

(defun one-key-template-move-line-region-up nil
  "Move the current line/region up by N lines where N is the prefix arg.
If no prefix is given then N will be set to 1.
If no region is active then just the current line will be moved."
  (interactive)
  (if (not (mark)) (push-mark (point)))
  (let ((start (region-beginning)) (end (region-end)) (n current-prefix-arg))
    (one-key-template-move-line-region start end (if (null n) -1 (- n)))))

(defun one-key-template-move-line-region-down nil
  "Move the current line/region down by N lines where N is the prefix arg.
If no prefix is given then N will be set to 1.
If no region is active then just the current line will be moved."  
  (interactive)
  (if (not (mark)) (push-mark (point)))
  (let ((start (region-beginning)) (end (region-end)) (n current-prefix-arg))
    (one-key-template-move-line-region start end (if (null n) 1 n))))

(defun one-key-template-test-menu ()
  "Test the one-key menu defined in this buffer."
  (interactive)
  (eval-buffer)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^(defun \\(one-key-menu-[a-zA-Z0-9_-]+\\) " nil t)
        (funcall (intern-soft (match-string 1)))
      (message "Can't find one-key menu function definition!"))))

(defun one-key-template-mark-key-items ()
  "thisandthat."
  (interactive)
  (goto-char (point-min))
  (if (re-search-forward "^\\s-*[; ]*((\".*?\" \\. \".*?\") \\. .*?)\\s-*$" nil t)
      (progn (move-beginning-of-line nil) (set-mark (point))
             (if (re-search-forward "^\\s-*(defun one-key-menu" nil t)
                 (progn (re-search-backward "^\\s-*[; ]*((\".*?\" \\. \".*?\") \\. .*?)\\s-*$")
                        (move-beginning-of-line nil) (forward-line 1))
               (message "Can't find one-key-menu function definition!")))
    (message "Can't find one-key-menu alist!")))


(defun one-key-template-sort-key-items-by-command-alphabetically ()
  "Sort one-key key definitions in region by command name alphabetically."
  (interactive)
  (sort-regexp-fields nil "^\\(\\s-\\|;\\)+((\"\\(.+?\\)\" \\. \"\\(.+?\\)\") \\. \\(.+?\\))\\s-*$" "\\4" (region-beginning) (region-end)))

(defun one-key-template-sort-key-items-by-description-alphabetically ()
  "Sort one-key key definitions in region by description alphabetically."
  (interactive)
  (sort-regexp-fields nil "^\\(\\s-\\|;\\)+((\"\\(.+?\\)\" \\. \"\\(.+?\\)\") \\. \\(.+?\\))\\s-*$" "\\3" (region-beginning) (region-end)))

(defun one-key-template-sort-key-items-by-key-alphabetically ()
  "Sort one-key key definitions in region by key alphabetically."
  (interactive)
  (sort-regexp-fields nil "^\\(\\s-\\|;\\)+((\"\\(.+?\\)\" \\. \"\\(.+?\\)\") \\. \\(.+?\\))\\s-*$" "\\2" (region-beginning) (region-end)))

(defun one-key-template-group-key-items-by-regexps (reverse beg end regexps)
  "Group lines between positions BEG and END according to which regexp in REGEXPS they match.
The groups are then placed in the same order as in REGEXPS; top first if REVERSE is nil, or bottom first if non-nil.
When called interactively the regexp's are prompted for until a blank is entered, BEG and END are defined by the currently
active region, and REVERSE is set to t if a prefix arg is passed but nil otherwise."
  (interactive (list current-prefix-arg (region-beginning) (region-end) nil))
  (let ((n 0) (regexp t) (intp (interactive-p)))
    (while (and intp (not (equal regexp "")))
      (setq regexps
            (append regexps (list (read-string (concat "Enter regexps to match, in order (leave blank to end): ")))))
      (setq regexp (nth n regexps))
      (setq n (1+ n))))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((inhibit-field-text-motion t))
	(sort-subr reverse 'forward-line 'end-of-line nil nil
		   (lambda (str1 str2) 
                     (let ((cur 0) (match nil))
                       (while (and (< cur (length regexps)) (not match))
                         (let* ((regexp (nth cur regexps))
                                (m1 (string-match regexp (buffer-substring (car str1) (cdr str1))))
                                (m2 (string-match regexp (buffer-substring (car str2) (cdr str2)))))
                           (setq cur (1+ cur))
                           (setq match
                                 (cond ((and (not m1) (not m2)) nil)
                                       ((and m1 (not m2)) 1)
                                       ((and (not m1) m2) -1)
                                       ((< m1 m2) 1)
                                       (t -1)))))
                       (> match 0))))))))


(defun one-key-template-describe-command ()
  "Show description for command associated with one-key item on current line."
  (interactive)
  (save-excursion
    (save-restriction
      (if (re-search-forward "\\(\\(?:\\s_\\|\\sw\\)+\\))\\s-*$" nil t)
          (describe-function (intern-soft (match-string 1)))
        (message "No command found!")))))


(defun one-key-template-write ()
  "Prompt user to save current one-key menu to `one-key-menus-location' with the name one-key-menu_??.el
where ?? is the name of the menu."
  (interactive)
  (let (name)
    (goto-char (point-max))
    (if (re-search-backward "^ *(one-key-menu \"\\(.*?\\)\"" nil t)
        (progn (setq name (concat "one-key-menu_" (match-string 1) ".el"))
               (ido-file-internal 'write 'write-file one-key-menus-location "Save as: " nil name 'ignore)
               (one-key-template-mode))
      (message "No one-key-menu function found!"))))

(define-key one-key-template-mode-map (kbd "M-<up>") 'one-key-template-move-line-region-up)
(define-key one-key-template-mode-map (kbd "M-<down>") 'one-key-template-move-line-region-down)
(define-key one-key-template-mode-map (kbd "M-p") 'one-key-template-move-line-region-up)
(define-key one-key-template-mode-map (kbd "M-n") 'one-key-template-move-line-region-down)
(define-key one-key-template-mode-map (kbd "C-c C-t") 'one-key-template-test-menu)
(define-key one-key-template-mode-map (kbd "C-c SPC") 'one-key-template-mark-key-items)
(define-key one-key-template-mode-map (kbd "C-c C-SPC") 'one-key-template-mark-key-items)
(define-key one-key-template-mode-map (kbd "C-c c") 'comment-region)
(define-key one-key-template-mode-map (kbd "C-c u") 'uncomment-region)
(define-key one-key-template-mode-map (kbd "C-c C-w") 'one-key-template-write)
(define-key one-key-template-mode-map (kbd "C-c C-h") 'one-key-template-describe-command)
;; (define-prefix-command 'one-key-template-sort-map)
;; (define-key one-key-template-mode-map (kbd "C-c C-s") 'one-key-template-sort-map)
;; (define-key one-key-template-mode-map (kbd "C-c C-s c") 'one-key-template-sort-key-items-by-command-alphabetically)
;; (define-key one-key-template-mode-map (kbd "C-c C-s d") 'one-key-template-sort-key-items-by-description-alphabetically)
;; (define-key one-key-template-mode-map (kbd "C-c C-s k") 'one-key-template-sort-key-items-by-key-alphabetically)
(define-key one-key-template-mode-map (kbd "C-c C-s") 'one-key-menu-one-key-template-sort)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; one-key menus ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; menu of commands to help make one-key menus
(defvar one-key-menu-one-key-alist nil
  "The `one-key' menu alist for one-key.")

(setq one-key-menu-one-key-alist
      '(
	(("t" . "show template") . one-key-show-template)
	(("T" . "insert template") . one-key-insert-template)
	(("C-b" . "Back to toplevel menu") . one-key-menu-toplevel)
	))

(defun one-key-menu-one-key ()
  "The `one-key' menu for one-key"
  (interactive)
  (one-key-menu "one-key" one-key-menu-one-key-alist))

(defvar one-key-menu-one-key-template-alist nil
  "The `one-key' menu alist for one-key-template.")

(setq one-key-menu-one-key-template-alist
      '(
	(("C-s" . "Sort commands (C-c C-s)") . one-key-menu-one-key-template-sort)
	(("C-c c" . "Comment Region (C-c c)") . comment-region)
	(("C-c u" . "Uncomment Region (C-c u)") . uncomment-region)
	(("SPC" . "Mark key items (C-c C-SPC)") . one-key-template-mark-key-items)
	(("M-<up>" . "Move key item(s) up (M-<up>)") . one-key-template-move-line-region-up)
	(("M-<down>" . "Move key item(s) down (M-<down>)") . one-key-template-move-line-region-down)
	(("C-M-q" . "Indent sexp (C-M-q)") . indent-sexp)
        (("M-TAB" . "Completion At Point (M-TAB)") . completion-at-point)
	(("C-t" . "Test menu (C-c C-t)") . one-key-template-test-menu)
        (("C-h" . "Describe command of current item (C-c C-h)") . one-key-template-describe-command)
        (("C-w" . "Write template to one-key menus folder (C-c C-w)") . one-key-template-write)
        (("e" . "emacs-lisp-mode") . emacs-lisp-mode)
	))

(defun one-key-menu-one-key-template ()
  "The `one-key' menu for one-key-template"
  (interactive)
  (one-key-menu "one-key-template" one-key-menu-one-key-template-alist))

(defvar one-key-menu-one-key-template-sort-alist nil
  "The `one-key' menu alist for one-key-template-sort.")

(setq one-key-menu-one-key-template-sort-alist
      '(
	(("c" . "Sort items by command alphabetically (C-c C-s c)") . one-key-template-sort-key-items-by-command-alphabetically)
	(("d" . "Sort items by description alphabetically (C-c C-s d)") . one-key-template-sort-key-items-by-description-alphabetically)
	(("k" . "Sort items by key alphabetically (C-c C-s k)") . one-key-template-sort-key-items-by-key-alphabetically)
        (("g" . "Group items by regexp matches (C-c C-s g)") . one-key-template-group-key-items-by-regexps)
        (("G" . "Group items by regexp matches, reverse order (C-c C-s G)") . (lambda nil (interactive) (setq current-prefix-arg 1) (call-interactively 'one-key-template-group-key-items-by-regexps)))
	(("C-b" . "back to previous menu") . one-key-menu-one-key-template)))

(defun one-key-menu-one-key-template-sort ()
  "The `one-key' menu for one-key-template-sort"
  (interactive)
  (one-key-menu "one-key-template-sort" one-key-menu-one-key-template-sort-alist))

(defun one-key-menu-toplevel ()
  "The `one-key' toplevel menu."
  (interactive)
  (one-key-menu "toplevel" one-key-toplevel-alist))

(defun one-key-get-menu (mode)
  "Show appropriate one-key menu for current major mode."
  (interactive `(,major-mode))
  (let ((menu (assoc mode one-key-mode-alist)))
    (if menu
	(funcall (cdr menu))
      (one-key-menu-toplevel))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun one-key-show-help nil
  (interactive)
  (let ((keystr (mapconcat
                 (lambda (elt) (format "%s\t: %s" (car elt) (cadr elt)))
                 one-key-special-keybindings "\n")))
    (with-help-window (help-buffer)
      (princ (concat "Press the highlighted key in the menu to perform the corresponding action written next to it.

The following special keys may also be used:\n\n"
                     keystr)))))

(defun one-key-show-template (keystroke title)
  "Show template code in buffer `one-key-template-buffer-name'.
KEYSTROKE is keymap/keystroke that you want generate menu items for.
TITLE is title name of the menu. It can be any string you like."
  (interactive (let* ((mmode (replace-regexp-in-string "-mode" "" (symbol-name major-mode)))
                      (kmap (concat mmode "-mode-map")))
                 (list (read-string (concat "Keystroke or keymap name (" kmap "): ") nil nil kmap)
                       (read-string (concat "Title (" mmode "): ") nil nil mmode))))
  (let ((keymap (one-key-read-keymap keystroke)))
    (with-current-buffer (get-buffer-create one-key-template-buffer-name)
      ;; Load `emacs-lisp' syntax highlight, and set one-key-template-mode.
      (set-syntax-table emacs-lisp-mode-syntax-table)
      (lisp-mode-variables)
      (setq font-lock-mode t)
      (font-lock-fontify-buffer)
      (one-key-template-mode)
      ;; Insert template.
      (erase-buffer)
      (insert (concat ";; one-key menu for " title "\n\n"))
      (insert (one-key-make-template keymap title))
      (insert "\n;; Use the `one-key-get-menu' command to show menu/keybindings for this buffer.\n")
      (insert "\n;; Uncomment and edit following line to set this menu as default for mode.")
      (insert (concat "\n;;(add-to-list 'one-key-mode-alist '("
                      (replace-regexp-in-string "-map" "" keystroke) " . one-key-menu-"
                      (replace-regexp-in-string " " "-" title) "))"))
      (insert "\n;; Uncomment and edit following line to add this menu to toplevel menu.")
      (insert (concat "\n;;(add-to-list 'one-key-toplevel-alist '((\"type key here\" . \""
                      title "\") . one-key-menu-" (replace-regexp-in-string " " "-" title) "))"))
      ;; Pop to buffer.
      (switch-to-buffer (current-buffer))
      (forward-line -3)
      (beginning-of-line))))

(defun one-key-insert-template (keystroke title)
  "Insert template code.
KEYSTROKE is keymap/keystroke that you want generate menu items for.
TITLE is title name of the menu. It can be any string you like."
  (interactive (let* ((mmode (replace-regexp-in-string "-mode" "" (symbol-name major-mode)))
                      (kmap (concat mmode "-mode-map")))
                 (list (read-string (concat "Keystroke or keymap name (" kmap "): ") nil nil kmap)
                       (read-string (concat "Title (" mmode "): ") nil nil mmode))))
  (let ((keymap (one-key-read-keymap keystroke)))
    ;; Insert.
    (insert (concat ";; one-key menu for " title "\n\n"))
    (forward-line 1)
    (insert (one-key-make-template keymap title))
    (insert "\n;; Use the `one-key-get-menu' command to show menu/keybindings for this buffer.\n")
    (insert "\n;; Uncomment and edit following line to set this menu as default for mode.")
    (insert (concat "\n;;(add-to-list 'one-key-mode-alist '("
                    (replace-regexp-in-string "-map" "" keystroke) " . one-key-menu-"
                    (replace-regexp-in-string " " "-" title) "))"))
    (insert "\n;; Uncomment and edit following line to add this menu to toplevel menu.")
    (insert (concat "\n;;(add-to-list 'one-key-toplevel-alist '((\"type key here\" . \""
                    title "\") . one-key-menu-" (replace-regexp-in-string " " "-" title) "))"))
    (forward-line -3)
    (beginning-of-line)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun one-key-get-next-sort-predicate nil
  "Return the sort predicate in `one-key-sort-method-alist' that comes after `one-key-current-sort-method'.
Also set `one-key-current-sort-method' to the car of the new item."
  (let* ((pos (position one-key-current-sort-method one-key-sort-method-alist
                        :test (lambda (a b) (eq a (car b)))))
         (newpos (if (and pos (< pos (1- (length one-key-sort-method-alist))))
                     (1+ pos) 0))
         (item (nth newpos one-key-sort-method-alist)))
    (setq one-key-current-sort-method (car item))
    (cdr item)))

(defun one-key-highlight (msg msg-regexp msg-face)
  "Highlight text in string `MSG' that matches regular expression `MSG-REGEXP' with face `MSG-FACE'."
  (with-temp-buffer
    (insert msg)
    (goto-char (point-min))
    (while (re-search-forward msg-regexp nil t)
      (add-text-properties (match-beginning 0)
                           (match-end 0)
                           msg-face))
    (buffer-string)))

(defun one-key-highlight-menu (title keystroke)
  "Highlight menu TITLE and KEYSTROKE information.
Title is the name of the `one-key' menu, and KEYSTROKE is the alist of menu items."
  (setq title (one-key-highlight (format one-key-menu-title-format-string title)
                                 "\\(<[^<>]*>\\|'[^']*'\\)" '(face one-key-title)))
  (setq keystroke (one-key-highlight keystroke "\\[\\([^\\[\\]\\)*\\]" '(face one-key-keystroke)))
  (concat title keystroke))

(defun one-key-menu (title
                     info-alist
                     &optional
                     miss-match-exit-p
                     recursion-p
                     protect-function
                     alternate-function
                     execute-last-command-when-miss-match
                     filter-regex)
  "One key menu.

`TITLE' is the title of menu, can use any string.
`INFO-ALIST' is a special alist
that contains KEY, DESCRIBE and COMMAND.
`MISS-MATCH-EXIT-P' whether to hide popup menu window
when keystroke can't match in menu.
`RECURSION-P' whether recursion execute self
when keystroke can't match in menu.
`PROTECT-FUNCTION' the protect function
that call in `unwind-protect'.
`ALTERNATE-FUNCTION' the alternate function execute at last.
`EXECUTE-LAST-COMMAND-WHEN-MISS-MATCH' whether to execute the
last command when it miss matches in key alist."
  (let ((self (function (lambda () (one-key-menu
                                    title info-alist miss-match-exit-p
                                    recursion-p
                                    protect-function
                                    alternate-function
                                    execute-last-command-when-miss-match
                                    filter-regex))))
        (filtered-list (if (stringp filter-regex)
                           (remove-if-not (lambda (elt) (string-match filter-regex (cdar elt))) info-alist)
                         info-alist))
        last-key)
    (unwind-protect
        (let* ((event (read-event (if one-key-menu-call-first-time ; Just show the menu buffer when first called.
                                      (progn (setq one-key-menu-call-first-time nil)
                                             (if one-key-popup-window
                                                 (one-key-menu-window-open title filtered-list)))
                                    "")))
               (key (single-key-description event))) ; get string representation of event
          (cond ((catch 'match          ; Match user keystrokes.
                   (loop for ((match-key . desc) . command) in filtered-list do
                         (when (equal key match-key)
                           (if one-key-menu-show-key-help 
                               (progn (if (symbolp command)
                                          (describe-function command)
                                        (with-help-window (help-buffer)
                                          (princ command)))
                                      (setq one-key-menu-call-first-time nil)
                                      (setq one-key-menu-show-key-help nil)
                                      (setq recursion-p t)
                                      (throw 'match t))
                             (one-key-menu-window-close)
                             (setq one-key-menu-call-first-time t) ; allow recursive execution of `one-key-menu'
                             (call-interactively command)
                             (setq one-key-menu-call-first-time nil)
                             (throw 'match t)))) nil)
                 (one-key-handle-last alternate-function self recursion-p)) 
                ((assoc key one-key-special-keybindings) ; Match special keys
                 (funcall (caddr (assoc key one-key-special-keybindings)) self))
                (t (one-key-menu-window-close) ; If the key doesn't match...
                   (when miss-match-exit-p ; quit if `miss-match-exit-p' is `non-nil'
                     (setq last-key key)   ; record the last key
                     (keyboard-quit))      ; abort
                   (one-key-handle-last alternate-function self recursion-p)))) 
      (setq one-key-menu-call-first-time t) 
      (setq one-key-menu-show-key-help nil) 
      (one-key-menu-window-close)
      (if (and protect-function ; execute `protect-function' if it's a valid function
               (functionp protect-function))
          (call-interactively protect-function))
      (when (and execute-last-command-when-miss-match last-key) ; execute the last-key if it didn't match 
        (one-key-execute-binding-command last-key))))) ; and `execute-last-command-when-miss-match' is non-nil

(defun one-key-execute-binding-command (key)
  "Execute the command bound to KEY, unless this command is `keyboard-quit'."
  (let ((function (key-binding key)))
    (when (and (not (eq function 'keyboard-quit))
               (functionp function))
      (setq last-command-event last-input-event)
      (call-interactively function))))

(defun one-key-read-keymap (keystroke)
  "Read keymap KEYSTROKE.
If KEYSTROKE is a name of keymap, use the keymap, otherwise it's interpreted as a key stroke."
  (let ((v (intern-soft keystroke)))
    (if (and (boundp v) (keymapp (symbol-value v)))
        (symbol-value v)
      (key-binding (read-kbd-macro keystroke)))))

(defun one-key-handle-last (alternate-function recursion-function recursion-p)
  "Last function called after handling a key press in the `one-key' menu that's not listed in `one-key-special-keys-alist'.
ALTERNATE-FUNCTION is the alternative function to be executed.
RECURSION-FUNCTION is the recursion function to be executed when option RECURSION-P is non-nil."
  ;; Execute alternate function.
  (when (and alternate-function
             (functionp alternate-function))
    (call-interactively alternate-function))
  ;; Recursion execute when argument
  ;; `recursion-p' is `non-nil'.
  (if recursion-p
      (funcall recursion-function)))

(defun one-key-menu-window-exist-p nil
  "Return `non-nil' if `one-key' menu window exists, otherwise return nil."
  (and (get-buffer one-key-buffer-name)
       (window-live-p (get-buffer-window (get-buffer one-key-buffer-name)))))

(defun one-key-menu-window-toggle (title info-alist)
  "Toggle the `one-key' menu window.
Argument TITLE is the alist of keys and associated decriptions and functions.
Each item of this list is in the form: ((key . describe) . command)."
  (if (one-key-menu-window-exist-p)
      ;; Close menu window.
      (one-key-menu-window-close)
    ;; Open menu window.
    (one-key-menu-window-open title info-alist))
  nil)

(defun one-key-menu-window-open (title info-alist)
  "Open the `one-key' menu window.
Argument TITLE is a title for the `one-key' menu.
Argument INFO-ALIST is the alist of keys and associated decriptions and functions.
Each item of this list is in the form: ((key . describe) . command)."
  ;; Save current window configuration.
  (or one-key-menu-window-configuration
      (setq one-key-menu-window-configuration (current-window-configuration)))
  ;; Generate buffer information.
  (unless (get-buffer one-key-buffer-name)
    (with-current-buffer (get-buffer-create one-key-buffer-name)
      (goto-char (point-min))
      (save-excursion
        (insert (one-key-highlight-menu
                 title
                 (one-key-menu-format info-alist))))))
  ;; Pop `one-key' buffer.
  (pop-to-buffer one-key-buffer-name)
  (set-buffer one-key-buffer-name)
  ;; Adjust height of menu window
  ;; to display buffer's contents exactly.
  (fit-window-to-buffer nil one-key-menu-window-max-height)
  nil)

(defun one-key-menu-window-close ()
  "Close the menu window."
  ;; Kill menu buffer.
  (when (bufferp (get-buffer one-key-buffer-name))
    (delete-window (get-buffer-window one-key-buffer-name))
    (kill-buffer one-key-buffer-name))
  ;; Restore window layout if
  ;; `one-key-menu-window-configuration' is valid value.
  (when (and one-key-menu-window-configuration
             (boundp 'one-key-menu-window-configuration))
    (set-window-configuration one-key-menu-window-configuration)
    (setq one-key-menu-window-configuration nil)))

(defun one-key-menu-window-scroll-up ()
  "Scroll up one screen of the `one-key' menu window."
  (if (one-key-menu-window-exist-p)
      (ignore-errors
        (with-current-buffer one-key-buffer-name
          (scroll-up)))))

(defun one-key-menu-window-scroll-down ()
  "Scroll down one screen of the `one-key' menu window."
  (if (one-key-menu-window-exist-p)
      (ignore-errors
        (with-current-buffer one-key-buffer-name
          (scroll-down)))))

(defun one-key-menu-window-scroll-up-line ()
  "Scroll up one line of the `one-key' menu window."
  (if (one-key-menu-window-exist-p)
      (ignore-errors
        (with-current-buffer one-key-buffer-name
          (scroll-up 1)))))

(defun one-key-menu-window-scroll-down-line ()
  "Scroll down one line of the `one-key' menu window."
  (if (one-key-menu-window-exist-p)
      (ignore-errors
        (with-current-buffer one-key-buffer-name
          (scroll-down 1)))))

(defun one-key-menu-format (info-alist)
  "Format `one-key' menu window key description text (as displayed by the `one-key-menu' function).
Argument INFO-ALIST is an alist of keys and corresponding descriptions and functions.
Each element of this list is in the form: ((key . describe) . command)."
  (if (> (length info-alist) 0)
      (let* ((max-length (loop for ((key . desc) . command) in info-alist
                               maximize (+ (string-width key) (string-width desc))))
             (current-length 0)
             (items-per-line (or one-key-items-per-line
                                 (floor (/ (- (window-width) 3)
                                           (+ max-length 4)))))
             keystroke-msg)
        (if one-key-column-major-order
            (let* ((numitems (length info-alist))
                   (column-sizes
                    (reverse (let ((a 0) (b 0))
                               (loop for col downfrom items-per-line to 1 do
                                     (setq b (/ (- numitems a) col))
                                     (setq a (+ a b))
                                     collect b)))))
              (loop for row from 0 to (1- (nth 0 column-sizes)) with current-length
                    for ((key . desc) . command) = (nth row info-alist) do
                    (push (format "[%s] %s " key desc) keystroke-msg)
                    (setq current-length (+ (string-width key) (string-width desc)))
                    (push (make-string (- max-length current-length) ? ) keystroke-msg)
                    (loop for col from 0 to (- items-per-line 2) with colsum = row
                          for ((key1 . desc1) . command1) = (nth (+ colsum (nth col column-sizes)) info-alist) do
                          (setq colsum (+ colsum (nth col column-sizes)))
                          (if (< (+ col 1 (* row items-per-line)) numitems)
                              (progn (push (format "[%s] %s " key1 desc1) keystroke-msg)
                                     (setq current-length (+ (string-width key1) (string-width desc1)))
                                     (push (make-string (- max-length current-length) ? ) keystroke-msg))
                            (push (make-string max-length ? ) keystroke-msg)))
                    (push "\n" keystroke-msg)))
          (loop for ((key . desc) . command) in info-alist
                for counter from 1  do
                (push (format "[%s] %s " key desc) keystroke-msg)
                (setq current-length (+ (string-width key) (string-width desc)))
                (push (if (zerop (% counter items-per-line))
                          "\n"
                        (make-string (- max-length current-length) ? ))
                      keystroke-msg)))
        (mapconcat 'identity (nreverse keystroke-msg) ""))
    "No menu items!"))


(defun add-to-alist (alist-var elt-cons &optional no-replace)
  "Add to the value of ALIST-VAR an element ELT-CONS if it isn't there yet.
If an element with the same car as the car of ELT-CONS is already present,
replace it with ELT-CONS unless NO-REPLACE is non-nil; if a matching
element is not already present, add ELT-CONS to the front of the alist.
The test for presence of the car of ELT-CONS is done with `equal'."
  (let ((existing-element (assoc (car elt-cons) (symbol-value alist-var))))
    (if existing-element
        (or no-replace
            (rplacd existing-element (cdr elt-cons)))
      (set alist-var (cons elt-cons (symbol-value alist-var)))))
  (symbol-value alist-var))

(defun one-key-make-template (keymap title)
  "Generate template code.
KEYMAP is keymap you want generate.
TITLE is title name that any string you like."
  (with-temp-buffer
    (let ((indent-tabs-mode t)
          (funcname (replace-regexp-in-string " " "-" title)))
      (insert (substitute-command-keys "\\<keymap>\\{keymap}"))
      ;; Remove header/footer
      (goto-char (point-min))
      (forward-line 3)
      (delete-region 1 (point))
      (goto-char (point-max))
      (backward-delete-char 1)
      ;; Insert.
      (goto-char (point-min))
      ;; Insert alist variable.
      (insert (format "(defvar one-key-menu-%s-alist nil\n\"The `one-key' menu alist for %s.\")\n\n"
                      funcname title)
              (format "(setq one-key-menu-%s-alist\n'(\n" funcname))
      ;; Insert (("key" . "desc") . command).
      (while (not (eobp))
        (let ((pair (split-string (buffer-substring (point-at-bol) (point-at-eol)) "\t+")))
          (if (and (eq 2 (length pair)) (not (equal "" (car pair))))
              (destructuring-bind (key cmd)
                  (split-string (buffer-substring (point-at-bol) (point-at-eol)) "\t+")
                (delete-region (point-at-bol) (point-at-eol))
                (let ((keystr (replace-regexp-in-string
                               "\\\"" "\\\\\""
                               (replace-regexp-in-string "\\\\" "\\\\\\\\" key))))
                  (insert (format "((\"%s\" . \"%s (%s)\") . %s)" 
                                  keystr
                                  (capitalize (replace-regexp-in-string "-" " " cmd))
                                  keystr
                                  cmd)))
                (when (and cmd
                           (string-match " " (concat key cmd)))
                  (forward-sexp -1)
                  (insert ";; ")))))
        (forward-line 1))
      (goto-char (point-max))
      (insert "))\n\n")
      ;; Insert function.
      (insert (format "(defun one-key-menu-%s ()\n\"The `one-key' menu for %s\"\n(interactive)\n(one-key-menu \"%s\" one-key-menu-%s-alist))\n" funcname title title funcname))
      ;; Indent.
      (emacs-lisp-mode)
      (indent-region (point-min) (point-max))
      ;; Result.
      (buffer-string)
      )))

;; This function is used to load the one-key menus at startup if `one-key-auto-load-menus' is set to t.
(defun one-key-load-files (filepattern)
  "Load all files with paths matching FILEPATTERN"
  (let ((directory (file-name-directory filepattern))
	(fp (file-name-nondirectory filepattern))
	filename
	(files nil))
    (setq files (directory-files directory nil fp nil))
    (if (not files)
	(if find-files-ignore-no-match
	    (load-file filepattern)
	  (error "No matching files for `%s'!" filepattern)))
    (while files
      (setq filename (car files))
      (if directory (setq filename (concat directory filename)))
      (if (not (file-directory-p filename))
	  (progn (message "Reading %s" filename)
		 (if (string-match "XEmacs" emacs-version)
		     (load-file filename)
		   (load-file filename))))
      (setq files (cdr files)))))

;; Load all one-key menus if necessary.
(if one-key-auto-load-menus
    (one-key-load-files (concat one-key-menus-location "/.*" one-key-menus-regexp)))
  

(provide 'one-key)

;;; one-key.el ends here

;;; LocalWords:  emms specpdl minish DarkRed msg FUNCITN num str decf elt args
;;; LocalWords:  rubikitch's desc SPC bmenu sKeymap nsTitle fontify funcname
;;; LocalWords:  bol eol destructuring cmd PageUp PageDown
