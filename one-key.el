;;; one-key.el --- One key

;; Filename: one-key.el
;; Description: One key
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;;         rubikitch <rubikitch@ruby-lang.org>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2012, Joe Bloggs, all rites reversed.
;; Copyright (C) 2008, 2009, 2010 Andy Stewart, all rights reserved.
;; Copyright (C) 2009, rubikitch, all rights reserved.
;; Created: 2008-12-22 21:54:30
;; Version: 0.7.1
;; Last-Updated: 5/4/2012 17:00:00
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/one-key.el
;; Keywords: one-key
;; Compatibility: GNU Emacs 22 ~ 23
;;
;; Features that might be required by this library:
;;
;; `cl' `hexrgb'
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
;; This package fixes that problem.
;;
;; One Key provides a single keystroke that when pressed presents you with
;; a menu of choices in a popup window for commands to execute with a further keystrokes.
;;
;; Just type one of the listed keystrokes to execute the corresponding command.
;;
;; You can delete, edit, sort, highlight and filter the menu items, add new menu items and even add new menus.
;; You can have access to several different menus from the same window which can be navigated with the arrow keys.
;; Such a collection of menus is called a menu set and you can define several different menu sets containing different
;; types of menus.
;; Several different types of menus are defined (and more may be added) for holding different types of menu items.
;; The "major-mode" type opens the menu corresponding to the current major-mode, the "menu-sets" type opens a menu
;; for accessing the currently defined menu sets, and the "top-level" type opens the top-level menu of user defined
;; items/menus. More different types are defined by one-key extension libraries (e.g. `one-key-dir').

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
;;   (one-key-menu "emms" 'one-key-menu-emms-alist t))
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
;; (one-key-menu "MENU-NAME" 'MENU-ALIST)
;;
;; Example:
;;
;; (defun example-menu ()
;;   (interactive)
;;   (one-key-menu "example" 'example-menu-alist)
;;
;; ** The arguments of the function `one-key-menu':
;;
;; `name' is the name of menu, any string you like.
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
;; After entering the keymap/prefix key you are prompted for a name for the menu, and then code for the menu
;; will be automatically generated.
;; E.g. if you type "C-x r", and then enter the name "bookmark" then it will generate template code
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
;;   (one-key-menu "BOOKMARK" 'one-key-menu-bookmark-alist))
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
;; they can be configured with the `one-key-default-special-keybindings' variable.
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
;; `one-key-keystroke-face' : face for highlighting keystroke
;; `one-key-auto-load-menus' : if t then automatically load one-key menus from `one-key-menus-location'
;; `one-key-menus-location' : location in which one-key menus will be stored
;; `one-key-menus-regexp' : regexp to match filenames of one-key menus
;; `one-key-mode-alist' : An alist of major-mode, one-key menu pairs to set the default menu for each major-mode.
;; `one-key-toplevel-alist' : A list of key items for the toplevel menu.
;; `one-key-popup-window' : whether to popup window when first time run, default is `t'.
;; `one-key-prompt-face' : face for highlighting prompt
;; `one-key-template-buffer-name' : the buffer name of the template code.
;; `one-key-name-face' : face for highlighting name
;; `one-key-default-special-keybindings' : special keybindings and associated descriptions and functions that apply to
;;                                 all `one-key' menus

;; All above options can be customized through:
;;      M-x customize-group RET one-key RET
;;

;;; Change log:
;; 2012/04/05
;;    * Joe Bloggs
;;    * Lots of changes! I have not been keeping track of them all.
;;    * Removed `one-key-items-per-line'
;;
;; 2012/3/01
;;    * Joe Bloggs
;;       * Lots of changes! Improved menu layout (can fit in more items), different menu sorting options,
;;       * colourization of menu items, limit items to those matching regexp, edit menu items in place,
;;       * manual repositioning of menu items in place.
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
;; Make sure only menu functions begin with `one-key-menu' (rename others)
;;
;; Autohighlighting of menu items using regexp associations (e.g. highlight items corresponding to further one-key menus).
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
(require 'hexrgb)
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup one-key nil
  "One key - easy access, refactorable menus."
  :group 'editing)

(defcustom one-key-default-menu-keys
  (let (letters-and-numbers)
    (dotimes (i 26)
      (push (- ?Z i) letters-and-numbers))
    (dotimes (i 26)
      (push (- ?z i) letters-and-numbers))
    (dotimes (i 10)
      (push (- ?9 i) letters-and-numbers))
    letters-and-numbers)
  "A list of chars which may be used as the default keys in automatically generated `one-key' menus.
This list will be used for generating keys by the `one-key-generate-key' function."
  :group 'one-key
  :type '(repeat character))

(defcustom one-key-excluded-keys '("<remap>" "mouse")
  "List of strings matching keys that should be excluded from `one-key' menus that are automatically generated from keymaps.
When `one-key' menus are automatically generated with the `one-key-create-menus-from-keymap' function, keys that match any of these
strings will be excluded from the final menus."
  :group 'one-key
  :type '(repeat (regexp :tag "Regexp" :help-echo "A regular expression matching keys to be excluded.")))

(defcustom one-key-popup-window t
  "Whether to popup window when `one-key-menu' is run for the first time."
  :type 'boolean
  :group 'one-key)

(defcustom one-key-buffer-name "*One-Key*"
  "The buffer name of the popup menu window."
  :type 'string
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

(defcustom one-key-menus-save-file "~/.emacs.d/one-key-menus-save-file.el"
  "The file where `one-key' menus are saved."
  :type 'file
  :group 'one-key)

(defcustom one-key-autosave-menus nil
  "If non-nil then one-key menus will automatically be saved when created or changed."
  :type 'boolean
  :group 'one-key)

(defcustom one-key-exclude-from-save nil
  "List of regular expressions matching names of menus which should not be autosaved."
  :type '(repeat (regexp :tag "Regexp" :help-echo "Regular expression matching menu names to exclude from autosave." ))
  :group 'one-key)


(defcustom one-key-item-foreground-colour "black"
  "Foreground colour of highlighted items in `one-key' menus."
  :type 'color
  :group 'one-key)

(defcustom one-key-auto-brighten-used-keys t
  "If non-nil then set brightness of menu items colours according to how often the keys are pressed."
  :type 'boolean
  :group 'one-key)

(defcustom one-key-submenus-replace-parents nil
  "If non-nil then when a submenu of a `one-key' menu is opened it will replace the parent menu.
Otherwise a new menu is created to hold the submenu and added to the current menu set."
  :type 'boolean
  :group 'one-key)

(defcustom one-key-major-mode-remap-alist '((Custom-mode . "custom-mode")
                                            (latex-mode ."LaTeX-mode"))
  "A list of cons cells mapping major modes to one-key-menus.
The car of each cell is the symbol of a major mode function (e.g. 'emacs-lisp-mode), and the cdr is the name of
a `one-key' menu associated with the major mode.
When a menu of type \"major-mode\" is opened this alist is checked, and if the current major mode is listed then the
associated menu will be used, otherwise the menu alist with name one-key-menu-???-alist (where ??? is the name of the
current major mode) will be used (and created if necessary)."
  :type '(alist :key-type (function :tag "Major mode" :help-echo "A major mode function") :value-type (string :tag "Name of associated menu" :help-echo "The name of the menu to be associated with the major mode"))
  :group 'one-key)

(defcustom one-key-toplevel-alist '((("k" . "one-key") . one-key-menu-one-key))
  "The `one-key' top-level alist.
Contains list of key items for toplevel one-key menu.
Each item contains a key, description and command, in that order.
The key should be entered in the same format as that returned by `describe-key'."
  :type '(alist :key-type (cons string string) :value-type function)
  :group 'one-key)

(defcustom one-key-sets-of-menus-alist (list '("Major mode, top-level & menu sets" "major-mode" "top-level" "menu-sets"))
  "Saved menu sets (sets of menus).
Each element in this list is a cons cell whose car is a name or description for the set, and whose cdr is a list of names
of menus which make up the set. Each menu name must correspond to a type in `one-key-types-of-menu' (which see),
and `one-key' must be able to reconstruct the menu from the name (which it will be able to if the corresponding entry
in `one-key-types-of-menu' is complete.
These menu sets may be opened from the `one-key-menu-set' menu, and you may want to create different sets for different
projects."
  :type '(alist :key-type (string :tag "Set description/name" :help-echo "A name or description for this collection of menus")
                :value-type (repeat (string :tag "Menu" :help-echo "The name of the menu. Must correspond to a type in `one-key-types-of-menu'.")))
  :group 'one-key)

(defcustom one-key-default-menu-set "Major mode, top-level & menu sets"
  "The default menu set. It's value should be the car of one of the items in `one-key-sets-of-menus-alist'.
It may be changed by the user from the menu-sets `one-key' menu.
This is only meaningful if it is used with `one-key-open-menu-set' bound to a key so that the key can open a different
menu set if the user has altered its value."
  :type 'string
  :group 'one-key)

(defcustom one-key-default-sort-method-alist '((key . (lambda (a b) (string< (caar a) (caar b))))
                                               (description . (lambda (a b) (string< (cdar a) (cdar b))))
                                               (command . (lambda (a b) (string< (prin1-to-string (cdr a))
                                                                                 (prin1-to-string (cdr b)))))
                                               (colour_name . (lambda (a b) (string< (cadr (get-text-property 0 'face (cdar a)))
                                                                                     (cadr (get-text-property 0 'face (cdar b))))))
                                               (colour_hue . (lambda (a b)
                                                               (let* ((bg (cdr (assq 'background-color (frame-parameters))))
                                                                      (cola (or (cadr (get-text-property 0 'face (cdar a))) bg))
                                                                      (colb (or (cadr (get-text-property 0 'face (cdar b))) bg))
                                                                      (hsva (destructuring-bind (r g b) (color-values cola)
                                                                              (color-rgb-to-hsv r g b)))
                                                                      (hsvb (destructuring-bind (r g b) (color-values colb)
                                                                              (color-rgb-to-hsv r g b))))
                                                                 (> (first hsva) (first hsvb)))))
                                               (colour_brightness . (lambda (a b)
                                                                      (let* ((bg (cdr (assq 'background-color (frame-parameters))))
                                                                             (cola
                                                                              (or (cadr (get-text-property 0 'face (cdar a))) bg))
                                                                             (colb
                                                                              (or (cadr (get-text-property 0 'face (cdar b))) bg))
                                                                             (hsva (destructuring-bind (r g b)
                                                                                       (color-values cola)
                                                                                     (color-rgb-to-hsv r g b)))
                                                                             (hsvb (destructuring-bind (r g b)
                                                                                       (color-values colb)
                                                                                     (color-rgb-to-hsv r g b))))
                                                                        (> (third hsva) (third hsvb)))))
                                               (colour_saturation . (lambda (a b)
                                                                      (let* ((bg (cdr (assq 'background-color (frame-parameters))))
                                                                             (cola
                                                                              (or (cadr (get-text-property 0 'face (cdar a))) bg))
                                                                             (colb
                                                                              (or (cadr (get-text-property 0 'face (cdar b))) bg))
                                                                             (hsva (destructuring-bind (r g b)
                                                                                       (color-values cola)
                                                                                     (color-rgb-to-hsv r g b)))
                                                                             (hsvb (destructuring-bind (r g b)
                                                                                       (color-values colb)
                                                                                     (color-rgb-to-hsv r g b))))
                                                                        (> (second hsva) (second hsvb)))))
                                               (length . (lambda (a b) (> (length (cdar a)) (length (cdar b))))))
  "An alist of sorting methods to use on the `one-key' menu items.
Each element is a cons cell of the form (NAME . PREDICATE) where NAME is a symbol for the name of the sort method,
and PREDICATE is a function which takes two items from the `one-key' menu alist as arguments and returns non-nil if
the first item should come before the second in the menu."
  :type '(alist :key-type (symbol :help-echo "Name for sort method (a symbol)")
                :value-type (function :help-echo "Predicate that returns non-nil if 1st item comes before 2nd"))
  :group 'one-key)

(defcustom one-key-default-special-keybindings
  '(("ESC" "Quit and close menu window" (lambda nil (keyboard-quit) nil))
    ("<C-escape>" "Quit, but keep menu window open"
     (lambda nil (setq keep-window-p t) nil))
    ("<C-menu>" "Toggle menu persistence"
     (lambda nil (if match-recursion-p
                     (setq match-recursion-p nil
                           miss-match-recursion-p nil)
                   (setq match-recursion-p t
                         miss-match-recursion-p t))))
    ("<menu>" "Toggle menu display" (lambda nil (one-key-menu-window-toggle) t))
    ("<left>" "Change to next menu" (lambda nil (if menu-number
                                                    (progn
                                                      (setq menu-number
                                                            (if (equal menu-number 0)
                                                                (1- (length info-alists))
                                                              (1- menu-number)))
                                                      (setq one-key-menu-call-first-time t))) t))
    ("<right>" "Change to previous menu" (lambda nil (if menu-number
                                                         (progn
                                                           (setq menu-number
                                                                 (if (equal menu-number (1- (length info-alists)))
                                                                     0 (1+ menu-number)))
                                                           (setq one-key-menu-call-first-time t))) t))
    ("<up>" "Scroll/move up one line" (lambda nil (one-key-scroll-or-move-up info-alist full-list) t))
    ("<down>" "Scroll/move down one line" (lambda nil (one-key-scroll-or-move-down info-alist full-list) t))
    ("<prior>" "Scroll menu down one page" (lambda nil (one-key-menu-window-scroll-down) t))
    ("<next>" "Scroll menu up one page" (lambda nil (one-key-menu-window-scroll-up) t))
    ("C-h" "Show help for next item chosen" (lambda nil
                                              (let ((key (read-key "Enter the key for the item that you want help on")))
                                                (one-key-show-item-help key full-list)
                                                (setq match-recursion-p t)) t))
    ("C-s" "Save current state of menu" (lambda nil (one-key-save-menu this-name info-alist full-list) t))
    ("<f1>" "Toggle this help buffer" (lambda nil (if (get-buffer-window "*Help*")
                                                      (kill-buffer "*Help*")
                                                    (one-key-show-help special-keybindings)) t))
    ("<f2>" "Toggle column/row ordering of items"
     (lambda nil (if one-key-column-major-order
                     (setq one-key-column-major-order nil)
                   (setq one-key-column-major-order t))
       (setq one-key-menu-call-first-time t) t))
    ("<f3>" "Sort items by next method"
     (lambda nil (setq one-key-current-sort-method
                       (one-key-sort-items-by-next-method info-alists full-list menu-number)) t))
    ("<C-f3>" "Sort items by previous method"
     (lambda nil (setq one-key-current-sort-method
                       (one-key-sort-items-by-next-method info-alists full-list menu-number t)) t))
    ("<f4>" "Reverse order of items"
     (lambda nil (one-key-reverse-item-order info-alists full-list menu-number) t))
    ("<f5>" "Limit items to those matching regexp"
     (lambda nil (setq filter-regex (read-regexp "Regular expression"))
       (setq one-key-menu-call-first-time t) t))
    ("<f6>" "Highlight items matching regexp"
     (lambda nil (let ((regex (read-regexp "Regular expression"))
                       (bgcolour (read-color "Colour: ")))
                   (one-key-highlight-matching-items
                    info-alist full-list bgcolour
                    (lambda (item) (string-match regex (cdar item))))) t))
    ("<f7>" "Edit a menu item"
     (lambda nil (one-key-edit-menu-item info-alist full-list) t))
    ("<f8>" "Delete a menu item"
     (lambda nil (one-key-delete-menu-item info-alist full-list) t))
    ("<f9>" "Swap menu item keys"
     (lambda nil (one-key-swap-menu-items info-alist full-list) t))
    ("<f10>" "Add a menu item"
     (lambda nil (one-key-prompt-to-add-menu-item info-alist full-list) t))
    ("<C-f10>" "Add a menu"
     (lambda nil (one-key-add-menus) nil)) ; no need to return t since `one-key-add-menus' does recursion itself
    ("<C-S-f10>" "Remove this menu"
     (lambda nil (one-key-delete-menu) t))
    ("<f11>" "Reposition item (with arrow keys)"
     (lambda nil (let ((key (single-key-description
                             (read-key "Enter key of item to be moved"))))
                   (setq one-key-current-item-being-moved key)
                   (setq one-key-menu-call-first-time t)) t)))

  "An list of special keys, their labels and associated functions that apply to all `one-key' menus.
Each item in the list contains (in this order):

  1) A string representation of the key (as returned by `single-key-description').

  2) A short description of the associated action.
     This description will be displayed in the *One-Key* buffer.

  3) A function for performing the action. The function takes no arguments but may use dynamic binding to
     read and change some of the values in the initial `one-key-menu' function call.
     The function should return t to display the `one-key' menu again after the function has finished,
     or nil to close the menu.

These keys and functions apply to all `one-key' menus and are not displayed with the menu specific keys.
They are for general tasks such as displaying command help, scrolling the window, sorting menu items, etc.

You may wish to temporarily alter this list when creating your own types of `one-key' menus."
  :group 'one-key
  :type '(repeat (list (string :tag "Keybinding" :help-echo "String representation of the keybinding for this action")
                       (string :tag "Description" :help-echo "Description to display in help buffer")
                       (function :tag "Function" :help-echo "Function for performing action. See description below for further details."))))

(defcustom one-key-menu-sets-special-keybindings
  '(("ESC" "Quit and close menu window" (lambda nil (keyboard-quit) nil))
    ("<C-escape>" "Quit, but keep menu window open"
     (lambda nil (setq keep-window-p t) nil))
    ("<C-menu>" "Toggle menu persistence"
     (lambda nil (if match-recursion-p
                     (setq match-recursion-p nil
                           miss-match-recursion-p nil)
                   (setq match-recursion-p t
                         miss-match-recursion-p t))))
    ("<menu>" "Toggle menu display" (lambda nil (one-key-menu-window-toggle) t))
    ("<left>" "Change to next menu" (lambda nil (if menu-number
                                                    (progn
                                                      (setq menu-number
                                                            (if (equal menu-number 0)
                                                                (1- (length info-alists))
                                                              (1- menu-number)))
                                                      (setq one-key-menu-call-first-time t))) t))
    ("<right>" "Change to previous menu" (lambda nil (if menu-number
                                                         (progn
                                                           (setq menu-number
                                                                 (if (equal menu-number (1- (length info-alists)))
                                                                     0 (1+ menu-number)))
                                                           (setq one-key-menu-call-first-time t))) t))
    ("<up>" "Scroll/move up one line" (lambda nil (one-key-scroll-or-move-up info-alist full-list) t))
    ("<down>" "Scroll/move down one line" (lambda nil (one-key-scroll-or-move-down info-alist full-list) t))
    ("<prior>" "Scroll menu down one page" (lambda nil (one-key-menu-window-scroll-down) t))
    ("<next>" "Scroll menu up one page" (lambda nil (one-key-menu-window-scroll-up) t))
    ("C-h" "Show menus in menu set" (lambda nil
                                      (let* ((key (read-key "Enter the key for the menu set"))
                                             (item (one-key-get-menu-item key full-list))
                                             (menuset (assoc (cdar item) one-key-sets-of-menus-alist))
                                             (desc (car menuset))
                                             (names (cdr menuset)))
                                        (message "%S" names) t)))
    ("C-s" "Customize menu sets" (lambda nil
                                   (setq one-key-menu-window-configuration nil)
                                   (with-selected-window (previous-window)
                                     (customize-variable 'one-key-sets-of-menus-alist)) nil))
    ("<f1>" "Toggle this help buffer" (lambda nil (if (get-buffer-window "*Help*")
                                                      (kill-buffer "*Help*")
                                                    (one-key-show-help special-keybindings)) t))
    ("<f2>" "Toggle column/row ordering of items"
     (lambda nil (if one-key-column-major-order
                     (setq one-key-column-major-order nil)
                   (setq one-key-column-major-order t))
       (setq one-key-menu-call-first-time t) t))
    ("<f3>" "Sort items by next method"
     (lambda nil (setq one-key-current-sort-method
                       (one-key-sort-items-by-next-method info-alists full-list menu-number)) t))
    ("<C-f3>" "Sort items by previous method"
     (lambda nil (setq one-key-current-sort-method
                       (one-key-sort-items-by-next-method info-alists full-list menu-number t)) t))
    ("<f4>" "Reverse order of items"
     (lambda nil (one-key-reverse-item-order info-alists full-list menu-number) t))
    ("<f5>" "Limit items to those matching regexp"
     (lambda nil (setq filter-regex (read-regexp "Regular expression"))
       (setq one-key-menu-call-first-time t) t))
    ("<f6>" "Highlight items matching regexp"
     (lambda nil (let ((regex (read-regexp "Regular expression"))
                       (bgcolour (read-color "Colour: ")))
                   (one-key-highlight-matching-items
                    info-alist full-list bgcolour
                    (lambda (item) (string-match regex (cdar item))))) t))
    ("<f7>" "Change default menu set"
     (lambda nil
       (let* ((key (read-event "Press the key of item to set as default"))
              (item (one-key-get-menu-item key full-list))
              (name (cdar item))
              (pos (position "menu-sets" names :test 'equal)))
         (if name (eval `(customize-save-variable 'one-key-default-menu-set
                                                  ,(substring-no-properties name))))
         (if pos (setf (nth pos info-alists) (one-key-build-menu-sets-menu-alist))
           (setq info-alists (one-key-build-menu-sets-menu-alist))))
       (setq one-key-menu-call-first-time t)
       (one-key-menu-window-close) t))
    ("<C-f10>" "Add a menu"
     (lambda nil (one-key-add-menus) nil)) ; no need to return t since `one-key-add-menus' does recursion itself
    ("<C-S-f10>" "Remove this menu"
     (lambda nil (one-key-delete-menu) t)))

  "An list of special keys, their labels and associated functions that apply to all `one-key' menus.
Each item in the list contains (in this order):

  1) A string representation of the key (as returned by `single-key-description').

  2) A short description of the associated action.
     This description will be displayed in the *One-Key* buffer.

  3) A function for performing the action. The function takes no arguments but may use dynamic binding to
     read and change some of the values in the initial `one-key-menu' function call.
     The function should return t to display the `one-key' menu again after the function has finished,
     or nil to close the menu.

These keys and functions apply to all `one-key' menus and are not displayed with the menu specific keys.
They are for general tasks such as displaying command help, scrolling the window, sorting menu items, etc.

You may wish to temporarily alter this list when creating your own types of `one-key' menus."
  :group 'one-key
  :type '(repeat (list (string :tag "Keybinding" :help-echo "String representation of the keybinding for this action")
                       (string :tag "Description" :help-echo "Description to display in help buffer")
                       (function :tag "Function" :help-echo "Function for performing action. See description below for further details."))))

(defcustom one-key-default-title-format-string
  (lambda nil (format "Sorted by %s (%s first). Press <f1> for help.\n" one-key-current-sort-method (if one-key-column-major-order "columns" "rows")))
  "A function that takes no arguments and should return a string to display at the top of the menu window.
The function will be evaluated in the context of the `one-key-highlight-menu' function, and will be processed by
`one-key-highlight' before display.
You should look at the `one-key-highlight-menu' function to see which variables may be used in this format string."
  :type 'function
  :group 'one-key)

(defcustom one-key-types-of-menu nil
  "A list of names of different types of `one-key' menu, and associated functions.
Each item in the list contains (in this order):

  1) Either a name for the menu type or a function which takes a name as its only argument and if that name corresponds
     to this menu type it should return a new name or list of names for the menu/menus returned by the next item in the
     list.

  2) A function which takes the menu name as its only argument and returns a cons cell whose car is the new name or list
     of names for the menus, and whose cdr is a menu alist, a symbol whose value is a menu alist, or a list of symbols
     and/or menu alists. The number of names returned in the car should be equal to the number of menu alists/symbols
     returned in the cdr.

  3) An function that takes no arguments and returns a title string for the `one-key' menu in the same form as
     `one-key-default-title-format-string'. The function will be evaluated in the context of the `one-key-highlight-menu'
     function, and will be processed by `one-key-highlight' before display.
     You should look at the `one-key-highlight-menu' function to see which variables may be used in this format string.
     Alternatively if this item is nil then `one-key-default-title-format-string' will be used.

  4) Either a list of special keybindings in the same form as `one-key-default-special-keybindings', or a symbol
     whose value is such a list, or nil. If nil then `one-key-default-special-keybindings' will be used."
  :type '(repeat (list (choice (string :tag "Name"
                                       :help-echo "A name for this menu type.")
                               (function :tag "Condition"
                                         :help-echo "A function which returns the new menu name(s) when passed a name corresponding to this type, and returns nil otherwise."))
                       (choice (symbol :tag "Menu alist symbol(s)"
                                       :help-echo "A symbol whose value is a menu alist of keys for menus of this type, or a list of such menus.")
                               (function :tag "Menu alist function"
                                         :help-echo "A function which takes the menu name as its only argument and returns either a `one-key' menu alist of keys, or a symbol whose value is such a list, or a list of menus and/or symbols."))
                       (function :tag "Title string function"
                                 :help-echo "A function which returns a title string for `one-key' menus of this type.")
                       (choice (symbol :tag "Special keybindings symbol"
                                       :help-echo "A symbol whose value is a list of special keybindings for menus of this type")
                               (repeat :tag "Special keybindings"
                                       (list (string :tag "Keybinding"
                                                     :help-echo "String representation of the keybinding for this action")
                                             (string :tag "Description"
                                                     :help-echo "Description to display in help buffer")
                                             (function :tag "Function"
                                                       :help-echo "Function for performing action. See description below for further details."))))))
  :group 'one-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Faces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface one-key-name
  '((t (:foreground "Gold")))
  "Face for highlighting name."
  :group 'one-key)

(defface one-key-keystroke
  '((t (:foreground "DarkRed")))
  "Face for highlighting keystroke."
  :group 'one-key)

(defface one-key-prompt
  '((t (:foreground "khaki3")))
  "Face for highlighting prompt."
  :group 'one-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Global Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar one-key-menu-window-configuration nil
  "Variable that records the window configuration that was in place before the popup menu window was opened.")

(defvar one-key-menu-call-first-time t
  "t if `one-key-menu' has been called non-recursively.")

(defvar one-key-menu-show-key-help nil
  "If true show help for function associated with next keystroke, when it is pressed in the one-key-menu.")

(defvar one-key-current-sort-method nil
  "The current method used to sort the items in the `one-key' menu list")

(defvar one-key-current-item-being-moved nil
  "The key corresponding to the item currently being moved in the `one-key' menu, or nil if none is being moved.")

(defvar one-key-current-window-state nil
  "The current state of the `one-key' window
If nil then the window is closed, if t then it is open at normal size, if 'full then it is open at full size.")

(defvar one-key-altered-menus nil
  "List of menu alist variables that should be saved on exit if `one-key-autosave-menus' is true.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun one-key-show-help (special-keybindings)
  "Show information about `one-key-menu' special keybindings in the alist SPECIAL-KEYBINDINGS."
  (interactive)
  (let* ((maxkey (loop for elt in special-keybindings
                       maximize (1+ (length (car elt)))))
         (maxstr (loop for elt in special-keybindings
                       maximize (+ 3 maxkey (length (cadr elt)))))
         (width (/ (window-width) 2))
         (keystr (if (> maxstr width)
                     (mapconcat (lambda (elt) (format "%s\t: %s" (car elt) (cadr elt)))
                                special-keybindings "\n")
                   (loop with colsize = (+ (/ (length special-keybindings) 2)
                                           (% (length special-keybindings) 2))
                         with finalstr
                         for n from 0 to (1- colsize)
                         for (key1 desc1) = (nth n special-keybindings)
                         for (key2 desc2) = (nth (+ n colsize) special-keybindings)
                         for keyspc1 = (make-string (- maxkey (length key1)) ? )
                         for keyspc2 = (make-string (- maxkey (length key2)) ? )
                         for str1 = (format "%s%s: %s" key1 keyspc1 desc1)
                         for str2 = (format "%s%s: %s" key2 keyspc2 desc2)
                         for spc = (make-string (- width (length str1)) ? ) do
                         (push (concat str1 spc (if key2 str2) "\n") finalstr)
                         finally return (mapconcat 'identity (nreverse finalstr) "")))))
    (with-help-window (help-buffer)
      (princ (concat "Press the highlighted key in the menu to perform the corresponding action written next to it.

The following special keys may also be used:\n\n"
                     keystr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun one-key-add-to-alist (alist-var elt-cons &optional no-replace)
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

(defun one-key-scroll-or-move-up (info-alist full-list)
  "Either scroll the `one-key' menu window down by one line or move an item down.
If `one-key-current-item-being-moved' contains a string representation of one of the keys in the menu move that item
down one line, otherwise scroll the window down one line.
FULL-LIST is as in the `one-key-menu' function."
  (let* ((key one-key-current-item-being-moved)
         (pos (position-if (lambda (x) (equal (caar x) key)) full-list)))
    (if pos
        (let* ((len (length full-list))
               (prevpos (if (eq pos 0) (1- len) (1- pos)))
               (item (nth pos full-list))
               (previtem (nth prevpos full-list))
               (copy (copy-list item)))
          (setf (car item) (car previtem) (cdr item) (cdr previtem)
                (car previtem) (car copy) (cdr previtem) (cdr copy))
          (setq one-key-menu-call-first-time t)
          (one-key-menu-window-close)
          (if (symbolp info-alist)
              (add-to-list 'one-key-altered-menus info-alist)))
      (one-key-menu-window-scroll-down-line))
    (setq protect-function (lambda nil (interactive) (setq one-key-current-item-being-moved nil)))))

(defun one-key-scroll-or-move-down (info-alist full-list)
  "Either scroll the `one-key' menu window down by one line or move an item down.
If `one-key-current-item-being-moved' contains a string representation of one of the keys in the menu move that item
down one line, otherwise scroll the window down one line.
FULL-LIST is as in the `one-key-menu' function."
  (let* ((key one-key-current-item-being-moved)
         (pos (position-if (lambda (x) (equal (caar x) key)) full-list)))
    (if pos
        (let* ((len (length full-list))
               (nextpos (if (eq pos (1- len)) 0 (1+ pos)))
               (item (nth pos full-list))
               (nextitem (nth nextpos full-list))
               (copy (copy-list item)))
          (setf (car item) (car nextitem) (cdr item) (cdr nextitem)
                (car nextitem) (car copy) (cdr nextitem) (cdr copy))
          (setq one-key-menu-call-first-time t)
          (one-key-menu-window-close)
          (if (symbolp info-alist)
              (add-to-list 'one-key-altered-menus info-alist)))
      (one-key-menu-window-scroll-up-line))
    (setq protect-function (lambda nil (interactive) (setq one-key-current-item-being-moved nil)))))

(defun one-key-show-item-help (key menu-alist)
  "Show help for item in MENU-ALIST that is associated with the key KEY.
MENU-ALIST should be a menu of items as used by the `one-key-menu' function."
  (let* ((item (one-key-get-menu-item key menu-alist))
         (cmd (cdr item)))
    (if (and (symbolp cmd) (commandp cmd))
        (describe-function cmd)
      (with-help-window (help-buffer)
        (princ cmd)))))

(defun one-key-prompt-to-add-menu-item (info-alist full-list)
  "Prompt the user for item details and add it to the current `one-key' menu.
INFO-ALIST and FULL-LIST are as in the `one-key-menu' function."
  (let* ((isref (symbolp info-alist))
         (newkey (let ((key (read-key "Enter the key for the new item")))
                   (while (and (one-key-get-menu-item key full-list)
                               (not (y-or-n-p "That key is already used! Overwrite old item?")))
                     (setq key (read-key "Enter new key for the item")))
                   key))
         (desc (read-string "Item description: "))
         (contents (read-from-minibuffer "Command: " nil nil t)))
    (if isref
        (progn (add-to-list 'one-key-altered-menus info-alist)
               (set info-alist (one-key-add-menu-item newkey desc contents full-list)))
      (setq info-alist (one-key-add-menu-item newkey desc contents full-list))))
    (setq one-key-menu-call-first-time t)
    (one-key-menu-window-close))

(defun one-key-swap-menu-items (info-alist full-list)
  "Prompt user for a pair of items in the `one-key' menu and swap the corresponding keys.
INFO-ALIST and FULL-LIST are as in the `one-key-menu' function."
  (let* ((keya (read-event "Press key for first item"))
         (keyastr (single-key-description keya))
         (itema (one-key-get-menu-item keyastr full-list))
         (keyb (read-key "Press key for second item"))
         (keybstr (single-key-description keyb))
         (itemb (one-key-get-menu-item keybstr full-list)))
    (if (not (and itema itemb)) (message "Invalid key!")
      (setf (caar itema) keybstr (caar itemb) keyastr))
    (if (symbolp info-alist)
        (add-to-list 'one-key-altered-menus info-alist))
    (setq one-key-menu-call-first-time t)
    (one-key-menu-window-close)))

(defun one-key-delete-menu-item (info-alist full-list)
  "Prompt the user for an item to delete from the `one-key' menu.
INFO-ALIST and FULL-LIST are as in the `one-key-menu' function."
  (let* ((isref (symbolp info-alist))
         (key (read-event "Press the key of the item you want to delete"))
         (item (one-key-get-menu-item key full-list)))
    (if (and item (y-or-n-p (format "Delete item \"%s\"?" (cdar item))))
        (if isref (set info-alist (delete item full-list))
          (setq info-alist (delete item full-list))))
    (if isref (add-to-list 'one-key-altered-menus info-alist))
    (setq one-key-menu-call-first-time t)
    (one-key-menu-window-close)))

(defun one-key-edit-menu-item (info-alist full-list)
  "Prompt user for the key of a menu item to edit, make changes and then reopen `one-key' menu.
INFO-ALIST and FULL-LIST are as in the `one-key-menu' function."
  (let* ((oldkey (read-event "Press the key of the item you want to edit"))
         (item (one-key-get-menu-item oldkey full-list))
         (newkey (let ((key (read-key "Enter new key for the item")))
                   (while (and (one-key-get-menu-item key full-list)
                               (not (eq key oldkey))
                               (not (y-or-n-p "That key is already used! Use it anyway?")))
                     (setq key (read-key "Enter new key for the item")))
                   key))
         (desc (read-string "Item description: " (cdar item) nil nil))
         (oldcontents (cdr item))
         (contents (read-from-minibuffer "Item contents: " (format "%S" oldcontents) nil t)))
    (setf (caar item) (single-key-description newkey))
    (setf (cdar item) desc)
    (setf (cdr item) contents))
  (if (symbolp info-alist)
      (add-to-list 'one-key-altered-menus info-alist))
  (setq one-key-menu-call-first-time t)
  (one-key-menu-window-close))

(defun one-key-highlight-matching-items (info-alist full-list colour pred)
  "Highlight items in FULL-LIST with colour COLOUR using predicate function PRED to select items.
The predicate function should take a single item from FULL-LIST as it's only argument.
INFO-ALIST and FULL-LIST are as in the `one-key-menu' function.
If COLOUR is \"\" then all highlighting (and more generally any text properties) are removed from the item."
  (loop for item in-ref full-list
        for str = (cdar item)
        if (funcall pred item) do
        (if (equal colour "")
            (setf (cdar item) (substring-no-properties str))
          (setf (cdar item)
                (propertize str 'face (list :background colour :foreground one-key-item-foreground-colour)))))
  (if (symbolp info-alist)
      (add-to-list 'one-key-altered-menus info-alist))
  (setq one-key-menu-call-first-time t)
  (one-key-menu-window-close))

(defun one-key-save-menu (name info-alist full-list)
  "Save the current `one-key' menu to the file `one-key-menus-save-file'.
NAME is the name of the current `one-key' menu, INFO-ALIST and FULL-LIST are as in the `one-key-menu' function."
  (let* ((varname (if (symbolp info-alist) (symbol-name info-alist)
                    (concat "one-key-menu-" name "-alist")))
         (file one-key-menus-save-file)
         (buf (get-file-buffer file)))
    (if file
        (if (file-exists-p file)
            (if (file-writable-p file)
                (with-current-buffer (find-file-noselect file)
                  (goto-char (point-min))
                  (if (not (search-forward (concat "(setq " varname) nil t))
                      (goto-char (point-max))
                    (beginning-of-line)
                    (mark-sexp)
                    (kill-region (point) (marker-position (mark-marker)))
                    (deactivate-mark))
                  (insert (concat "(setq " varname "\n      '"
                                  (replace-regexp-in-string
                                   ") ((" ")\n        ((" (eval `(prin1-to-string full-list))) ")"))
                  (save-buffer)
                  (if (not buf) (kill-buffer (get-file-buffer file))))
              (message "Can't write to file %s" file))
          (message "Can't file file %s" file))
      (message "`one-key-menus-save-file' not set" file))))

(defun one-key-get-next-alist-item (currentcar allitems-alist &optional prev)
  "Get the item in ALLITEMS-ALIST which comes after the item with car equal to CURRENTCAR.
If CURRENTCAR is the car of the last item in ALLITEMS-ALIST, then return the first item in the list.
If PREV is non-nil then return the previous item instead of the next item (returning the last item if CURRENTCAR is
the car of the first item)."
  (let* ((pos (position currentcar allitems-alist
                        :test (lambda (a b) (equal a (car b)))))
         (len (length allitems-alist))
         (newpos (if prev (if (and pos (> pos 0))
                              (1- pos) (1- len))
                   (if (and pos (< pos (1- len)))
                       (1+ pos) 0))))
    (nth newpos allitems-alist)))

(defun* one-key-sort-items-by-next-method (info-alists full-list menu-number &optional prev)
  "Sort the items in the current `one-key' menu.
This function is called in the context of the `one-key-menu', where INFO-ALISTS, FULL-LIST and MENU-NUMBER are defined.
Sort the items in FULL-LIST according to the method in `one-key-default-sort-method-alist' that comes after `one-key-current-sort-method', or the previous method if PREV is non-nil.
Return the symbol corresponding to the sort method used."
  (let* ((info-alist (if menu-number (nth menu-number info-alists) info-alists))
         (isref (symbolp info-alist))
         (nextmethod (one-key-get-next-alist-item
                      one-key-current-sort-method
                      one-key-default-sort-method-alist prev))
         (sorted-list (sort (copy-list full-list)
                            (cdr nextmethod)))
         (major (if one-key-column-major-order "columns" "rows")))
    (if isref (progn (set info-alist sorted-list)
                     (add-to-list 'one-key-altered-menus info-alist))
      (if menu-number
          (setf (nth menu-number info-alists) sorted-list)
        (setq info-alists sorted-list)))
    (setq one-key-menu-call-first-time t)
    (one-key-menu-window-close)
    (car nextmethod)))

(defun one-key-reverse-item-order (info-alists full-list menu-number)
  "Reverse the order of items in the current `one-key' menu.
This function is called in the context of the `one-key-menu', where INFO-ALISTS, FULL-LIST and MENU-NUMBER are defined.
The function will reverse the order of the items in FULL-LIST, and then update the MENU-NUMBER'th item in INFO-ALIST,
or the list that it points to (if its value is a symbol)."
  (let* ((info-alist (nth menu-number info-alists))
         (isref (symbolp info-alist))
         (reversed-list (reverse full-list)))
    (if isref (progn (set info-alist reversed-list)
                     (add-to-list 'one-key-altered-menus info-alist))
      (if menu-number
          (setf (nth menu-number info-alists) reversed-list)
        (setq info-alists reversed-list)))
    (setq one-key-menu-call-first-time t)
    (one-key-menu-window-close)))

(defun one-key-get-menu-type (name)
  "Return the element of ``one-key-types-of-menu' corresponding to menu with name NAME, or nil if none exists."
  (assoc-if (lambda (x) (or (equal x name)
                            (and (functionp x)
                                 (funcall x name))))
            one-key-types-of-menu))

(defun one-key-get-menus-for-type (name)
  "Given the name of an existing menu or menu type in `one-key-types-of-menu', return associated names and menu alists.
If no such menu or menu type exists, return nil."
  (let* ((listname (concat "one-key-menu-" name "-alist"))
         (func (or (second (one-key-get-menu-type name))
                   (loop for sym being the symbols
                         for symname = (symbol-name sym)
                         when (equal listname symname)
                         return (cons name sym))
                   (error "Invalid menu name: \"%s\"" name))))
    (if (functionp func) (funcall func name) func)))

(defun one-key-prompt-for-menu nil
  "Prompt the user for a `one-key' menu type."
  (let* ((alltypes (remove nil
                           (mapcar (lambda (x) (let ((y (car x))) (if (stringp y) y)))
                                   one-key-types-of-menu)))
         (type (if (featurep 'ido)
                   (ido-completing-read "Menu type: " alltypes)
                 (completing-read "Menu type: " alltypes))))
    (one-key-get-menus-for-type type)))
    
(defun one-key-add-menus (&optional newnames newlists)
  "Add a menu/menus to the current list of menus in the `one-key' menu function call.
This function assumes dynamic binding of the `info-alists', `menu-number' and `names' arguments to the `one-key-menu'
function, and is called within that function."
  (let* ((both (if (and newnames newlists)
                   (cons newnames newlists)
                 (one-key-prompt-for-menu)))
         (newnames (car both))
         (newlists (cdr both))
         (multi (listp newnames)))
    (if menu-number
        (let* ((listlen (length info-alists))
               (namen (length names))
               (titnum (min menu-number namen)))
          (setq names
                (concatenate 'list
                             (subseq names 0 (1+ titnum))
                             (if multi newnames (list newnames))
                             (subseq names (1+ titnum) namen))
                info-alists
                (concatenate 'list
                             (subseq info-alists 0 (1+ menu-number))
                             (if multi newlists (list newlists))
                             (subseq info-alists (1+ menu-number) listlen))
                menu-number (1+ menu-number)))
      (setq menu-number 1
            info-alists (if multi (concatenate 'list (list info-alists) newlists)
                          (list info-alists newlists))
            names (if multi (concatenate 'list (list this-name) newnames)
                    (list this-name newnames)))))
  (setq one-key-menu-call-first-time t)
  (one-key-handle-last nil self t))

(defun* one-key-delete-menu (&optional (name this-name))
  "Remove the menu with name NAME from the list of menus, or the current menu if NAME is not supplied.
This function will only work if called within the context of the `one-key-menu' function since it depends on the dynamic
binding of the info-alists, menu-number and names variables."
  (if menu-number
      (let* ((listlen (length info-alists))
             (namen (length names))
             (pos (if name (position name names :test 'equal)
                    (min menu-number (1- namen)))))
        (setq names
              (concatenate 'list
                           (subseq names 0 pos)
                           (subseq names (1+ pos) namen))
              info-alists
              (concatenate 'list
                           (subseq info-alists 0 pos)
                           (subseq info-alists (1+ pos) listlen))
              menu-number (min pos (- listlen 2))
              this-name name
              one-key-menu-call-first-time t))
    (one-key-menu-close)))
    
(defun one-key-open-menus (names)
  "Invoke `one-key-menu' with names and corresponding menu-alists.
NAMES should be the name of a single `one-key' menu or menu type, or a list of such names.
If called interactively a single name will be prompted for."
  (let* ((names (if (stringp names) (list names) names))
         (pairs (mapcar 'one-key-get-menus-for-type names))
         (names (mapcan (lambda (x) (let ((y (car x))) (if (stringp y) (list y) y))) pairs))
         (alists (mapcan (lambda (x) (let ((a (car x)) (b (cdr x)))
                                       (if (stringp a) (list b) b))) pairs)))
    (one-key-menu names alists)))

(defun one-key-open-menu-set (menuset)
  "Open `one-key' menus defined by `one-key' menu set MENUSET.
MENUSET should be the car of an element of `one-key-sets-of-menus-alist'.
If called interactively, MENUSET will be prompted for."
  (interactive (list (if (featurep 'ido)
                         (ido-completing-read "Menu set: " (mapcar 'car one-key-sets-of-menus-alist))
                       (completing-read "Menu set: " (mapcar 'car one-key-sets-of-menus-alist)))))
  (let* ((item (assoc menuset one-key-sets-of-menus-alist))
         (names (cdr item)))
    (one-key-open-menus names)))

(defun one-key-open-default-menu-set nil
  "Open the menu set defined by `one-key-default-menu-set'."
  (interactive)
  (one-key-open-menu-set one-key-default-menu-set))

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

(defun one-key-highlight-menu (name keystroke &optional names)
  "Highlight menu NAME and KEYSTROKE information, and return contents for insertion in *One-Key* buffer.
NAME is the name of the currently selected menu, if multiple menus are in use then NAMES should be
the list of corresponding menu names (which will include NAME).
KEYSTROKE is the alist of menu items."
  (let* ((namefunc (lambda (x) (if (equal x name) (propertize x 'face 'one-key-name) x)))
         (names1 (if names
                     (if (listp names) names (list names))
                   (list name)))
         (names2 (mapcar namefunc names1))
         (namesline (if names2 (concat "| " (mapconcat 'identity names2 " | ") " |")
                         (propertize name 'face 'one-key-name)))
         (namelen (length name))
         (winwidth (window-width))
         (namepos (/ (- winwidth namelen) 2))
         (namepos2 (if names2 (next-property-change 0 namesline)))
         (namepos3 (if names2 (+ namepos2 namelen)))
         (namediff (if names2 (- namepos2 namepos)))
         (nameendpos (if names2 (+ winwidth namediff)))
         (centredline (if names2 (concat (if (< namediff 0) (make-string (- namediff) ? ))
                                          (substring namesline (max 0 namediff) namepos2)
                                          (substring namesline namepos2 namepos3)
                                          (substring namesline namepos3 (min nameendpos (length namesline)))
                                          "\n")
                        (concat (make-string namepos ? ) namesline "\n")))
         (title-func (or (third (one-key-get-menu-type name))
                         one-key-default-title-format-string))
         (infoline (one-key-highlight (funcall title-func)
                                      "\\(<[^<>]*>\\|'[^']*'\\)" '(face one-key-name)))
         (keystrokelist (one-key-highlight keystroke "\\[\\([^\\[\\]\\)*?\\]" '(face one-key-keystroke))))
    (concat centredline infoline keystrokelist)))

(defun* one-key-menu (names
                      info-alists
                      &key
                      (menu-number (if ; hack to check if info-alists is a list of lists or just a single list
                                       (and (listp info-alists) (not (and (listp (car info-alists))
                                                                          (listp (caar info-alists))
                                                                          (stringp (caaar info-alists)))))
                                       0 nil))
                      keep-window-p
                      execute-when-miss-match-p
                      miss-match-recursion-p
                      match-recursion-p
                      protect-function
                      alternate-function
                      filter-regex)
  "Function to open `one-key' menu of commands. The commands are executed by pressing the associated keys.
If global variable `one-key-popup-window' is t (default) then a menu window will be displayed showing the keybindings.
NAMES is the name of the menu as displayed in the menu window, or a list of names corresponding to different menu
lists in INFO-ALISTS.
INFO-ALISTS is either a list of menu items, a list of such lists or a symbol whose value is a list or list of lists.
Each item in a menu list is of the form: ((key . description) . command).
If INFO-ALISTS is a list then MENU-NUMBER should be an index (starting at 0) indicating which list to display
initially (default is 0), otherwise it should be nil. 
The user can switch between the menu lists by pressing the appropriate keys in `one-key-default-special-keybindings'.
If KEEP-WINDOW-P is non-nil then the menu window will be kept open even after exiting.
If EXECUTE-WHEN-MISS-MATCH-P is nil then keys not matching menu items or `one-key-default-special-keybindings' will be ignored, otherwise the associated commands in the current keymaps will be executed.
The arguments MATCH-RECURSION-P and MISS-MATCH-RECURSION-P indicate whether to execute `one-key-menu' recursively after
a matching or non-matching key are pressed (and corresponding commands executed) respectively.
PROTECT-FUNCTION, if non-nil, is a function that is called within an `unwind-protect' statement at the end
of `one-key-menu'.
ALTERNATE-FUNCTION if non-nil is a function that is called after each key press while the menu is active.
If FILTER-REGEX is non-nil then only menu items whose descriptions match FILTER-REGEX will be displayed."
  (let* ((info-alist (if menu-number
                         (nth menu-number info-alists)
                       info-alists))
         (issymbol (symbolp info-alist))
         (full-list (if issymbol 
                        (eval info-alist)
                      info-alist))
         (this-name (if (stringp names) names
                       (nth menu-number names)))
         ;; the list of items after filtering
         (filtered-list (if (stringp filter-regex)
                            (remove-if-not
                             (lambda (elt) (string-match filter-regex (cdar elt)))
                             full-list)
                          full-list))
         ;; the special keybindings for this menu
         (special-keybindings-var (or (fourth (one-key-get-menu-type this-name))
                                      one-key-default-special-keybindings))
         (special-keybindings (if (symbolp special-keybindings-var)
                                  (eval special-keybindings-var)
                                special-keybindings-var))
         ;; following function is used for recursively calling itself when needed
         (self (function (lambda () (one-key-menu names info-alists
                                                  :menu-number menu-number
                                                  :keep-window-p keep-window-p
                                                  :execute-when-miss-match-p execute-when-miss-match-p
                                                  :miss-match-recursion-p miss-match-recursion-p
                                                  :match-recursion-p match-recursion-p
                                                  :protect-function protect-function
                                                  :alternate-function alternate-function
                                                  :filter-regex filter-regex)))))
    (unwind-protect
        ;; read a key and get the key description
        (let* ((namelist (if (listp names) names nil))
               (event (read-key (if one-key-menu-call-first-time
                                    ;; just show the menu buffer when first called
                                    (progn (setq one-key-menu-call-first-time nil)
                                           (if one-key-popup-window
                                               (one-key-menu-window-open))))))
               (key (single-key-description event)))
          (cond (
                 ;; HANDLE KEYSTROKES MATCHING MENU ITEMS
                 ;; (unless the help window is open)
                 (and (not (get-buffer-window "*Help*"))
                      (catch 'match
                        (loop for item in filtered-list
                              for match-key = (caar item)
                              for desc = (cdar item)
                              for rest = (cdr item)
                              for command = (if (commandp rest) rest
                                              (if (one-key-list-longer-than-1-p rest)
                                                  (car rest)
                                                (lambda nil (interactive) (message "Invalid command %S" rest)))) do
                              (when (equal key match-key)
                                ;; Update key usage statistics if necessary
                                (if one-key-auto-brighten-used-keys
                                    (progn (one-key-menu-increment-key-usage item)
                                           (if issymbol
                                               (add-to-list 'one-key-altered-menus info-alist))))
                                ;; We need to close the `one-key' menu window before running the items command.
                                ;; Save previous state of the `one-key' window before doing this.
                                (let* ((old-one-key-popup-window one-key-popup-window)
                                       (one-key-popup-window (one-key-menu-window-exist-p)))
                                  (one-key-menu-window-close)
                                  (setq one-key-menu-call-first-time t) ; allow recursive execution of `one-key-menu'
                                  (call-interactively command) ; call the items command
                                  ;; reopen the `one-key' window if necessary
                                  (if (and one-key-popup-window (or keep-window-p match-recursion-p))
                                      (one-key-menu-window-open))
                                  (setq one-key-popup-window old-one-key-popup-window))
                                (setq one-key-menu-call-first-time nil)
                                ;; throw t if the key matched so that this clause's body is executed, otherwise return nil
                                (throw 'match t))) nil))
                 ;; call the `alternate-function' and if `match-recursion-p' is non-nil wait for next keypress
                 (let ((temp (one-key-handle-last alternate-function self match-recursion-p)))
                   ;; if necessary, propagate the value of `keep-window-p' back up
                   (if match-recursion-p
                       (setq keep-window-p temp))))
                ;; HANDLE SPECIAL KEYS:
                ((assoc key special-keybindings)
                 ;; call the `alternate-function' and the function associated with the special key
                 ;; if this function returns non-nil then wait for the next keypress
                 (let* ((again (funcall (caddr (assoc key special-keybindings))))
                        (temp (one-key-handle-last alternate-function self again)))
                   ;; if necessary, propagate the value of `keep-window-p' back up
                   (if again (setq keep-window-p temp))))
                ;; HANDLE ALL OTHER KEYS:
                (t
                 (when execute-when-miss-match-p
                   ;; If `execute-when-miss-match-p' is non-nil then execute the normal command for this key
                   ;; Need to close the `one-key' menu window before running the command.
                   ;; Save the previous state of the `one-key' window before doing this.
                   (let* ((old-one-key-popup-window one-key-popup-window)
                          (one-key-popup-window (one-key-menu-window-exist-p)))
                     (one-key-menu-window-close)
                     (one-key-execute-binding-command key)
                     ;; reopen the `one-key' window if necessary
                     (if (and one-key-popup-window (or keep-window-p miss-match-recursion-p))
                         (one-key-menu-window-open))
                     (setq one-key-popup-window old-one-key-popup-window)))
                 ;; call the `alternate-function' and if `miss-match-recursion-p' is non-nil wait for next keypress
                 (let ((temp (one-key-handle-last alternate-function self miss-match-recursion-p)))
                   ;; if necessary, copy the value of `keep-window-p' from recursive call
                   (if miss-match-recursion-p
                       (setq keep-window-p temp))))))
      ;; all keypresses have now been handled so reset global variables ready for next time
      (setq one-key-menu-call-first-time t)
      (setq one-key-menu-show-key-help nil)
      ;; If `keep-window-p' is non-nil then don't close the `one-key' window,
      ;; just change the focus to the previous window.
      (if keep-window-p (if (equal (buffer-name (window-buffer)) one-key-buffer-name) (other-window -1))
        (one-key-menu-window-close))
      ;; Finally, execute `protect-function' if it's a valid function.
      (if (and protect-function 
               (functionp protect-function))
          (call-interactively protect-function)))
    ;; propagate the value of `keep-window-p' back down the stack
    keep-window-p))

(defun one-key-execute-binding-command (key)
  "Execute the command bound to KEY (a string description of a key), unless this command is `keyboard-quit'.
If KEY contains shift, and there is no command bound to that key, then the same key binding with shift removed
will be tried (in accordance with normal emacs behaviour)."
  (let* ((rawkey (elt (eval `(kbd ,key)) 0))
         (mods (event-modifiers rawkey))
         (basic (event-basic-type rawkey))
         (func (key-binding (vector rawkey)))
         (keynoshift (nconc (remove 'shift mods) (list basic) nil))
         (func2 (or func
                    (key-binding (vector (event-convert-list keynoshift))))))
    (when (and (not (eq func2 'keyboard-quit))
               (functionp func2))
      (setq last-command-event last-input-event)
      (call-interactively func2))))

(defun one-key-read-keymap (keystroke)
  "Read keymap KEYSTROKE.
If KEYSTROKE is the name of a keymap, use the keymap, otherwise it's interpreted as a key stroke."
  (let ((v (intern-soft keystroke)))
    (if (and (boundp v) (keymapp (symbol-value v)))
        (symbol-value v)
      (key-binding (read-kbd-macro keystroke)))))

(defun one-key-handle-last (alternate-function recursion-function recursion-p)
  "Last function called after handling a key press in the `one-key' menu that's not listed in `one-key-special-keys-alist'.
ALTERNATE-FUNCTION is the alternative function to be executed.
RECURSION-FUNCTION is the recursion function to be executed when option RECURSION-P is non-nil.
The return value of RECURSION-FUNCTION will be returned by this function also."
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

(defun one-key-menu-window-toggle nil
  "Toggle the `one-key' menu window."
  (if one-key-current-window-state
      (if (and (stringp one-key-current-window-state)
               (get-buffer one-key-current-window-state))
          (one-key-menu-window-close)
        (setq one-key-current-window-state
              (with-selected-window (previous-window) (buffer-name)))
        (fit-window-to-buffer (get-buffer-window one-key-buffer-name)
                              (frame-height)
                              one-key-menu-window-max-height))
    (one-key-menu-window-open)))

(defun one-key-menu-window-open nil
  "Open the `one-key' menu window."
  ;; Save current window configuration.
  (or one-key-menu-window-configuration
      (setq one-key-menu-window-configuration
            (current-window-configuration)))
  ;; Update key brightnesses if necessary
  (if one-key-auto-brighten-used-keys
      (one-key-menu-brighten-most-used info-alist))
  ;; Generate buffer information.
  (with-current-buffer (get-buffer-create one-key-buffer-name)
    (erase-buffer)
    (goto-char (point-min))
    (save-excursion
      (insert (one-key-highlight-menu
               this-name (one-key-menu-format filtered-list) names))))
  ;; Pop `one-key' buffer.
  (pop-to-buffer one-key-buffer-name)
  (set-buffer one-key-buffer-name)
  ;; Adjust height of menu window appropriately.
  ;; If `one-key-current-window-state' is a string then we have switched
  ;; from another menu at full height, and so should make this window full height too.
  (if (stringp one-key-current-window-state)
      (fit-window-to-buffer nil (frame-height) one-key-menu-window-max-height)
    (fit-window-to-buffer nil one-key-menu-window-max-height)
    (setq one-key-current-window-state t))
  nil)

(defun one-key-menu-window-close nil
  "Close the menu window."
  ;; Kill menu buffer.
  (when (bufferp (get-buffer one-key-buffer-name))
    (if (and (stringp one-key-current-window-state)
             (get-buffer one-key-current-window-state))
        (pop-to-buffer one-key-current-window-state))
    (delete-window (get-buffer-window one-key-buffer-name))
    (kill-buffer one-key-buffer-name)
    (setq one-key-current-window-state nil))
  ;; Restore window layout if `one-key-menu-window-configuration' is valid value.
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

(defun one-key-menu-increment-key-usage (item)
  "Increment the key usage statistic for ITEM."
  (destructuring-bind ((key . desc) . rest) item
    (if (commandp rest)
        (setf (cdr item) (list rest 1))
      (if (and (one-key-list-longer-than-1-p rest)
               (numberp (second rest)))
          (progn (incf (second rest))
                 (setf (cdr item) rest))))))

(defsubst one-key-list-longer-than-1-p (x)
  "Return t if x is a list of length > 1, and is not a command."
  (and (not (commandp x))
       (listp x)
       (listp (cdr x))
       (> (length x) 1)))

(defun one-key-menu-brighten-most-used (info-alist)
  "Set values of menu item colours proportionally according to how often they have been used.
Argument INFO-ALIST is the alist of keys and associated decriptions and functions, or a symbol referencing the list."
  ;; first get min and max keypress values
  (if info-alist
      (let ((menu-alist (if (symbolp info-alist) (eval info-alist) info-alist)))
        (if menu-alist
            (let* ((minmaxvals (loop for ((key . desc) . rest) in menu-alist
                                     for val = (if (one-key-list-longer-than-1-p rest)
                                                   (second rest) 0)
                                     maximize val into max
                                     minimize val into min
                                     finally return (list min max)))
                   (minval (first minmaxvals))
                   (maxval (second minmaxvals))
                   (range (- maxval minval)))
              ;; update the colour value (from HSV) of each item in menu-alist
              (loop for item in menu-alist
                    for ((key . desc) . rest) = item
                    ;; get current keypress value (indicating number of times key has been pressed)
                    for val = (if (one-key-list-longer-than-1-p rest)
                                  (second rest) 0)
                    ;; calculate colour value from keypress value
                    for vval = (if (= range 0)
                                   0.5
                                 (+ (/ (- val minval) (* 2.0 range)) 0.5))
                    ;; get current HSV values
                    for descface = (get-text-property 0 'face desc)
                    for oldbgcol = (or (and (facep descface)
                                            (not (equal (face-attribute descface :background) 'unspecified))
                                            (face-attribute descface :background))
                                       (plist-get descface :background)
                                       (cdr (assq 'background-color (frame-parameters))))
                    for fgcol = (or (and (facep descface)
                                         (not (equal (face-attribute descface :foreground) 'unspecified))
                                         (face-attribute descface :foreground))
                                    (plist-get descface :foreground)
                                    one-key-item-foreground-colour)
                    for (h s v) = (hexrgb-hex-to-hsv oldbgcol)
                    ;; set new HSV values to update colour value
                    for newbgcol = (hexrgb-hsv-to-hex h s vval) do
                    (setf (cdar item)
                          (propertize desc 'face (list :background newbgcol
                                                       :foreground fgcol)))))))))

(defun one-key-optimize-col-widths (lengths maxlength)
  "Given a list of the lengths of the menu items, work out the maximum possible number of columns and return their widths.
Actually the function returns a list of cons cells in the form (numrows . width) each of which corresponds to a column in
the optimal assignment and indicates the number of rows and width of that column."
  (let ((nitems (length lengths))
        (ncols 1)
        bestcols)
    (if (< (apply '+ lengths) maxlength)
        (mapcar (lambda (len) (cons 1 len)) lengths)
      (while (progn
               (setq ncols (1+ ncols))
               (let ((itemspercol (make-list ncols (/ nitems ncols)))
                     colspecs)
                 (loop for n to (1- (% nitems ncols)) do (incf (nth n itemspercol) 1))
                 (setq colspecs (loop for colnum to (1- ncols) with sofar = 0
                                      for nrows = (nth colnum itemspercol)
                                      for colwidth = (loop for rownum to (1- nrows)
                                                           for itemnum = (if one-key-column-major-order
                                                                             (+ sofar rownum)
                                                                           (+ colnum (* rownum ncols)))
                                                           maximize (nth itemnum lengths))
                                      summing colwidth into width
                                      collecting (cons nrows colwidth) into specs
                                      do (setq sofar (+ sofar nrows))
                                      finally (return (list width specs))))
                 (if (< (car colspecs) maxlength) (setq bestcols (cadr colspecs)) nil))))
      (or bestcols (list (cons nitems (1- maxlength)))))))

(defun one-key-menu-format (info-alist)
  "Format `one-key' menu window key description text (as displayed by the `one-key-menu' function).
Argument INFO-ALIST is an alist of keys and corresponding descriptions and functions, or a symbol referencing that list.
Each element of this list is in the form: ((key . describe) . command)."
  (let ((items-alist (if (symbolp info-alist) (eval info-alist) info-alist)))
    (if (> (length items-alist) 0)
        (let* ((item-lengths (mapcar (lambda (item) (+ (length (cdar item))
                                                       (length (caar item)) 4)) items-alist))
               (colspecs (one-key-optimize-col-widths item-lengths (- (window-width) 3)))
               (numitems (length items-alist))
               (maxcols (length colspecs))
               (maxrow (caar (last colspecs)))
               (extras (% numitems maxcols))
               keystroke-msg)
          (loop for row from 0 to maxrow
                for ncols = (if (= row maxrow) extras maxcols) do
                (loop for col from 0 to (1- ncols) with sofar = 0
                      for (colsize . width) = (nth col colspecs)
                      for itemnum = (if one-key-column-major-order
                                        (+ sofar row)
                                      (+ (* row maxcols) col))
                      for item = (nth itemnum items-alist)
                      for (key . desc) = (car item)
                      for keytext = (format "[%s] %s " key desc)
                      if item do
                      (push keytext keystroke-msg)
                      (push (make-string (- width (length keytext)) ? ) keystroke-msg)
                      (setq sofar (+ sofar colsize)))
                (push "\n" keystroke-msg))
          (mapconcat 'identity (nreverse keystroke-msg) ""))
      "No menu items!")))

(defun one-key-get-menu-item (key menu-alist)
  "Return the member of MENU-ALIST corresponding to key KEY, or nil if no such item exists.
KEY may be a char or the string representation of a char.
MENU-ALIST is a list of `one-key' menu items."
  (let ((thekey (if (stringp key) key (single-key-description key))))
    (find-if (lambda (x) (equal (caar x) thekey)) menu-alist)))

(defun one-key-add-menu-item (key desc contents menu-alist)
  "Add a new item to MENU-ALIST in the form ((KEY . DESC) . CONTENTS), overwriting any item with the same key.
Return the new value of MENU-ALIST after adding the item.
KEY may be a char or the string representation of a char.
DESC must be a string (the description to display in the menu).
CONTENTS may be a command or a list whose first element is a command (it will be executed when KEY is pressed in the menu)."
  (let* ((thekey (if (stringp key) key (single-key-description key)))
         (item (one-key-get-menu-item thekey menu-alist)))
    (if item (progn (setf (cdar item) desc (cdr item) contents) menu-alist)
      (add-to-list 'menu-alist (cons (cons thekey desc) contents)))))

(defun one-key-open-submenu (name var)
  "Open a menu named NAME with menu alist variable VAR as a submenu of the current menu, replacing it if necessary.
If `one-key-submenus-replace-parents' is non-nil then the current menu will be replaced with the submenu, otherwise
a new menu will be added to the current menu set.
This function will only work if called within the context of the `one-key-menu' function since it depends on the variable
THIS-NAME being dynamically bound."
  (let ((currname this-name))
  (one-key-add-menus name var)
  (if one-key-submenus-replace-parents
      (one-key-delete-menu currname))))

(defun* one-key-create-menus-from-keymap (keymap &optional (name (if (symbolp keymap)
                                                         (substring (symbol-name keymap) 0 -4)
                                                       "unknown")))
  "Create a menu alist for a keymap and all sub menu alists."
  (let* ((case-fold-search nil)
         (keymap1 (if (symbolp keymap) (eval keymap) keymap))
         (nulllines '("digit-argument" "^\\s-.*" "^$" "^key\\s-+binding" "^[-\\s-]+"))
         (nulllines2 (mapconcat 'identity nulllines "\\|"))
         (keystr (substitute-command-keys "\\<keymap1>\\{keymap1}"))
         (lines (string-split keystr "\n"))
         (lines2 (remove-if (lambda (line) (string-match nulllines2 line)) lines))
         (lines3 (mapcar (lambda (line) (string-split line "  +" 2)) lines2))
         (pred1 (lambda (line) (string-match "Prefix Command\\|-map$" (cadr line))))
         (submenulines (remove-if-not pred1 lines3))
         (submenulines2 (sort submenulines (lambda (a b) (> (length (car a)) (length (car b))))))
         (lines4a (remove-if pred1 lines3))
         (nullkeys (regexp-opt one-key-excluded-keys))
         (lines4 (remove-if (lambda (line) (string-match nullkeys (car line))) lines4a))
         ;; function to convert '(key cmd) pairs into menu items
         (keymapname (replace-regexp-in-string "mode-map$\\|map$" "" (symbol-name keymap)))
         (keymapnameregex (regexp-opt (list keymapname (capitalize keymapname))))
         (converter (lambda (line) (let* ((keystr (car line))
                                          (cmdname (cadr line))
                                          (cmd (intern-soft cmdname))
                                          (cmdname2 (replace-regexp-in-string keymapnameregex "" cmdname))
                                          (desc (capitalize
                                                 (replace-regexp-in-string "-" " " cmdname2)))
                                          (desc2 (concat desc " (" keystr ")"))
                                          (lastkey (car (last (string-split keystr " ")))))
                                     (cons (cons lastkey desc2) cmd))))
         ;; remove duplicate entries (keep one with shortest keystring)
         (lines5 (loop for line in lines4 with lines4b
                       for key = (car line)
                       for cmd = (cadr line)
                       for func = (lambda (x) (equal (car x) cmd))
                       for elm = (rassoc-if func lines4b)
                       for key2 = (car elm)
                       if elm do (if (< (length key) (length key2))
                                     (setf (car elm) key))
                       else do (push line lines4b)
                       finally return lines4b))
         ;; split items into submenus corresponding with prefix keys in submenulines2
         (pair (loop for line in submenulines2
                     for key1 = (car line)
                     for key2 = (replace-regexp-in-string "ESC$" "M-" key1)
                     for keyregex = (concat "^\\("
                                            (regexp-opt (list (concat key1 " ") key2))
                                            "\\)")
                     for pred2 = (lambda (line) (string-match keyregex (car line)))
                     for newlines = (remove-if-not pred2 lines5)
                     for items = (mapcar converter newlines)
                     if newlines collect items into menus
                     and collect line into submenulines3
                     and do (setq lines5 (remove-if pred2 lines5))
                     end
                     finally return (cons menus submenulines3)))
         (lines6 (mapcar converter lines5))
         (menus2 (car pair))
         (submenulines4 (cdr pair))
         (mainvar (intern (concat "one-key-menu-" name "-alist")))
         ;; place prefix key items in appropriate submenus
         (vars (loop for lines on submenulines4
                     for line = (car lines)
                     for key = (car line)
                     for lastkey = (car (last (string-split key " ")))
                     for desc = (capitalize (replace-regexp-in-string "-" " " (cadr line)))
                     for desc2 = (propertize desc 'face (list :background "cyan"
                                                              :foreground one-key-item-foreground-colour)) 
                     for menuname = (concat name "-" (replace-regexp-in-string " " "_" key))
                     for var = (intern (concat "one-key-menu-" menuname "-alist"))
                     for cmd = `(lambda nil (interactive) (one-key-open-submenu ,menuname ,var))
                     for item = (cons (cons lastkey desc2) cmd)
                     for others = (cdr lines)
                     for i = (if i (1+ i) 1)
                     for pos = (position-if (lambda (x)
                                              (let* ((key1 (car x))
                                                     (key2 (replace-regexp-in-string "ESC$" "M-" key1))
                                                     (keyregex (concat "^\\("
                                                                       (regexp-opt (list (concat key1 " ") key2))
                                                                       "\\)")))
                                                (string-match keyregex key))) others)
                     for pos2 = (if pos (+ i pos))
                     for matchmenu = (if pos (nth pos2 menus2))
                     if pos do (setf (nth pos2 menus2) (add-to-list 'matchmenu item))
                     else do (setq lines6 (add-to-list 'lines6 item))
                     collect var)))
    (add-to-list 'one-key-altered-menus mainvar)
    (loop for var in vars
          for menu in menus2
          do (set var menu)
          (add-to-list 'one-key-altered-menus var)
          collect var)
    (set mainvar lines6)
    mainvar))

(defun one-key-generate-key (desc usedkeys &optional elements)
  "Return a key for the menu item whose description string is DESC.
The generated key can be used in a `one-key' menu. 
If provided, ELEMENTS should be a list of keys to choose from, otherwise `one-key-default-menu-keys' will be used.
USEDKEYS should be a list of keys which cannot be used (since they have already be used).
This function can be used to help automatic creation of `one-key' menus."
  (let ((elements (or elements one-key-default-menu-keys)))
    (or (dolist (element elements)
          (when (not (memq element usedkeys))
            (return element)))
        (error "Can not generate a unique key for menu item : %s" desc))))

(defun* one-key-create-menu-lists (commands &optional descriptions keys
                                            (maxsize (length one-key-default-menu-keys))
                                            (keyfunc 'one-key-generate-key))
  "Create list/lists of menu items for use in `one-key' menu.
COMMANDS should be a list of commands for the menu items, and KEYS an optional corresponding list of keys.
If any element in KEYS is nil, or if KEYS is nil, then KEYFUNC will be used to generate a key for the item.
DESCRIPTIONS is an optional argument which should contain a list of descriptions for the menu items.
If any of the items in DESCRIPTIONS is nil or if DESCRIPTIONS is not supplied then the item will have its description
set from the corresponding command name. 
If the number of menu items is larger than MAXSIZE then several menus will be created, each of
which contains at most MAXSIZE items. By default MAXSIZE is equal to the length of `one-key-default-menu-keys',
and KEYFUNC is set to `one-key-generate-key' (which selects keys from `one-key-default-menu-keys')."
  (let* ((nitems (length commands))
         (nitemslast (% nitems maxsize))
         (nummenus (+ (/ nitems maxsize) (min nitemslast 1)))
         (indices (loop with start = 0
                        with end = 0
                        while (< end nitems)
                        do (setq start end end (min (+ end maxsize) nitems))
                        collect (cons start end)))
         (menu-alists (loop for (start . end) in indices
                            for cmds = (subseq commands start end)
                            for descs = (subseq descriptions start end)
                            for keys2 = (subseq keys start end)
                            for descs2 = (loop for desc in descs
                                               for cmd in cmds
                                               for key in keys2
                                               for desc2 = (or desc
                                                               (capitalize
                                                                (replace-regexp-in-string
                                                                 "-" " " (symbol-name cmd))))
                                               collect (if key
                                                           (concat desc2 " ("
                                                                   (single-key-description key)
                                                                   ")")
                                                         desc2))
                            for usedkeys = (loop for key in keys2 if key collect key)
                            for keys3 = (loop for key in keys2
                                              for desc in descs2
                                              collect (or key
                                                          (let ((newkey (one-key-generate-key desc usedkeys)))
                                                            (push newkey usedkeys)
                                                            newkey)))
                            for keystrs = (mapcar 'single-key-description keys3)
                            collect (loop for cmd in cmds
                                          for desc in descs2
                                          for key in keystrs
                                          collect (cons (cons key desc) cmd)))))
    menu-alists))
                                                          
(defun one-key-build-menu-sets-menu-alist nil
  "Build menu-alist for opening menu sets defined in `one-key-sets-of-menus-alist'."
  (let* ((descriptions (mapcar (lambda (item)
                                 (let ((str (car item)))
                                   (if (equal str one-key-default-menu-set)
                                       (propertize str 'face (list :background "red"
                                                                   :foreground one-key-item-foreground-colour))
                                     str))) one-key-sets-of-menus-alist))
         (commands (mapcar (lambda (item)
                 `(lambda nil (interactive)
                    (one-key-open-menu-set ,(car item))))
                           one-key-sets-of-menus-alist)))
    (car (one-key-create-menu-lists commands descriptions))))

(defun one-key-save-altered-menus nil
  "Save the menus listed in `one-key-altered-menus' into the file `one-key-menus-save-file'.
Any menu names that match the regular expressions in `one-key-exclude-from-save' will not be saved."
  (loop for var in one-key-altered-menus
        for varname = (symbol-name var)
        for name = (substring varname 13 -6)
        for menulist = (eval var)
        for exclude = (loop for regex in one-key-exclude-from-save
                            if (string-match regex varname) return t)
        if (not exclude) do (one-key-save-menu name var menulist)))

(defun one-key-get-major-mode-menu (name)
  "Return a menu name and menu alist for the current major mode.
This function is used by `one-key-types-of-menu' and the NAME argument is redundant.
A cons cell in the form (menu-name . menu-alist) will be returned.
If there is an element of `one-key-major-mode-remap-alist' associated with the current major mode then that will be used,
otherwise the name of the current major mode will be used.
In both cases if the variable `one-key-menu-<menu-name>-alist' (where <menu-name> is the menu name associated with this
major mode) exists then it will be used, otherwise it will be created."
  (let* ((menuname (or (cdr (assoc major-mode one-key-major-mode-remap-alist))
                       (with-selected-window
                           (previous-window)
                         (symbol-name major-mode))))
         (symname (concat "one-key-menu-" menuname "-alist"))
         (menusym (intern-soft symname)))
    (if (or (not menusym) (not (boundp menusym)))
        (let* ((mapname (concat menuname "-map"))
               (mapsym (intern-soft mapname)))
          (if (and mapsym (boundp mapsym))
              (progn (one-key-create-menus-from-keymap mapsym menuname)
                     (setq menusym (intern-soft symname)))
            (message "Can't create menu for %S" major-mode)
            (setq menusym nil))))
    (cons menuname menusym)))

;; Set the menu-alist, title string format and special keybindings for the top-level `one-key' menu
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "top-level"
                            (cons "top-level" 'one-key-toplevel-alist)
                            nil
                            nil) t)
;; Set the menu-alist, title string format and special keybindings for blank `one-key' menus
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "blank menu"
                            (lambda (name)
                              (let* ((name (read-string "Menu name: "))
                                     (symname (concat "one-key-menu-" name "-alist"))
                                     (menusym (intern-soft symname)))
                                (while menusym
                                  (if (not (y-or-n-p "Menu with that name already exists, overwrite?"))
                                      (progn (setq name (read-string "Menu name: "))
                                             (setq symname (concat "one-key-menu-" name "-alist"))
                                             (setq menusym (intern-soft symname)))
                                    (setq menusym nil)))
                                (setq menusym (intern symname))
                                (set menusym nil)
                                (add-to-list 'one-key-altered-menus menusym)
                                (cons name menusym)))
                            nil
                            nil) t)
;; Set the menu-alist, title string format and special keybindings for `one-key' menus for this major mode
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "major-mode"
                            'one-key-get-major-mode-menu
                            nil
                            nil) t)
;; Set the menu-alist, title string format and special keybindings for adding existing menus
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "existing menu"
                            (lambda (name)
                              (let* ((names (loop for sym being the symbols
                                                  for name = (symbol-name sym)
                                                  when (string-match "one-key-menu-\\(.+\\)-alist" name)
                                                  collect (match-string 1 name)))
                                     (name (if (featurep 'ido)
                                               (ido-completing-read "Menu: " names)
                                             (completing-read "Menu: " names))))
                                (cons name (intern-soft (concat "one-key-menu-" name "-alist")))))
                            nil
                            nil) t)
;; Set the menu-alist, title string format and special keybindings for adding menus for existing keymaps
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "existing keymap"
                            (lambda (name)
                              (let* ((names (loop for sym being the symbols
                                                  for name = (symbol-name sym)
                                                  when (string-match "\\(.*\\)-map$" name)
                                                  collect (match-string 1 name)))
                                     (name (if (featurep 'ido)
                                               (ido-completing-read "Keymap: " names)
                                             (completing-read "Keymap: " names)))
                                     (kmap (intern-soft (concat name "-map"))))
                                (one-key-create-menus-from-keymap kmap)
                                (cons name (intern-soft (concat "one-key-menu-" name "-alist")))))
                            nil
                            nil) t)
;; Set the menu-alist, title string format and special keybindings for the menu-sets menu
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "menu-sets"
                            (lambda (name)
                              (cons name (one-key-build-menu-sets-menu-alist)))
                            (lambda nil
                              (let* ((col1 (if one-key-auto-brighten-used-keys "#7FFF00000000" "red"))
                                     (col2 (cdr (assq 'background-color (frame-parameters))))
                                     (hsv2 (hexrgb-hex-to-hsv col2))
                                     (col2a (hexrgb-hsv-to-hex (first hsv2) (second hsv2) 0.5))
                                     (col2b (if one-key-auto-brighten-used-keys col2a col2))
                                     (keystr1 (propertize "default menu set"
                                                          'face (list :background col1
                                                                      :foreground one-key-item-foreground-colour)))
                                     (keystr2 (propertize "normal menu set"
                                                          'face (list :background col2b
                                                                      :foreground one-key-item-foreground-colour))))
                                (concat keystr1 keystr2 "\n" 
                                        (format "Sorted by %s (%s first). Press <f1> for help.\n"
                                                one-key-current-sort-method
                                                (if one-key-column-major-order "columns" "rows")))))
                            'one-key-menu-sets-special-keybindings) t)

;; add function for autosaving menus to kill-emacs-hook
(add-hook 'kill-emacs-hook (lambda nil (if one-key-autosave-menus (one-key-save-altered-menus))))

;; Load the saved one-key menus.
(if one-key-menus-save-file
    (if (file-exists-p one-key-menus-save-file)
        (if (file-readable-p one-key-menus-save-file)
            (load-file one-key-menus-save-file)
          (message "Can't read file %s" one-key-menus-save-file))
      (message "Can't find file %s" one-key-menus-save-file))
  (message "`one-key-menus-save-file' is not set, no menus loaded"))

(provide 'one-key)

;;; one-key.el ends here


