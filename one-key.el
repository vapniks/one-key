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
;; Load one-key-menu-alists on demand to save memory.
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
  :type 'integer
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

(defcustom one-key-menus-location "~/.emacs.d/"
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

(defcustom one-key-item-foreground-colour "black"
  "Foreground colour of highlighted items in `one-key' menus."
  :type 'color
  :group 'one-key)

(defcustom one-key-menu-auto-brighten-used-keys t
  "If non-nil then set brightness of menu items colours according to how often the keys are pressed."
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

(defcustom one-key-special-keybindings
  '(("ESC" "Quit and close menu window" (lambda nil (keyboard-quit) nil))
    ("<C-escape>" "Quit and keep menu window open"
     (lambda nil (setq keep-window-p t) nil))
    ("<C-menu>" "Keep menu window open and dont quit"
     (lambda nil (if match-recursion-p
                     (setq match-recursion-p nil
                           miss-match-recursion-p nil)
                   (setq match-recursion-p t
                         miss-match-recursion-p t))))
    ("<menu>" "Toggle menu window display" (lambda nil (one-key-menu-window-toggle title filtered-list) t))
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
    ("<up>" "Scroll menu or move item up by one line" (lambda nil (one-key-scroll-or-move-up full-list) t))
    ("<down>" "Scroll menu or move item down by one line" (lambda nil (one-key-scroll-or-move-down full-list) t))
    ("<prior>" "Scroll menu down one page" (lambda nil (one-key-menu-window-scroll-down) t))
    ("<next>" "Scroll menu up one page" (lambda nil (one-key-menu-window-scroll-up) t))
    ("C-h" "Show help for next item chosen" (lambda nil (setq one-key-menu-show-key-help t) t))
    ("C-f" "Edit the file associated with this menu" (lambda nil (one-key-open-associated-file info-alist) nil))
    ("C-s" "Save current state of this menu" (lambda nil (one-key-save-menu this-title info-alist full-list) t))
    ("<f1>" "Toggle this help buffer" (lambda nil (if (get-buffer-window "*Help*")
                                                      (kill-buffer "*Help*")
                                                    (one-key-show-help)) t))
    ("<f2>" "Toggle column/row ordering of menu items"
     (lambda nil (if one-key-column-major-order
                        (setq one-key-column-major-order nil)
                      (setq one-key-column-major-order t))
       (setq one-key-menu-call-first-time t) t))
    ("<f3>" "Sort menu items by next method"
     (lambda nil (one-key-sort-items-by-next-method info-alist full-list) t))
    ("<C-f3>" "Sort menu items by previous method"
     (lambda nil (one-key-sort-items-by-next-method info-alist full-list t) t))
    ("<f4>" "Reverse the order of the menu items"
     (lambda nil (one-key-reverse-item-order info-alist full-list) t))
    ("<f5>" "Limit menu items to those matching regexp"
     (lambda nil (setq filter-regex (read-regexp "Regular expression"))
       (setq one-key-menu-call-first-time t) t))
    ("<f6>" "Highlight menu items matching regexp"
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
     (lambda nil (one-key-swap-menu-items full-list) t))
    ("<f10>" "Add a menu item"
     (lambda nil (one-key-add-menu-item info-alist full-list) t))
    ("<f11>" "Reposition a menu item"
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Global Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-window-configuration nil
  "The window configuration that records the current window configuration before the popup menu window.")

(defvar one-key-menu-call-first-time t
  "t if `one-key-menu' has been called non-recursively.")

(defvar one-key-menu-show-key-help nil
  "If true show help for function associated with next keystroke, when it is pressed in the one-key-menu.")

(defvar one-key-menu-title-format-string
  '(if titles
       (format "Sorted by %s (%s first). Press <f1> for help.\n\n" one-key-current-sort-method (if one-key-column-major-order "columns" "rows"))
     (format "<%s> keystrokes, sorted by %s (%s first). Press <f1> for help.\n\n"
             title one-key-current-sort-method (if one-key-column-major-order "columns" "rows")))
  "An sexp that should return a string to display at the top of the menu window.
The sexp will be evaluated in the context of the `one-key-highlight-menu' function, and will be processed by
`one-key-highlight' before display.
You may wish to alter this temporarily for some `one-key' menus.")

(defvar one-key-current-sort-method nil
  "The current method used to sort the items in the list")

(defvar one-key-current-item-being-moved nil
  "The key corresponding to the item currently being moved in the `one-key' menu, or nil if none is being moved.")
  
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
               (ido-file-internal 'write 'write-file
                                  (file-name-as-directory one-key-menus-location)
                                  "Save as: " nil name 'ignore)
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
  (one-key-menu "one-key" 'one-key-menu-one-key-alist))

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
  (one-key-menu "one-key-template" 'one-key-menu-one-key-template-alist))

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
  (one-key-menu "one-key-template-sort" 'one-key-menu-one-key-template-sort-alist))

(defun one-key-menu-toplevel ()
  "The `one-key' toplevel menu."
  (interactive)
  (one-key-menu "toplevel" 'one-key-toplevel-alist))

(defun one-key-get-menu (mode)
  "Show appropriate one-key menu for current major mode."
  (interactive `(,major-mode))
  (let ((menu (assoc mode one-key-mode-alist)))
    (if menu
	(funcall (cdr menu))
      (one-key-menu-toplevel))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun one-key-show-help nil
  "Show information about to special keybindings in the `one-key' menu."
  (interactive)
  (let* ((maxkey (loop for elt in one-key-special-keybindings
                       maximize (1+ (length (car elt)))))
         (maxstr (loop for elt in one-key-special-keybindings
                       maximize (+ 3 maxkey (length (cadr elt)))))
         (width (/ (window-width) 2))
         (keystr (if (> maxstr width)
                     (mapconcat (lambda (elt) (format "%s\t: %s" (car elt) (cadr elt)))
                                one-key-special-keybindings "\n")
                   (loop with colsize = (+ (/ (length one-key-special-keybindings) 2)
                                           (% (length one-key-special-keybindings) 2))
                         with finalstr
                         for n from 0 to (1- colsize)
                         for (key1 desc1) = (nth n one-key-special-keybindings)
                         for (key2 desc2) = (nth (+ n colsize) one-key-special-keybindings)
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

(defun one-key-scroll-or-move-up (full-list)
  "Either scroll the `one-key' menu window down by one line or move an item down.
If `one-key-current-item-being-moved' contains a string representation of one of the keys in the menu move that item
down one line, otherwise scroll the window down one line."
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
          (one-key-menu-window-close))
      (one-key-menu-window-scroll-down-line))
    (setq protect-function (lambda nil (interactive) (setq one-key-current-item-being-moved nil)))))

(defun one-key-scroll-or-move-down (full-list)
  "Either scroll the `one-key' menu window down by one line or move an item down.
If `one-key-current-item-being-moved' contains a string representation of one of the keys in the menu move that item
down one line, otherwise scroll the window down one line."
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
          (one-key-menu-window-close))
      (one-key-menu-window-scroll-up-line))
    (setq protect-function (lambda nil (interactive) (setq one-key-current-item-being-moved nil)))))

(defun one-key-add-menu-item (info-alist full-list)
  "Prompt the user for item details and add it to FULL-LIST, then update INFO-ALIST and redisplay the `one-key' menu."
  (let* ((isref (symbolp info-alist))
         (key (read-event "Enter the key for the new item"))
         (keystr (single-key-description key))
         (usingregs (eq info-alist 'one-key-regs-menu-alist)))
    (if usingregs (progn (one-key-regs-function key '(4))
                         (one-key-regs-update-menu-alist))
      (let ((desc (read-string "Item description: "))
            (contents (read-from-minibuffer "Command: " nil nil t))
            (newitem (cons (cons keystr desc) contents)))
        (if isref (set info-alist (add-to-list 'full-list newitem))
          (setq info-alist (add-to-list 'full-list newitem)))))
    (setq one-key-menu-call-first-time t)
    (one-key-menu-window-close)))

(defun one-key-swap-menu-items (full-list)
  "Prompt user for a pair of items from FULL-LIST and swap the corresponding keys."
  (let* ((keya (read-event "Press key for first item"))
         (keyastr (single-key-description keya))
         (itema (assoc-if (lambda (itemcar) (equal (car itemcar) keyastr)) full-list))
         (keyb (read-key "Press key for second item"))
         (keybstr (single-key-description keyb))
         (itemb (assoc-if (lambda (itemcar) (equal (car itemcar) keybstr)) full-list))
         (usingregs (eq info-alist 'one-key-regs-menu-alist)))
    (if (not (and itema itemb)) (message "Invalid key!")
      (setf (caar itema) keybstr (caar itemb) keyastr)
      (if usingregs (let ((rega (assq keya register-alist))
                          (regb (assq keyb register-alist)))
                      (setf (car rega) keyb (car regb) keya))))
    (setq one-key-menu-call-first-time t)
    (one-key-menu-window-close)))

(defun one-key-delete-menu-item (info-alist full-list)
  "Prompt the user for an item to delete from FULL-LIST, delete it, and then redisplay the `one-key' menu."
  (let* ((isref (symbolp info-alist))
         (key (read-event "Press the key of the item you want to delete"))
         (keystr (single-key-description key))
         (item (assoc-if (lambda (itemcar) (equal (car itemcar) keystr)) full-list))
         (usingregs (eq info-alist 'one-key-regs-menu-alist)))
    (if (and item (y-or-n-p (format "Delete item \"%s\"?" (cdar item))))
        (if isref (set info-alist (delete item full-list))
          (setq info-alist (delete item full-list))))
    (if usingregs (setq register-alist (assq-delete-all key register-alist)))
    (setq one-key-menu-call-first-time t)
    (one-key-menu-window-close)))

(defun one-key-edit-menu-item (info-alist full-list)
  "Prompt user for details of item in FULL-LIST to edit, make changes and then reopen `one-key' menu."
  (let* ((oldkey (read-event "Press the key of the item you want to edit"))
         (oldkeystr (single-key-description oldkey))
         (item (assoc-if (lambda (itemcar) (equal (car itemcar) oldkeystr)) full-list))
         (newkey (let ((key (read-key "Enter new key for the item")))
                   (while (and (assoc-if (lambda (x) (equal (car x) (single-key-description key)))
                                         full-list)
                               (not (eq key oldkey))
                               (not (y-or-n-p "That key is already used! Use it anyway?")))
                     (setq key (read-key "Enter new key for the item")))
                   key))
         (newkeystr (single-key-description newkey))
         (desc (read-string "Item description: " (cdar item) nil nil))
         (usingregs (eq info-alist 'one-key-regs-menu-alist))
         (oldcontents (if usingregs (cdr (assq oldkey register-alist)) (cdr item)))
         (contents (read-from-minibuffer (if usingregs "Register contents: "
                                           "Item contents: ") (format "%S" oldcontents) nil t)))
    (if item (setf (caar item) newkeystr (cdar item) desc (cdr item) (if usingregs (cdr item) contents)))
    (if usingregs (progn (setq register-alist (assq-delete-all oldkey register-alist))
                         (setq register-alist (assq-delete-all newkey register-alist))
                         (add-to-list 'register-alist (cons newkey contents))))
    (setq one-key-menu-call-first-time t)
    (one-key-menu-window-close)))

(defun one-key-highlight-matching-items (info-alist full-list colour pred)
  "Highlight items in FULL-LIST with colour COLOUR using predicate function PRED to select items.
The predicate function should take a single item from FULL-LIST as it's only argument.
FULL-LIST should be the `one-key' menu alist pointed to by info-alist in the SELF call.
If COLOUR is \"\" then all highlighting (and more generally any text properties) are removed from the item."
  (loop for item in-ref full-list
        for str = (cdar item)
        if (funcall pred item) do
        (if (equal colour "")
            (setf (cdar item) (substring-no-properties str))
          (setf (cdar item)
                (propertize str 'face (list :background colour :foreground one-key-item-foreground-colour)))))
    (setq one-key-menu-call-first-time t)
    (one-key-menu-window-close)
    (if (eq info-alist 'one-key-regs-menu-alist)
        (one-key-regs-update-menu-alist)))

(defun one-key-save-menu (title info-alist full-list)
  "Save the current `one-key' menu to a file (prompting the user if an associated file can't be found)."
  (let* ((varname (if (symbolp info-alist) (symbol-name info-alist)
                    (concat "one-key-menu-" title "-alist")))
         (dir (file-name-as-directory one-key-menus-location))
         (file (or (find-lisp-object-file-name info-alist 'defvar)
                   (if (y-or-n-p "Can't find associated source file, create a new one?")
                       (expand-file-name (read-file-name
                                          "File name: " dir (concat "one-key-menu_" title ".el"))
                                         dir))))
         (usingregs (eq info-alist 'one-key-regs-menu-alist)))
    (if usingregs (one-key-regs-save-registers)
      (if file (with-current-buffer (find-file-noselect file)
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
                 (save-buffer))
        (message "Save aborted")))))

(defun one-key-open-associated-file (info-alist)
  "Open the file associated with INFO-ALIST, which should be a symbol whose value is a list of `one-key' menu items."
  (let* ((usingregs (eq info-alist 'one-key-regs-menu-alist))
         (file (if usingregs (one-key-regs-open-registers-file)
                 (find-lisp-object-file-name info-alist 'defvar))))
    (if file
        (progn (find-file-other-window file)
               (one-key-template-mode)
               (goto-char (point-min))
               (search-forward (symbol-name info-alist) nil t)
               (setq one-key-menu-window-configuration nil)
               (setq match-recursion-p nil)
               (setq miss-match-recursion-p nil))
      (message "Can't find associated source file!"))))

(defun one-key-get-next-sort-predicate (&optional prev)
  "Return the sort predicate in `one-key-sort-method-alist' that comes after `one-key-current-sort-method'.
Also set `one-key-current-sort-method' to the car of the new item. If PREV is non-nil get previous sort predicate instead."
  (let* ((pos (position one-key-current-sort-method one-key-sort-method-alist
                        :test (lambda (a b) (eq a (car b)))))
         (len (length one-key-sort-method-alist))
         (newpos (if prev (if (and pos (> pos 0))
                              (1- pos) (1- len))
                   (if (and pos (< pos (1- len)))
                       (1+ pos) 0)))
         (item (nth newpos one-key-sort-method-alist)))
    (setq one-key-current-sort-method (car item))
    (cdr item)))

(defun one-key-sort-items-by-next-method (info-alist full-list &optional prev)
  "Sort the items in FULL-LIST according to the method in `one-key-sort-method-alist' after `one-key-current-sort-method'.
If PREV is non-nil use method before `one-key-current-sort-method'.
Then update INFO-ALIST or the list that it points to (if its value is a symbol), and call SELF (a call to `one-key-menu').
The variables INFO-ALIST and FULL-LIST are arguments in the call to SELF."
  (let* ((isref (symbolp info-alist))
         (sorted-list (sort (copy-list full-list) (one-key-get-next-sort-predicate prev)))
         (usingregs (eq info-alist 'one-key-regs-menu-alist))
         (major (if one-key-column-major-order "columns" "rows")))
    (if isref (set info-alist sorted-list)
      (setq info-alist sorted-list))
    (setq one-key-menu-call-first-time t)
    (one-key-menu-window-close)
    (if usingregs
        (one-key-regs-update-menu-alist))))

(defun one-key-reverse-item-order (info-alist full-list)
  "Reverse the order of items in FULL-LIST.
Then update INFO-ALIST or the list that it points to (if its value is a symbol), and call SELF (a call to `one-key-menu').
The variables INFO-ALIST and FULL-LIST are arguments in the call to SELF."
  (let ((isref (symbolp info-alist))
        (reversed-list (reverse full-list))
        (usingregs (eq info-alist 'one-key-regs-menu-alist)))
    (if isref (set info-alist reversed-list)
      (setq info-alist reversed-list))
    (setq one-key-menu-call-first-time t)
    (one-key-menu-window-close)
    (if usingregs (one-key-regs-update-menu-alist))))

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

(defun one-key-highlight-menu (title keystroke &optional titles)
  "Highlight menu TITLE and KEYSTROKE information.
Title is the name of the `one-key' menu, and KEYSTROKE is the alist of menu items."
  (let* ((titlefunc (lambda (x) (if (equal title x) (propertize x 'face 'one-key-title) x)))
         (titles2 (mapcar titlefunc titles))
         (titlesline (if titles
                         (concat "| " (mapconcat 'identity titles2 " | ") " |\n")))
         (infoline (one-key-highlight (eval one-key-menu-title-format-string)
                                      "\\(<[^<>]*>\\|'[^']*'\\)" '(face one-key-title)))
         (keystrokelist (one-key-highlight keystroke "\\[\\([^\\[\\]\\)*\\]" '(face one-key-keystroke))))
    (concat titlesline infoline keystrokelist)))

(defun* one-key-menu (titles
                      info-alists
                      &key
                      
                      (menu-number (if  ; hack to check if info-alists is a list of lists or just a single list
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
If `one-key-popup-window' is t (default) then a menu window will be displayed showing the keybindings.
TITLES is the title of the menu as displayed in the menu window, or a list of titles corresponding to different menu
lists in INFO-ALIST.
INFO-ALIST is either a list of menu items, a list of such lists or a symbol whose value is a list or list of lists.
Each item in a menu list is of the form: ((key . description) . command).
If INFO-ALIST is a list then MENU-NUMBER should be an index (starting at 0) indicating which list to display
initially, otherwise the first list will be used.
The user can switch between the menu lists by pressing the appropriate keys in `one-key-special-keybindings'.
If KEEP-WINDOW-P is non-nil then the menu window will be kept open even after exiting.
If EXECUTE-WHEN-MISS-MATCH-P is nil then keys not matching menu items or `one-key-special-keybindings' will be ignored,
otherwise the associated commands in the current keymaps will be executed.
The arguments MATCH-RECURSION-P and MISS-MATCH-RECURSION-P indicate whether to execute `one-key-menu' recursively after
a matching or non-matching key are pressed (and corresponding commands executed) respectively.
PROTECT-FUNCTION, if non-nil, is a function that is called within an `unwind-protect' statement at the end
of `one-key-menu'.
ALTERNATE-FUNCTION if non-nil is a function that is called after each key press while the menu is active."
  (let* ((issymbol (symbolp info-alists))
         (info-alist (if menu-number
                         (nth menu-number info-alists)
                       info-alists))
         (full-list (if (symbolp info-alist) 
                        (eval info-alist)
                      info-alist))
         (this-title (if (stringp titles) titles
                       (nth menu-number titles)))
         ;; the list of items after filtering
         (filtered-list (if (stringp filter-regex)
                            (remove-if-not
                             (lambda (elt) (string-match filter-regex (cdar elt)))
                             full-list)
                          full-list))
         ;; following function is used for recursively calling itself when needed
         (self (function (lambda () (one-key-menu titles info-alists
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
        (let* ((titlelist (if (listp titles) titles nil))
               (event (read-key (if one-key-menu-call-first-time
                                    ;; just show the menu buffer when first called
                                    (progn (setq one-key-menu-call-first-time nil)
                                           (if one-key-popup-window
                                               (one-key-menu-window-open this-title filtered-list titlelist))))))
               (key (single-key-description event)))
          (cond (
                 ;; HANDLE KEYSTROKES MATCHING MENU ITEMS:
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
                           (if one-key-menu-show-key-help
                               ;; show help for key if previous keypress was for key help
                               (progn (if (symbolp command)
                                          (describe-function command)
                                        (with-help-window (help-buffer)
                                          (princ command)))
                                      (setq one-key-menu-show-key-help nil) ; reset `one-key-menu-show-key-help'
                                      (setq match-recursion-p t)) ; set recursion so that `one-key-menu' is called again
                             ;; Update key usage statistics if necessary
                             (if one-key-menu-auto-brighten-used-keys
                                 (one-key-menu-increment-key-usage item))
                             ;; We need to close the `one-key' menu window before running the items command.
                             ;; Save previous state of the `one-key' window before doing this.
                             (let* ((old-one-key-popup-window one-key-popup-window)
                                    (one-key-popup-window (one-key-menu-window-exist-p)))
                               (one-key-menu-window-close)
                               (setq one-key-menu-call-first-time t) ; allow recursive execution of `one-key-menu'
                               (call-interactively command) ; call the items command
                               ;; reopen the `one-key' window if necessary
                               (if (and one-key-popup-window (or keep-window-p match-recursion-p))
                                   (one-key-menu-window-open this-title filtered-list titlelist))
                               (setq one-key-popup-window old-one-key-popup-window)))
                           (setq one-key-menu-call-first-time nil)
                           ;; throw t if the key matched so that this clause's body is executed, otherwise return nil
                           (throw 'match t))) nil)
                 ;; call the `alternate-function' and if `match-recursion-p' is non-nil wait for next keypress
                 (let ((temp (one-key-handle-last alternate-function self match-recursion-p)))
                   ;; if necessary, propagate the value of `keep-window-p' back up
                   (if match-recursion-p
                       (setq keep-window-p temp))))
                ;; HANDLE SPECIAL KEYS:
                ((assoc key one-key-special-keybindings)
                 ;; call the `alternate-function' and the function associated with the special key
                 ;; if this function returns non-nil then wait for the next keypress
                 (let* ((again (funcall (caddr (assoc key one-key-special-keybindings))))
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
                         (one-key-menu-window-open this-title filtered-list titlelist))
                     (setq one-key-popup-window old-one-key-popup-window)))
                 ;; call the `alternate-function' and if `miss-match-recursion-p' is non-nil wait for next keypress
                 (let ((temp (one-key-handle-last alternate-function self miss-match-recursion-p)))
                   ;; if necessary, propagate the value of `keep-window-p' back up
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
If KEYSTROKE is a name of keymap, use the keymap, otherwise it's interpreted as a key stroke."
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

(defun one-key-menu-window-toggle (title info-alist &optional titles)
  "Toggle the `one-key' menu window.
Argument TITLE is the alist of keys and associated decriptions and functions.
Each item of this list is in the form: ((key . describe) . command)."
  (if (one-key-menu-window-exist-p)
      (one-key-menu-window-close)
    (one-key-menu-window-open title info-alist titles)))

(defun one-key-menu-window-open (title info-alist &optional titles)
  "Open the `one-key' menu window.
Argument TITLE is a title for the `one-key' menu.
Argument INFO-ALIST is the alist of keys and associated decriptions and functions, or a symbol referencing the list.
Each item of this list is in the form: ((key . describe) . command).
If KEEPOLD is non-nil then if there is already a `one-key' menu buffer, that will be reopened,
otherwise a new one will be created."
  ;; Save current window configuration.
  (if (one-key-menu-window-exist-p)
      (one-key-menu-window-close))
  (or one-key-menu-window-configuration
      (setq one-key-menu-window-configuration (current-window-configuration)))
  ;; Update key brightnesses if necessary
  (if one-key-menu-auto-brighten-used-keys
      (one-key-menu-brighten-most-used info-alist))
  ;; Generate buffer information.
  (with-current-buffer (get-buffer-create one-key-buffer-name)
    (erase-buffer)
    (goto-char (point-min))
    (save-excursion
      (insert (one-key-highlight-menu
               title (one-key-menu-format info-alist) titles))))
  ;; Pop `one-key' buffer.
  (pop-to-buffer one-key-buffer-name)
  (set-buffer one-key-buffer-name)
  ;; Adjust height of menu window to display buffer's contents exactly.
  (fit-window-to-buffer nil one-key-menu-window-max-height)
  nil)

(defun one-key-menu-window-close ()
  "Close the menu window."
  ;; Kill menu buffer.
  (when (bufferp (get-buffer one-key-buffer-name))
    (delete-window (get-buffer-window one-key-buffer-name))
    (kill-buffer one-key-buffer-name))
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
  "Return t if x is a list of length > 1"
  (and (not (commandp x))
       (listp x)
       (listp (cdr x))
       (> (length x) 1)))

(defun one-key-menu-brighten-most-used (info-alist)
  "Set values of menu item colours proportionally according to how often they have been used.
Argument INFO-ALIST is the alist of keys and associated decriptions and functions, or a symbol referencing the list."
  ;; first get min and max keypress values
  (let* ((menu-alist (if (symbolp info-alist) (eval info-alist) info-alist))
         (minmaxvals (loop for ((key . desc) . rest) in menu-alist
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
          for oldcol = (or (plist-get (get-text-property 0 'face desc) :background)
                           (cdr (assq 'background-color (frame-parameters))))
          for (h s v) = (hexrgb-hex-to-hsv oldcol)
          ;; set new HSV values to update colour value
          for newcol = (hexrgb-hsv-to-hex h s vval) do
          (setf (cdar item)
                (propertize desc 'face (list :background newcol
                                             :foreground one-key-item-foreground-colour))))))

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
      (insert (format "(defun one-key-menu-%s ()\n\"The `one-key' menu for %s\"\n(interactive)\n(one-key-menu \"%s\" 'one-key-menu-%s-alist))\n" funcname title title funcname))
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
    (one-key-load-files (concat (file-name-as-directory one-key-menus-location) one-key-menus-regexp)))

(provide 'one-key)

;;; one-key.el ends here

;;; LocalWords:  emms specpdl minish DarkRed msg FUNCITN num str decf elt args
;;; LocalWords:  rubikitch's desc SPC bmenu sKeymap nsTitle fontify funcname
;;; LocalWords:  bol eol destructuring cmd PageUp PageDown
