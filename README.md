# one-key.el --- One key

* Filename: one-key.el
* Description: One key
* Author: Andy Stewart <lazycat.manatee@gmail.com>
        rubikitch <rubikitch@ruby-lang.org>
* Maintainer: Joe Bloggs <vapniks@yahoo.com>
Copyleft (Ↄ) 2010, Joe Bloggs, all rites reversed.
Copyright (C) 2008, 2009, 2010 Andy Stewart, all rights reserved.
Copyright (C) 2009, rubikitch, all rights reserved.
* Created: 2008-12-22 21:54:30
* Version: 0.7.1
* Last-Updated: 7/12/2010 20:22:00
          By: Joe Bloggs
* URL: http://www.emacswiki.org/emacs/download/one-key.el
* Keywords: one-key
* Compatibility: GNU Emacs 22 ~ 23
;; Features that might be required by this library:
;; `cl'

# This file is NOT part of GNU Emacs

# License
Licensed under the [GPL version 3](http://www.gnu.org/licenses/) or later.

# Commentary
;; With so many Emacs extensions, you have a lot of keystrokes to remember, and you probably forget most of them.
;; This package fixes this problem.
;; One Key provides a TOP keystroke that when pressed presents you with
a menu of choices in a popup window for commands to execute with a further keystroke.
;; Just type one of the listed keystrokes to execute the corresponding command.
;; You can also associate different menus with different major modes so that the menu presented depends on the
current major mode.
;; * Quick use example:
;; Add the variables and functions below to your ~/.emacs
;; (defvar one-key-menu-emms-alist nil
  "`One-Key' menu list for EMMS.")
;; (setq one-key-menu-emms-alist
      '(
        (("g" . "Playlist Go") . emms-playlist-mode-go)
        (("d" . "Play Directory Tree") . emms-play-directory-tree)
        (("f" . "Play File") . emms-play-file)
        (("i" . "Play Playlist") . emms-play-playlist)
        (("t" . "Add Directory Tree") . emms-add-directory-tree)
        (("c" . "Toggle Repeat Track") . emms-toggle-repeat-track)
        (("w" . "Toggle Repeat Playlist") . emms-toggle-repeat-playlist)
        (("u" . "Play Now") . emms-play-now)
        (("z" . "Show") . emms-show)
        (("s" . "Emms Streams") . emms-streams)
        (("b" . "Emms Browser") . emms-browser)))
;; (defun one-key-menu-emms ()
  "`One-Key' menu for EMMS."
  (interactive)
  (one-key-menu "emms" one-key-menu-emms-alist t))
;; Add an item to `one-key-toplevel-alist' in the customization buffer for one-key
(M-x customize-group RET one-key RET). The first item should be the key (e.g. m), the second item
should be a description (e.g. "Emacs multimedia system"), and the third item should be the command:
`one-key-menu-emms'. Then bind `one-key-menu-toplevel' to any key you want E.g:
;;  (global-set-key (kbd "C-M-s-SPC") 'one-key-menu-toplevel)
;; Alternatively you can ignore the toplevel menu and just bind `one-key-menu-emms' to a key,
E.g:
;;      (global-set-key (kbd "C-c p") 'one-key-menu-emms)
;; Now when you type the key, a one-key menu will popup at the bottom of the window.
Then you just type a keystroke listed in the menu to execute the corresponding command.
;; You can also associate menus with major-modes using the customizable `one-key-mode-alist' variable, 
and the `one-key-get-menu' command. When this command is run it will open the menu associated with the 
current major-mode, or the toplevel menu if there is no associated menu.
You can bind this to a global key, e.g:
;;     (global-set-key (kbd "C-s-SPC") 'one-key-get-menu)
;; Now you don't need to remember so many keystrokes, just remembering one keystroke is enough!
;; ** The format of the menu list:
;; (("KEYSTROKE" . "DESCRIBE") . COMMAND)
;; Example:
;; (defvar example-menu-alist
     '(
       (("Keystroke-A" . "Describe-A") . Command-A)
       (("Keystroke-B" . "Describe-B") . Command-B)
       (("Keystroke-C" . "Describe-C") . Command-C)
       ))
;; Make sure COMMAND is `interactive', otherwise it will throw an error.
;; ** The format of menu function:
;; (one-key-menu "MENU-NAME" MENU-ALIST)
;; Example:
;; (defun example-menu ()
  (interactive)
  (one-key-menu "example" example-menu-alist)
;; ** The arguments of the function `one-key-menu':
;; `title' is the title of menu, any string you like.
`info-alist' is a special list that contains KEY, DESCRIPTION
     and COMMAND.  see above description about `example-menu-alist'.
`miss-match-exit-p' set to t means the popup window will exit when you
     type a KEY that can't match in menu.
`recursion-p' is whether or not recursion will execute `one-key-menu' on self
     when no KEY matchs in the menu.
`protect-function' is a protect function that is called last in `one-key-menu',
     make sure this function is an `interactive' function.
`alternate-function' is an alternate function to execute last.
`execute-last-command-when-miss-match' whether to execute the last input command
when keystroke is not matched.
;; Creating menus for keymaps:
;; You can use `one-key-insert-template' to insert template code for a special keymap,
or `one-key-show-template' to create a special buffer called "One-Key-Template" containing the template code.
For example, after you run `one-key-insert-template', you will get a Keymap prompt:
"Keymap to One-Key: ", in which you enter the name of a keymap or a prefix key with an associated keymap.
After entering the keymap/prefix key you are prompted for a title for the menu, and then code for the menu
will be automatically generated.
E.g. if you type "C-x r", and then enter the title "bookmark" then it will generate template code
like the code shown below:
;; (defvar one-key-menu-bookmark-alist nil
  "The `one-key' menu list for BOOKMARK.")
;; (setq one-key-menu-bookmark-alist
   '(
     (("C-@" . "point-to-register") . point-to-register)
     (("SPC" . "point-to-register") . point-to-register)
     (("+" . "increment-register") . increment-register)
     (("b" . "bookmark-jump") . bookmark-jump)
     (("c" . "clear-rectangle") . clear-rectangle)
     (("d" . "delete-rectangle") . delete-rectangle)
     (("f" . "frame-configuration-to-register") . frame-configuration-to-register)
     (("g" . "insert-register") . insert-register)
     (("i" . "insert-register") . insert-register)
     (("j" . "jump-to-register") . jump-to-register)
     (("k" . "kill-rectangle") . kill-rectangle)
     (("l" . "bookmark-bmenu-list") . bookmark-bmenu-list)
     (("m" . "bookmark-set") . bookmark-set)
     (("n" . "number-to-register") . number-to-register)
     (("o" . "open-rectangle") . open-rectangle)
     (("r" . "copy-rectangle-to-register") . copy-rectangle-to-register)
     (("s" . "copy-to-register") . copy-to-register)
     (("t" . "string-rectangle") . string-rectangle)
     (("w" . "window-configuration-to-register") . window-configuration-to-register)
     (("x" . "copy-to-register") . copy-to-register)
     (("y" . "yank-rectangle") . yank-rectangle)
     (("C-SPC" . "point-to-register") . point-to-register)
     ))
;; (defun one-key-menu-bookmark ()
  (interactive)
  (one-key-menu "BOOKMARK" one-key-menu-bookmark-alist))
;; If you used `one-key-show-template' the code is placed in the special buffer "One-Key-Template"
which has it's own one-key menu and keybindings bound to special helper functions to help you edit the
menu. Type M-x one-key-get-menu to see a menu of commands/keybindings for this buffer
(or use one-key-menu-one-key-template if it is not listed in one-key-mode-alist).
For example you can move items in the menu up/down using "M-<up>" or "M-<down>".
You can sort the items in the currently active region alphabetically by description/key binding/command
by pressing "C-c C-s" followed by d/k/c.
You can quickly test your menu by pressing "C-c C-t".
;; Fixed menu keys:
;; Some keys are available for all menus and are always the same, they can be configured with the following variables:
 `one-key-key-hide'   :  key to hide the menu, default is "?"
 `one-key-key-quit'   :  key to quit the menu, default is "q"
 `one-key-key-up'     :  key to scroll the menu down one line, default is "<up>"
 `one-key-key-down'   :  key to scroll the menu up one line, default is "<down>"
 `one-key-key-pgup'   :  key to scroll the menu down one page, default is "<prior>"
 `one-key-key-pgdown' :  key to scroll the menu up one page, default is "<next>"
 `one-key-key-help'   :  when this key is pressed, the next keypress will show help for the corresponding command,
                         default is "C-?"
 `one-key-key-edit'   :  key to edit the menu (if the source file can be found), default is "C-/"
;; Auto-load one-key menus:
;; If you set `one-key-auto-load-menus' to t (in the customization group for one-key), then any files
in the directory specified by `one-key-menus-location' that match the regexp `one-key-menus-regexp'
will automatically be loaded on startup.

# Installation
;; Put one-key.el in a directory in your load-path, e.g. ~/.emacs.d/
You can add a directory to your load-path with the following line in ~/.emacs
(add-to-list 'load-path (expand-file-name "~/elisp"))
where ~/elisp is the directory you want to add 
(you don't need to do this for ~/.emacs.d - it's added by default).
;; Add the following to your ~/.emacs startup file.
;; (require 'one-key)
;; Because this library uses a special implementation,
sometimes a `max-lisp-eval-depth' or `max-specpdl-size' error can occur.
;; So making the above two variables larger will reduce the probability that an error occurs.
E.g:
;; (setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 10000)

# Customize
;; `one-key-buffer-name' : the buffer name of the popup menu.
`one-key-help-window-max-height' : the maximal height use in popup window.
`one-key-items-per-line' : number of items in one line, if this option is `nil', will be calculated by `window-width'.
`one-key-keystroke-face' : face for highlighting keystroke
`one-key-auto-load-menus' : if t then automatically load one-key menus from `one-key-menus-location'
`one-key-menus-location' : location in which one-key menus will be stored
`one-key-menus-regexp' : regexp to match filenames of one-key menus
`one-key-mode-alist' : An alist of major-mode, one-key menu pairs to set the default menu for each major-mode.
`one-key-toplevel-alist' : A list of key items for the toplevel menu.
`one-key-popup-window' : whether to popup window when first time run, default is `t'.
`one-key-prompt-face' : face for highlighting prompt
`one-key-template-buffer-name' : the buffer name of the template code.
`one-key-title-face' : face for highlighting title
`one-key-key-hide'   :  key to hide the menu, default is "?"
`one-key-key-quit'   :  key to quit the menu, default is "q"
`one-key-key-up'     :  key to scroll the menu down one line, default is "<up>"
`one-key-key-down'   :  key to scroll the menu up one line, default is "<down>"
`one-key-key-pgup'   :  key to scroll the menu down one page, default is "<prior>"
`one-key-key-pgdown' :  key to scroll the menu up one page, default is "<next>"
`one-key-key-help'   :  when this key is pressed, the next keypress will show help for the corresponding command,
                        default is "C-?"
`one-key-key-edit'   :  key to edit the menu (if the source file can be found), default is "C-/"

All above options can by customized through:
     M-x customize-group RET one-key RET

# Change log
2010/12/07
   * Joe Bloggs
      * Added key-binding ("C-/" by default) to jump to source file of current one-key menu for editing.
      * Made fixed menu keys configurable with variables `one-key-key-hide' `one-key-key-quit' `one-key-key-up'
        `one-key-key-down' `one-key-key-pgup' `one-key-key-pgdown' `one-key-key-help' `one-key-key-edit'
        (they are called one-key-key-??? instead of one-key-???-key so that they will group together in the
         customization buffer).
      * Deleted `one-key-highlight-prompt' function since this is not used anywhere.
      * Added new variable `one-key-column-major-order', and altered `one-key-help-format' function so that
        now you can choose whether items should be listed column first or row first.
;; 2010/11/27
   * Joe Bloggs
      * Quick fix to one-key-template-write so that it remains in one-key-template-mode after writing
      
2010/11/23
   * Joe Bloggs
      * Added `one-key-template-group-key-items-by-regexps', `one-key-template-describe-command',
        and associated keybindings and menu items.
;; 2010/11/20
   * Joe Bloggs
      * Added `one-key-template-write' function for saving *One-Key-Template* buffer in `one-key-menus-location',
        and added keybinding `one-key-template-mode' and item to `one-key-menu-one-key-template-alist'.
      
2010/11/18
   * Joe Bloggs
      * Added new major mode for editing one-key-menus in *One-Key-Template* buffer
      * Added following functions to aid editing menus in *One-Key-Template* buffer:
         `one-key-template-mode', `one-key-template-move-line-region', `one-key-template-move-line-region-up'
         `one-key-template-move-line-region-down', `one-key-template-test-menu', `one-key-template-mark-key-items'
         `one-key-template-sort-key-items-by-command-alphabetically',
         `one-key-template-sort-key-items-by-description-alphabetically',
         `one-key-template-sort-key-items-by-key-alphabetically',
         `one-key-menu-one-key-template', `one-key-menu-one-key'
      * Added keybindings for `one-key-template-mode'.
      * Altered `one-key-help-format' function so that the keys are ordered by column instead of by row.
      * Added `one-key-toplevel-alist' customizable variable and `one-key-menu-toplevel' function.
      * Added `one-key-mode-alist' customizable variable and `one-key-get-menu' function.
      * Alterend `one-key-insert-template' and `one-key-show-template' functions so that they also add
        optional (commented) code to add items to `one-key-mode-alist' and `one-key-toplevel-alist'
      * Added customization variables `one-key-menus-location', `one-key-menus-regexp' and
        `one-key-auto-load-menus', and function `one-key-load-files'.
        Added code to automatically load menus if `one-key-auto-load-menus' is set to t.
      * Fixed spelling mistakes in documentation and added documentation for new features.
;; 2010/09/27
   * Joe Bloggs
      * Altered one-key-make-template so that it adds the original keys to the descriptions of each item.
      
2010/09/21
   * Joe Bloggs
      * Fixed a problems with one-key-make-template so it should work with more keymaps
      * Added ability to get help on one-key-menu items by pressing C-? followed by item key
      * Altered header text of menu
      * Fixed bug in one-key-menu so that window pops up if one-key-popup-window is t
        (this was also fixed independently by Andy, but I'm keeping my fix since it works fine)
;; 2009/03/09
  * Andy Stewart:
     * Add `char-valid-p' for compatibility Emacs 22.
;; 2009/02/25
  * Andy Stewart:
     * Fix a bug of `one-key-menu'.
;; 2009/02/19
  * Andy Stewart:
     * Just show help message when first call function `one-key-menu',
       don't overwritten message from command.
     * Remove function `one-key-menu-quit' and
       option `one-key-show-quit-message', unnecessary now.
;; 2009/02/10
  * rubikitch
     * Fix bug.
     * PageUp and PageDown are scroll page keys now.
     * Add new option `one-key-show-quit-message'.
;; 2009/01/28
  * Andy Stewart:
     * Capitalize describe in variable `one-key-menu-*-alist'.
;; 2009/01/27
  * rubikitch
     * Fix doc.
;; 2009/01/26
  * rubikitch
     * Improve code.
;; 2009/01/25
  * Andy Stewart:
     * Applied rubikitch's patch for generate
       template code automatically, very nice!
;; 2009/01/22
  * rubikitch:
     * Add new option `one-key-items-per-line'.
     * Refactory code make it more clear.
     * Fix bug.
  * Andy Stewart:
     * Applied rubikitch's patch. Thanks!
     * Modified code make build-in keystroke
       can be overridden.
     * Fix doc.
;; 2009/01/20
  * Andy Stewart:
     * Add new option `execute-last-command-when-miss-match'
       to function `one-key-menu', make user can execute
       last input command when miss match key alist.
;; 2009/01/15
  * rubikitch:
     * Fix bug of `one-key-menu'.
     * Add recursion execute support for `one-key-menu'.*
       Thanks rubikitch patched for this! ;)
;; 2009/01/04
  * Andy Stewart:
     * Add `alternate-function' argument with function `one-key-menu'.
;; 2008/12/22
  * Andy Stewart:
     * First released.

# Acknowledgements
;;      rubikitch <rubikitch@ruby-lang.org>
             For send many patches.

# TODO
;; Add configurable colourization of menu items.
Could have alist of alists, called e.g. `one-key-colours-regexp-alist',
the keys to the list would be symbols for the one-key menu alists (e.g. 'one-key-menu-bookmark-alist)
and each value would be an alist of regexp/colour pairs.
Then when a menu is formatted, any items matching a regexp in the associated colours-regexp alist
would be coloured with the associated colour. E.g. could make items that are themselves one-key menus
all the same colour, and other items a different colour.
;; Option to automatically split menu when creating templates based on prefix keys.
;; Function to split items matching regexp into seperate menu in when editing menu in `one-key-template-mode'.
;; Automatically generate one-key menus for common keybindings and store them in memory. This is already implemented
to a certain extent but I think it could be improved. Needs further investigation.
;;; Require
