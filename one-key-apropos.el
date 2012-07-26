;;; one-key-apropos.el --- Use apropos-command with one-key to quickly find commands and keybindings

;; Filename: one-key-apropos.el
;; Description: Use apropos-command with one-key to quickly find commands and keybindings
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2012, Joe Bloggs, all rites reversed.
;; Created: 2012-04-09 05:11:07
;; Version: 0.1
;; Last-Updated: 2012-04-09 05:11:07
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/one-key-apropos.el
;; Keywords: convenience, help
;; Compatibility: GNU Emacs 24.0.50.2
;;
;; Features that might be required by this library:
;;
;; one-key, cl, hexrgb
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
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;; 
;; This library will add a new one-key menu type called "apropos-command".
;; When you add a new menu of this type to a set of one-key menus you will be prompted for a word list or regexp,
;; and new one-key menus will be added containing commands matching those words/regexps.
;;

;;; Installation:
;;
;; Put one-key-apropos.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'one-key-apropos)

;;; Customize: `one-key-apropos-special-keybindings' 
;;
;; All of the above can customized by:
;;      M-x customize-group RET one-key-apropos RET
;;

;;; Change log:
;;	
;; 2012/04/09
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; 
;;

;;; TODO
;; New special keybinding for invoking `apropos-command' again.
;; Alter menu type so that it can be added to a menu set without prompoting the user for a search term.
;;

;;; Require
(require 'apropos)

;;; Code:

(defcustom one-key-apropos-special-keybindings
  '(quit-close quit-open toggle-persistence toggle-display next-menu prev-menu up down scroll-down scroll-up
               toggle-help toggle-row/column-order sort-next sort-prev reverse-order apropos limit-items
               highlight-items edit-item delete-item swap-keys add-menu remove-menu donate report-bug)
  "List of special keys to be used for apropos-command menus (see `one-key-default-special-keybindings' for more info)."
  :group 'one-key
  :type '(repeat (symbol :tag "Name" :help-echo "The name/symbol corresponding to the keybinding.")))

(defun one-key-apropos-prompt-for-words nil
  "Prompt the user for a string of words, and return regexp to use for searching commands."
  (let ((string (apropos-read-pattern "command")))
    (apropos-parse-pattern string)))

(defun one-key-apropos-build-menus (regexp)
  "Build a `one-key' menu of commands that match REGEXP from the output of the apropos-command function."
  (setq apropos-accumulator (apropos-internal apropos-regexp 'commandp))
  (let* ((items apropos-accumulator)
         (cmds (loop for symbol in apropos-accumulator
                     if (not (or (get symbol 'apropos-inhibit)
                                 (apropos-false-hit-symbol symbol)))
                     collect symbol))
         (descs (mapcar (lambda (cmd)
                          (capitalize
                           (replace-regexp-in-string "-" " " (symbol-name cmd)))) cmds))
         (keys (loop for cmd in cmds
                     for key = (where-is-internal cmd nil t)
                     for nokey = (or (not key)
                                     (loop for i to (1- (length key))
                                           if (or (framep (aref key i))
                                                  (bufferp (aref key i)))
                                           return t))
                     collect (if nokey nil key)))
         (descs2 (one-key-append-keys-to-descriptions descs keys)))
    (setq apropos-accumulator nil)
    (one-key-create-menu-lists cmds descs2 keys)))

;; Set menu-alist, title string and special keybindings for new `apropos-command' menus
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "apropos-command"
                            (lambda (name) (string-match "apropos-command" name))
                            (lambda (name)
                              (let* ((regexp (one-key-apropos-prompt-for-words))
                                     (menus (one-key-apropos-build-menus regexp))
                                     (nummenus (length menus))
                                     (names (one-key-append-numbers-to-menu-name name nummenus)))
                                (cons names menus)))
                            nil
                            'one-key-apropos-special-keybindings) t)

(provide 'one-key-apropos)
;;; one-key-apropos.el ends here
