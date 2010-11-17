;;; one-key-config.el --- Configuration for one-key.el

;; Filename: one-key-config.el
;; Description: Configuration for one-key.el
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-23 18:07:50
;; Version: 0.1
;; Last-Updated: 2008-12-23 18:07:50
;;           By: Andy Stewart
;; URL:
;; Keywords: one-key
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `one-key'
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
;; This package is some example to use `one-key'.
;;

;;; Installation:
;;
;; Put one-key-config.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'one-key-config)
;;
;; No need more.

;;; Change log:
;;
;; 2008/12/23
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'one-key)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMMS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defvar one-key-menu-emms-alist nil
;;   "The `one-key' menu list for EMMS.")

;; (setq one-key-menu-emms-alist
;;       '(
;;         (("g" . "Playlist Go") . emms-playlist-mode-go)
;;         (("d" . "Play Directory Tree") . emms-play-directory-tree)
;;         (("f" . "Play File") . emms-play-file)
;;         (("i" . "Play Playlist") . emms-play-playlist)
;;         (("F" . "Play Find") . emms-play-find)
;;         (("t" . "Add Directory Tree") . emms-add-directory-tree)
;;         (("c" . "Toggle Repeat Track") . emms-toggle-repeat-track)
;;         (("v" . "Jump To File") . emms-jump-to-file)
;;         (("w" . "Toggle Repeat Playlist") . emms-toggle-repeat-playlist)
;;         (("z" . "Show") . emms-show)
;;         (("l" . "Lyrics Toggle Show") . emms-lyrics-toggle-display-on-minibuffer)
;;         (("r" . "Lyrics Re download") . emms-lyrics-redownload-lyric)
;;         (("e" . "Lyrics Visit") . emms-lyrics-visit-lyric)
;;         (("s" . "Emms Streams") . emms-streams)
;;         (("b" . "Emms Browser") . emms-browser)
;;         (("B" . "Emms Browser Commands") . one-key-menu-emms-browser)
;;         (("S" . "Emms Stream Commands") . one-key-menu-emms-stream)
;;         (("P" . "Emms Playlist Commands") . one-key-menu-emms-playlist)
;; 	(("C-b" . "Back to previous menu") . one-key-menu-toplevel)
;; 	))

;; (defun one-key-menu-emms ()
;;   "The `one-key' men for EMMS."
;;   (interactive)
;;   (one-key-menu "EMMS" one-key-menu-emms-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ECB ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-ecb-alist nil
  "The `one-key' menu list for ECB.")

(setq one-key-menu-ecb-alist
      '(
        (("s" . "Sources") . ecb-goto-window-sources)
        (("h" . "History") . ecb-goto-window-history)
        (("d" . "Directory") . ecb-goto-window-directories)
        (("m" . "Methods") . ecb-goto-window-methods)
        (("e" . "Edit") . ecb-goto-window-edit1)))

(defun one-key-menu-ecb ()
  "The `one-key' men for ECB."
  (interactive)
  (one-key-menu "ECB" one-key-menu-ecb-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Gtags ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-gtags-alist nil
  "The `one-key' menu list for GTAGS.")

(setq one-key-menu-gtags-alist
      '(
        (("," . "Find Tag Define") . xgtags-find-tag-from-here)
        (("." . "Find Tag Reference (No Prompt)") . xgtags-find-rtag-no-prompt)
        ((">" . "Find Tag Reference") . xgtags-find-rtag)
        (("t" . "Search Tag Define") . xgtags-find-tag)
        (("s" . "Find Symbol") . xgtags-find-symbol)
        (("p" . "Find Pattern") . xgtags-find-pattern)
        (("/" . "Pop Stack") . xgtags-pop-stack)
        (("b" . "Switch Current Window") . xgtags-switch-to-buffer)
        (("o" . "Switch Other Window") . xgtags-switch-to-buffer-other-window)
        (("x" . "Parse File") . xgtags-parse-file)
        (("f" . "Find File") . xgtags-find-file)
        (("g" . "Find With Grep") . xgtags-find-with-grep)
        (("i" . "Find With Idutils") . xgtags-find-with-idutils)
        (("m" . "Make Complete List") . xgtags-make-complete-alist)
        (("q" . "Query Replace Regexp") . xgtags-query-replace-regexp)
        (("v" . "Visit Root Directory") . xgtags-visit-rootdir)
        (("r" . "Return Window") . xgtags-select-tag-return-window)))

(defun one-key-menu-gtags ()
  "The `one-key' men for GTAGS."
  (interactive)
  (one-key-menu "GTAGS" one-key-menu-gtags-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Hideshow ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-hideshow-alist nil
  "The `one-key' menu list for HIDESHOW.")

(setq one-key-menu-hideshow-alist
      '(
        (("s" . "Show Block") . hs-show-block)
        (("h" . "Hide Block") . hs-hide-block)
        (("c" . "Toggle Hiding") . hs-toggle-hiding)
        (("j" . "Show All") . hs-show-all)
        (("k" . "Hide All") . hs-hide-all)))

(defun one-key-menu-hideshow ()
  "The `one-key' men for HIDESHOW."
  (interactive)
  (one-key-menu "HIDESHOW" one-key-menu-hideshow-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Festival ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-festival-alist nil
  "The `one-key' menu list for FESTIVAL.")

(setq one-key-menu-festival-alist
      '(
        (("s" . "Stop") . festival-stop)
        (("a" . "Say") . festival-say)
        (("f" . "Read File") . festival-read-file)
        (("b" . "Read Buffer") . festival-read-buffer)
        (("r" . "Read Region") . festival-read-region)
        (("w" . "Read Word") . festival-read-word)))

(defun one-key-menu-festival ()
  "The `one-key' men for FESTIVAL."
  (interactive)
  (one-key-menu "FESTIVAL" one-key-menu-festival-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-ui-alist nil
  "The `one-key' menu list for UI.")

(setq one-key-menu-ui-alist
      '(
        (("t" . "Tool-Bar") . tool-bar-mode)
        (("m" . "Menu-Bar") . menu-bar-mode)
        (("c" . "Scroll-Bar") . scroll-bar-mode)))

(defun one-key-menu-ui ()
  "The `one-key' men for UI."
  (interactive)
  (one-key-menu "UI" one-key-menu-ui-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Open Directory ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-directory-alist nil
  "The `one-key' menu list for DIRECTORY.")

(setq one-key-menu-directory-alist
      '(
        (("h" . "Home") . (lambda () (interactive) (dired-x-find-file my-home-directory)))
        (("e" . "Emacs Backup") . (lambda () (interactive) (dired-x-find-file my-emacs-backup-directory)))
        (("d" . "Download") . (lambda () (interactive) (dired-x-find-file my-default-download-directory)))
        (("b" . "Book") . (lambda () (interactive) (dired-x-find-file my-book-directory)))
        (("i" . "Image") . (lambda () (interactive) (dired-x-find-file my-picture-directory)))
        (("p" . "Emacs Package") . (lambda () (interactive) (dired-x-find-file my-emacs-lisp-package-directory)))
        (("k" . "Mldonkey") . (lambda () (interactive) (dired-x-find-file my-mldonkey-download-directory)))
        (("m" . "Music") . (lambda () (interactive) (dired-x-find-file my-music-default-directory)))
        (("s" . "Screenshots") . (lambda () (interactive) (dired-x-find-file my-screenshots-storage-directory)))
        (("r" . "Resource Backup") . (lambda () (interactive) (dired-x-find-file my-resource-backup-directory)))
        (("n" . "Notes") . (lambda () (interactive) (dired-x-find-file my-notes-directory)))
        (("x" . "Reading") . (lambda () (interactive) (dired-x-find-file my-reading-directory)))
        (("l" . "Lyrics") . (lambda () (interactive) (dired-x-find-file my-lyrics-directory)))
        (("u" . "Emule") . (lambda () (interactive) (dired-x-find-file my-emlue-download-directory)))
        (("z" . "Elisp") . (lambda () (interactive) (dired-x-find-file my-elisp-directory)))
        ))

(defun one-key-menu-directory ()
  "The `one-key' men for DIRECTORY."
  (interactive)
  (one-key-menu "DIRECTORY" one-key-menu-directory-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Cycle Buffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-cycle-buffer-alist nil
  "The `one-key' menu list for CYCLE-BUFFER.")

(setq one-key-menu-cycle-buffer-alist
      '(
        (("l" . "Elisp") . (lambda () (interactive) (cycle-buffer-in-special-mode 'emacs-lisp-mode)))
        (("i" . "IRC") . (lambda () (interactive) (cycle-buffer-in-special-mode 'erc-mode)))
        (("d" . "Dired") . (lambda () (interactive) (cycle-buffer-in-special-mode 'dired-mode)))
        (("o" . "Org") . (lambda () (interactive) (cycle-buffer-in-special-mode 'org-mode)))
        (("h" . "Haskell") . (lambda () (interactive) (cycle-buffer-in-special-mode 'haskell-mode)))
        (("w" . "W3m") . (lambda () (interactive) (cycle-buffer-in-special-mode 'w3m-mode)))
        (("t" . "Term") . (lambda () (interactive) (cycle-buffer-in-special-mode 'term-mode)))
        (("y" . "Yaoddmuse") . (lambda () (interactive) (cycle-buffer-in-special-mode 'yaoddmuse-mode)))
        ))

(defun one-key-menu-cycle-buffer ()
  "The `one-key' men for CYCLE-BUFFER."
  (interactive)
  (one-key-menu "CYCLE-BUFFER" one-key-menu-cycle-buffer-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Backup File ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-backup-file-alist nil
  "The `one-key' menu list for BACKUP-FILE.")

(setq one-key-menu-backup-file-alist
      '(
        (("e" . "Emacs") . (lambda () (interactive) (shell-aliase "bake")))
        (("x" . "XMonad") . (lambda () (interactive) (shell-aliase "bakx")))
        (("q" . "Qemu") . (lambda () (interactive) (shell-aliase "bakq")))
        (("v" . "VirtualBox") . (lambda () (interactive) (shell-aliase "bakv")))
        (("s" . "Stardict") . (lambda () (interactive) (shell-aliase "baks")))
        (("c" . "Configure File") . (lambda () (interactive) (shell-aliase "bakc")))
        (("p" . "Projects") . (lambda () (interactive) (shell-aliase "bakp")))
        (("a" . "All") . (lambda () (interactive) (shell-aliase "bak")))
        ))

(defun one-key-menu-backup-file ()
  "The `one-key' men for BACKUP-FILE."
  (interactive)
  (one-key-menu "BACKUP-FILE" one-key-menu-backup-file-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Boxquote ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-boxquote-alist nil
  "The `one-key' menu list for BOXQUOTE.")

(setq one-key-menu-boxquote-alist
      '(
        (("z" . "Boxquote") . boxquote-boxquote)
        (("b" . "Buffer") . boxquote-buffer)
        (("d" . "Defun") . boxquote-defun)
        (("f" . "Describe Function") . boxquote-describe-function)
        (("k" . "Describe Key") . boxquote-describe-key)
        (("v" . "Describe Variable") . boxquote-describe-variable)
        (("o" . "Fill Paragraph") . boxquote-fill-paragraph)
        (("i" . "Insert File") . boxquote-insert-file)
        (("x" . "Kill") . boxquote-kill)
        (("r" . "Kill Ring Save") . boxquote-kill-ring-save)
        (("n" . "Narrow To Boxquote") . boxquote-narrow-to-boxquote)
        (("q" . "Narrow To Boxquote Content") . boxquote-narrow-to-boxquote-content)
        (("p" . "Paragraph") . boxquote-paragraph)
        (("g" . "Region") . boxquote-region)
        (("s" . "Shell Command") . boxquote-shell-command)
        (("t" . "Text") . boxquote-text)
        (("L" . "Title") . boxquote-title)
        (("u" . "Unbox") . boxquote-unbox)
        (("y" . "Yank") . boxquote-yank)
        (("w" . "Where Is") . boxquote-where-is)
        (("h" . "Unbox Region") . boxquote-unbox-region)
        ))

(defun one-key-menu-boxquote ()
  "The `one-key' men for BOXQUOTE."
  (interactive)
  (one-key-menu "BOXQUOTE" one-key-menu-boxquote-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Cscope ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-cscope-alist nil
  "The `one-key' menu list for CSCOPE.")

(setq one-key-menu-cscope-alist
      '(
        (("s" . "This Symbol") . cscope-find-this-symbol)
        (("d" . "Definition Prompt") . cscope-find-global-definition)
        (("g" . "Definition No Prompt") . cscope-find-global-definition-no-prompting)
        (("f" . "This File") . cscope-find-this-file)
        (("i" . "Including This File") . cscope-find-files-including-file)
        (("c" . "Calling This Function") . cscope-find-functions-calling-this-function)
        (("e" . "This Function Called") . cscope-find-called-functions)
        (("p" . "Pattern") . cscope-find-egrep-pattern)
        (("t" . "This String") . cscope-find-this-text-string)))

(defun one-key-menu-cscope ()
  "The `one-key' men for CSCOPE."
  (interactive)
  (one-key-menu "CSCOPE" one-key-menu-cscope-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Dired Sort ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-dired-sort-alist nil
  "The `one-key' menu list for DIRED-SORT.")

(setq one-key-menu-dired-sort-alist
      '(
        (("s" . "Size") . dired-sort-size)
        (("x" . "Extension") . dired-sort-extension)
        (("n" . "Name") . dired-sort-name)
        (("t" . "Modified Time") . dired-sort-time)
        (("u" . "Access Time") . dired-sort-utime)
        (("c" . "Create Time") . dired-sort-ctime)))

(defun one-key-menu-dired-sort ()
  "The `one-key' men for DIRED-SORT."
  (interactive)
  (one-key-menu "DIRED-SORT" one-key-menu-dired-sort-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Window Navigation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-window-navigation-alist nil
  "The `one-key' menu list for WINDOW-NAVIGATION.")

(setq one-key-menu-window-navigation-alist
      '(
        (("j" . "Downward") . windmove-down)
        (("k" . "Upward") . windmove-up)
        (("h" . "Leftward") . windmove-left)
        (("l" . "Rightward") . windmove-right)
        (("s" . "Move Down") . buf-move-down)
        (("d" . "Move Up") . buf-move-up)
        (("a" . "Move Left") . buf-move-left)
        (("f" . "Move Right") . buf-move-right)
        (("u" . "Enlarge Down") . (lambda () (interactive) (windresize-up-inwards '-1)))
        (("i" . "Enlarge Up") . (lambda () (interactive) (windresize-down-inwards '-1)))
        (("y" . "Enlarge Left") . (lambda () (interactive) (windresize-right-inwards '-1)))
        (("o" . "Enlarge Right") . (lambda () (interactive) (windresize-left-inwards '-1)))
        (("m" . "Shrink Down") . (lambda () (interactive) (windresize-up-inwards '1)))
        (("," . "Shrink Up") . (lambda () (interactive) (windresize-down-inwards '1)))
        (("n" . "Shrink Left") . (lambda () (interactive) (windresize-right-inwards '1)))
        (("." . "Shrink Right") . (lambda () (interactive) (windresize-left-inwards '1)))
        (("x" . "Outward Window") . outward-window)
        (("c" . "Inward Window") . inward-window)
        (("7" . "Tabbar Left") . tabbar-backward-tab)
        (("8" . "Tabbar Right") . tabbar-forward-tab)
        (("9" . "Tabbar Next") . tabbar-backward-group)
        (("0" . "Tabbar Previous") . tabbar-forward-group)
        ((";" . "Kill Buffer") . kill-this-buffer)
        ((":" . "Kill Other Windows") . delete-other-windows)
        (("'" . "Kill Buffer And Window") . delete-current-buffer-and-window)
        (("b" . "Anything Mode") . anything)
        (("e" . "List Registers") . list-registers)
        (("r" . "Remember Register") . frame-configuration-to-register)
        (("t" . "Jump Register") . jump-to-register)
        (("g" . "Split Horizontally") . split-window-horizontally)
        (("v" . "Split Vertically") . split-window-vertically)
        (("z" . "Window Number Jump") . window-number-jump)
        ))

(defun one-key-menu-window-navigation ()
  "The `one-key' men for WINDOW-NAVIGATION."
  (interactive)
  (one-key-menu "WINDOW-NAVIGATION" one-key-menu-window-navigation-alist t t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; W3m Search ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-w3m-search-alist nil
  "The `one-key' menu list for W3M-SEARCH.")

(setq one-key-menu-w3m-search-alist
      '(
        (("L" . "Google Lucky") . w3m-search-google-lucky)
        (("s" . "Google Web CN") . w3m-search-google-web-cn)
        (("e" . "Google Web EN") . w3m-search-google-web-en)
        (("f" . "Google File") . w3m-search-google-file)
        (("i" . "Google Image") . w3m-search-google-image)
        (("c" . "Google Code") . w3m-search-google-code)
        (("g" . "Google Group") . w3m-search-google-group)
        (("b" . "Google Blog") . w3m-search-google-blog)
        (("t" . "Google News Sci/Tech CN") . w3m-search-google-news-cn-Sci/Tech)
        (("T" . "Google News Sci/Tech EN") . w3m-search-google-news-en-Sci/Tech)
        (("k" . "Google Desktop") . w3m-search-google-desktop)
        (("o" . "Gmail") . w3m-auto-logon-gmail)
        (("w" . "Emacs Wiki") . w3m-search-emacswiki)
        (("r" . "Emacs Wiki Random") . w3m-search-emacswiki-random)
        (("h" . "Haskell Wiki") . w3m-search-haskell-wiki)
        (("u" . "Haskell Hoogle") . w3m-search-haskell-hoogle)
        (("m" . "BaiDu MP3") . w3m-search-baidu-mp3)
        (("d" . "Dict CN") . w3m-search-dict-cn)
        (("l" . "Lispdoc Basic") . w3m-search-lispdoc-basic)
        (("L" . "Lispdoc Full") . w3m-search-lispdoc-full)
        ((";" . "Slang") . w3m-search-slang)
        (("a" . "Answer") . w3m-search-answers)
        (("p" . "Wikipedia CN") . w3m-search-wikipedia-cn)
        (("P" . "Wikipedia EN") . w3m-search-wikipedia-en)
        (("n" . "RFC Number") . w3m-search-rfc-number)
        (("y" . "Insert Default Input") . w3m-search-advance-insert-search-object)
        ))

(defun one-key-menu-w3m-search ()
  "The `one-key' men for W3M-SEARCH."
  (interactive)
  (let ((current-mode major-mode))
    (one-key-menu "W3M-SEARCH" one-key-menu-w3m-search-alist t nil nil
                  '(lambda ()
                     (interactive)
                     (unless (eq current-mode 'w3m-mode)
                       (w3m))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Etags ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defvar one-key-menu-etags-alist nil
;;   "The `one-key' menu list for ETAGS.")

;; (setq one-key-menu-etags-alist
;;       '(
;;         (("G" . "Generate Tag Table") . generate-tag-table-of-emacs)
;;         (("w" . "Find Tag Window") . find-tag-window)
;;         (("W" . "Find Tag Window Small") . release-small-tag-window)
;;         (("," . "Find Tag+") . find-tag+)
;;         (("." . "Find Tag") . find-tag)
;;         (("p" . "Pop Tag Mark") . pop-tag-mark)
;;         (("r" . "Find Tag Regexp") . find-tag-regexp)
;;         (("s" . "Tags Search") . tags-search)
;;         (("Q" . "Tags Query Replace") . tags-query-replace)
;;         ))

;; (defun one-key-menu-etags ()
;;   "The `one-key' men for ETAGS."
;;   (interactive)
;;   (let ((current-mode major-mode))
;;     (one-key-menu "ETAGS" one-key-menu-etags-alist t t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Gnus Summary Sort ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-gnus-summary-sort-alist nil
  "The `one-key' menu list for GNUS-SUMMARY-SORT.")

(setq one-key-menu-gnus-summary-sort-alist
      '(
        (("a" . "Author") . gnus-summary-sort-by-author)
        (("c" . "Chars") . gnus-summary-sort-by-chars)
        (("d" . "Date") . gnus-summary-sort-by-date)
        (("e" . "Score") . gnus-summary-sort-by-score)
        (("l" . "Lines") . gnus-summary-sort-by-lines)
        (("n" . "Number") . gnus-summary-sort-by-number)
        (("o" . "Original") . gnus-summary-sort-by-original)
        (("x" . "Random") . gnus-summary-sort-by-random)
        (("s" . "Subject") . gnus-summary-sort-by-subject)
        (("i" . "Recipient") . gnus-summary-sort-by-recipient)
        (("r" . "Reverse") . gnus-summary-sort-by-reverse)
        ))

(defun one-key-menu-gnus-summary-sort ()
  "The `one-key' men for GNUS-SUMMARY-SORT."
  (interactive)
  (let ((current-mode major-mode))
    (one-key-menu "GNUS-SUMMARY-SORT" one-key-menu-gnus-summary-sort-alist t t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Yaoddmuse ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-yaoddmuse-alist nil
  "The `one-key' menu list for YAODDMUSE.")

(setq one-key-menu-yaoddmuse-alist
      '(
        (("e" . "Edit Default") . yaoddmuse-edit-default)
        (("E" . "Edit") . yaoddmuse-edit)
        (("b" . "Post Current Buffer") . yaoddmuse-post-current-buffer)
        (("B" . "Post Buffer") . yaoddmuse-post-buffer)
        (("l" . "Post Library Default") . yaoddmuse-post-library-default)
        (("L" . "Post Library") . yaoddmuse-post-library)
        (("o" . "Follow") . yaoddmuse-follow)
        (("f" . "Post File") . yaoddmuse-post-file)
        (("d" . "Past Dired") . yaoddmuse-post-dired)
        (("r" . "Revert") . yaoddmuse-revert)
        (("v" . "Browse Page") . yaoddmuse-browse-page)
        (("s" . "Brose This Page") . yaoddmuse-browse-current-page)
        (("i" . "Insert Pagename") . yaoddmuse-insert-pagename)
        (("k" . "Kill Url") . yaoddmuse-kill-url)
        ))

(defun one-key-menu-yaoddmuse ()
  "The `one-key' men for YAODDMUSE."
  (interactive)
  (let ((current-mode major-mode))
    (one-key-menu "YAODDMUSE" one-key-menu-yaoddmuse-alist t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Thing-Edit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-thing-edit-alist nil
  "The `one-key' menu list for THING-EDIT.")

(setq one-key-menu-thing-edit-alist
      '(
        ;; Copy.
        (("w" . "Copy Word") . thing-copy-word)
        (("s" . "Copy Symbol") . thing-copy-symbol)
        (("m" . "Copy Email") . thing-copy-email)
        (("f" . "Copy Filename") . thing-copy-filename)
        (("u" . "Copy URL") . thing-copy-url)
        (("x" . "Copy Sexp") . thing-copy-sexp)
        (("g" . "Copy Page") . thing-copy-page)
        (("t" . "Copy Sentence") . thing-copy-sentence)
        (("o" . "Copy Whitespace") . thing-copy-whitespace)
        (("i" . "Copy List") . thing-copy-list)
        (("c" . "Copy Comment") . thing-copy-comment)
        (("h" . "Copy Function") . thing-copy-defun)
        (("p" . "Copy Parentheses") . thing-copy-parentheses)
        (("l" . "Copy Line") . thing-copy-line)
        (("a" . "Copy To Line Begin") . thing-copy-to-line-beginning)
        (("e" . "Copy To Line End") . thing-copy-to-line-end)
        ;; Copy.
        (("W" . "Paste Word") . thing-paste-word)
        (("S" . "Paste Symbol") . thing-paste-symbol)
        (("M" . "Paste Email") . thing-paste-email)
        (("F" . "Paste Filename") . thing-paste-filename)
        (("U" . "Paste URL") . thing-paste-url)
        (("X" . "Paste Sexp") . thing-paste-sexp)
        (("G" . "Paste Page") . thing-paste-page)
        (("T" . "Paste Sentence") . thing-paste-sentence)
        (("O" . "Paste Whitespace") . thing-paste-whitespace)
        (("I" . "Paste List") . thing-paste-list)
        (("C" . "Paste Comment") . thing-paste-comment)
        (("H" . "Paste Function") . thing-paste-defun)
        (("P" . "Paste Parentheses") . thing-paste-parentheses)
        (("L" . "Paste Line") . thing-paste-line)
        (("A" . "Paste To Line Begin") . thing-paste-to-line-beginning)
        (("E" . "Paste To Line End") . thing-paste-to-line-end)
        ))

(defun one-key-menu-thing-edit ()
  "The `one-key' men for THING-EDIT."
  (interactive)
  (let ((current-mode major-mode))
    (one-key-menu "THING-EDIT" one-key-menu-thing-edit-alist t)))

(provide 'one-key-config)

;;; one-key-config.el ends here

;;; LocalWords:  config EMMS emms Playlist redownload ECB ecb Gtags gtags rtag
;;; LocalWords:  xgtags idutils rootdir Hideshow hideshow hs UI ui Mldonkey bak
;;; LocalWords:  bakx bakq bakv baks bakc bakp Boxquote boxquote Unbox unbox
;;; LocalWords:  Cscope cscope egrep utime Tabbar logon Etags etags buf
;;; LocalWords:  windresize
