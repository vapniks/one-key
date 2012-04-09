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
;; Use apropos-command with one-key to quickly find commands and keybindings
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

;;; Customize:
;;
;; 
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
;;
;; 
;;

;;; Require


;;; Code:



(provide 'one-key-apropos)

;;; one-key-apropos.el ends here



(provide 'one-key-apropos)
;;; one-key-apropos.el ends here
