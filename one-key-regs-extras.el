;;; one-key-regs-extras.el --- Extra register types for `one-key-regs'

;; Filename: one-key-regs-extras.el
;; Description: Extra register types for `one-key-regs'
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2012, Joe Bloggs, all rites reversed.
;; Created: 2012-02-10 00:03:58
;; Version: 0.1
;; Last-Updated: 2012-02-10 00:03:58
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/one-key-regs-extras.el
;; Keywords: abbrev, convenience, files, frames, tools
;; Compatibility: GNU Emacs 24.0.50.2
;;
;; Features that might be required by this library:
;;
;; one-key, one-key-regs, bookmark+, run-assoc, policy-switch, desktop, auth-source, sql, ess, ido, python,
;; haskell, octave, matlab, slime, shell, term, js-comint, gnuplot, telnet, bashdb
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
;; Bitcoin donations gratefully accepted: 1HnSqGHrVenb1t2V2aijyocWyZcd7qt1k

;; Extra register types for `one-key-regs', including starting various different types of processes (shell, database
;; clients, repl's, debuggers, etc.), bookmarks, desktops, and window configs (using policy switch).
;; If you want to use auth-source for obtaining login credentials for the database registers then you should set
;; `auth-source-do-cache' to nil.
;; 

;;; Installation:
;;
;; Put one-key-regs-extras.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file, after the lines that `require' other packages needed by this
;; library.
;;
;; (require 'one-key-regs-extras)

;;; Customize:
;;  
;;  one-key-regs-processes : A list of processes and associated functions for the custom register type 'start-process.
;;
;; All of the above can customized by:
;;      M-x customize-group RET one-key-regs-extras RET
;;

;;; Change log:
;;	
;; 2012/02/10
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
(require 'one-key-regs)
;;; Code:

(defcustom one-key-regs-processes nil
  "A list of processes and associated functions for the custom register type 'start-process.
Each item in the list is a list of three items (NAME FUNC OPTIONS-FUNC).
NAME is a name for the process (a string) which the user can select when prompted for a process type.
FUNC is a function for starting the process which takes one argument which may contain extra options for starting the
process.
OPTIONS-FUNC is a function with no arguments which is called when the register is created and returns the value of the
options argument to be passed to FUNC."
  :group 'one-key-regs
  :type '(repeat (list (string :tag "Process name" :help-echo "A name for processes of this type (for the user to select when prompted).")
                       (function :tag "Startup function" :help-echo "A function for starting the process. It should take a list of options as it's only argument.")
                       (function :tag "Options function" :help-echo "A function called when the register is created. It should return the value that will be passed as the options argument to the startup function."))))

(add-to-list 'one-key-regs-custom-register-types
             '(start-process
               (let* ((type (if (featurep 'ido)
                                (ido-completing-read "Process type: "
                                                     (mapcar 'car one-key-regs-processes) nil t)
                              (completing-read "Process type: "
                                               (mapcar 'car one-key-regs-processes) nil t)))
                      (proctype (assoc type one-key-regs-processes))
                      (startfun (second proctype))
                      (optfun (third proctype))
                      (options (funcall optfun)))
                 `(let ((type ,type)) (funcall ,startfun ',options)))
               (lambda (reg) (format "Start %s process" (cadar (third reg))))))
;; add processes that only need the starting directory to be set
(let ((proclist '(("shell" . shell)
                  ("eshell" . eshell)
                  ("python-shell" . python-shell)
                  ("octave" . run-octave)
                  ("matlab" . matlab-shell)
                  ("interactive elisp" . ielm)
                  ("slime" . slime)
                  ("haskell" . run-haskell))))
  (loop for (name . func) in proclist
        if (and (functionp func) (not (assoc name one-key-regs-processes)))
        do (add-to-list 'one-key-regs-processes (list name
                                                      `(lambda (options)
                                                         (let ((default-directory options))
                                                           (funcall ',func)))
                                                      (lambda nil
                                                        (let ((dir (if (featurep 'ido)
                                                                       (ido-read-directory-name "Starting directory: "
                                                                                                nil nil t)
                                                                     (read-directory-name "Starting directory: "
                                                                                          nil nil t))))
                                                          dir))))))

;; terminal process
(if (and (functionp 'term) (not (assoc "terminal" one-key-regs-processes)))
    (add-to-list 'one-key-regs-processes (list "terminal"
                                               `(lambda (options)
                                                  (let ((default-directory (car options))
                                                        (cmd (cdr options)))
                                                    (funcall 'term cmd)))
                                               (lambda nil
                                                 (let ((dir (if (featurep 'ido)
                                                                (ido-read-directory-name "Starting directory: "
                                                                                         nil nil t)
                                                              (read-directory-name "Starting directory: "
                                                                                   nil nil t)))
                                                       (cmd (read-string "Run program: "
                                                                         (or explicit-shell-file-name
                                                                             (getenv "ESHELL")
                                                                             (getenv "SHELL")
                                                                             "/bin/sh"))))
                                                   (cons dir cmd))))))
;; javascript repl process
(if (and (functionp 'run-js) (not (assoc "javascript" one-key-regs-processes)))
    (add-to-list 'one-key-regs-processes (list "javascript"
                                               `(lambda (options)
                                                  (let ((default-directory (car options))
                                                        (cmd (cdr options)))
                                                    (funcall 'run-js cmd)))
                                               (lambda nil
                                                 (let ((dir (if (featurep 'ido)
                                                                (ido-read-directory-name "Starting directory: "
                                                                                         nil nil t)
                                                              (read-directory-name "Starting directory: "
                                                                                   nil nil t)))
                                                       (cmd (read-string "Run program: " inferior-js-program-command)))
                                                   (cons dir cmd))))))

;; ESS statistics programs
(loop for procname in '("R" "stata" "SAS" "S+3" "S+5" "S+6")
      for func = (intern-soft procname)
      if (and (functionp func) (not (assoc procname one-key-regs-processes)))
      do (add-to-list 'one-key-regs-processes
                      (list procname
                            `(lambda (dir)
                               (let ((buf (loop for procn in (remove-if-not
                                                              (lambda (x)
                                                                (eq (compare-strings
                                                                     (car x) 0 (length ,procname)
                                                                     ,procname 0 (length ,procname)
                                                                     t) t))
                                                              ess-process-name-list)
                                                for proc = (get-process (car procn))
                                                for buf = (if proc (process-buffer proc))
                                                for bufdir = (if buf (with-current-buffer buf default-directory))
                                                if (and bufdir (equal dir bufdir)) do (return buf))))
                                 (if buf (switch-to-buffer buf)
                                   (let ((default-directory dir)
                                         (ess-directory dir)
                                         (ess-ask-for-ess-directory nil))
                                     (funcall ',func)))))
                            (lambda nil
                              (if (featurep 'ido)
                                  (ido-read-directory-name "Starting directory: "
                                                           nil nil t)
                                (read-directory-name "Starting directory: "
                                                     nil nil t))))))

;; gnuplot process
(if (and (require 'gnuplot nil t)
         (not (assoc "gnuplot" one-key-regs-processes)))
    (add-to-list 'one-key-regs-processes
                 (list "gnuplot"
                       (lambda (dir)
                         (let ((default-directory dir))
                           (gnuplot-make-gnuplot-buffer)
                           (switch-to-buffer gnuplot-buffer)))
                       (lambda nil
                         (let ((dir (if (featurep 'ido)
                                        (ido-read-directory-name "Starting directory: "
                                                                 nil nil t)
                                      (read-directory-name "Starting directory: "
                                                           nil nil t))))
                           dir)))))
;; remote login processes
(if (and (functionp 'ssh)
         (not (assoc "ssh" one-key-regs-processes)))
    (add-to-list 'one-key-regs-processes
                 (list "ssh"
                       (lambda (args)
                         (ssh args))
                       (lambda nil
                         (concat (read-string "Username: ") "@"
                                 (read-string "Host: ")
                                 (read-string "Further command line args: "))))))
(if (and (functionp 'telnet)
         (not (assoc "telnet" one-key-regs-processes)))
    (add-to-list 'one-key-regs-processes
                 (list "telnet"
                       (lambda (options)
                         (telnet (car options) (cdr options)))
                       (lambda nil
                         (cons (read-string "Host: ")
                               (read-number "Port: " 23))))))
;; database clients
(if (require 'sql nil t)
    (progn
      (loop for name in (mapcar (lambda (x) (symbol-name (car x))) sql-product-alist)
            for symname = (intern name)
            for func = (intern-soft (concat "sql-" name))
            if (and (functionp func) (not (assoc name one-key-regs-processes)))
            do (add-to-list 'one-key-regs-processes
                            (list name
                                  `(lambda (options)
                                     (funcall 'one-key-regs-sql-product-function ',symname options))
                                  (lambda nil
                                    (one-key-regs-get-database-login-details)))))

      (defun one-key-regs-get-database-login-details nil
        "Get database login details from user."
        (let (authsrc user database server port bufname)
          (if (and (featurep 'auth-source) (y-or-n-p "Use auth-source for login credentials?"))
              (let* ((host (read-string "auth-source host: "))
                     (protocol (read-string "auth-source port (leave blank if none specified in auth-source): ")))
                (setq authsrc (list host protocol))
                (setq user (auth-source-user-or-password "login" host protocol)))
            (setq authsrc nil)
            (setq user (read-string "Username: " nil nil user-login-name)))
          (setq database (read-string "Database: "))
          (setq server (read-string "Server (default localhost): " nil nil "localhost"))
          (setq port (read-number "Port (leave blank for default): " 0))
          (setq bufname (format "%s@%s.%s" user database server))
          (list authsrc user database server port bufname)))

      (defun one-key-regs-sql-product-function (&optional product options)
        "Copy of `sql-product-interactive' function which doesn't prompt for login credentials.
Instead the values passed in OPTIONS are used instead.
The password will still be prompted for if the first element of options is non-nil, otherwise
it will be obtained using `auth-source-user-or-password' and the first element of options which
should be a list containing the host and protocol to use. Note that this only seems to work if
 `auth-source-do-cache' is nil.
See `sql-product-interactive' for more details."
        ;; make sure the required sql functions are loaded
        (require 'sql)
        ;; If we have a product and it has a interactive mode
        (when (sql-get-product-feature product :sqli-comint-func)
          ;; If their is already a SQL buffer connected to the database we want, use that
          (let* ((new-name (sixth options))
                 (buf (find-if (lambda (x)
                                 (string-match (regexp-opt (list new-name))
                                               (buffer-name x)))
                               (buffer-list))))
            (if buf (pop-to-buffer buf)
              ;; We have a new name or sql-buffer doesn't exist or match.
              ;; Save old login credentials, set new ones and remember where we start.
              (let* ((old-sql-user sql-user)
                     (old-sql-password sql-password)
                     (old-sql-database sql-database)
                     (old-sql-server sql-server)
                     (old-sql-port sql-port)
                     (authsrc (first options))
                     (host (car authsrc))
                     (protocol (cadr authsrc))
                     (sql-user (second options))
                     (sql-password
                      (if authsrc (auth-source-user-or-password "password" host protocol)
                        nil))
                     (sql-database (third options))
                     (sql-server (fourth options))
                     (sql-port (fifth options))
                     (start-buffer (current-buffer))
                     new-sqli-buffer)
                ;; Get password if necessary
                (if (and (memq 'password (sql-get-product-feature product :sqli-login))
                         (not sql-password))
                    (apply 'sql-get-login '(password)))
                ;; Connect to database.
                (message "Login...")
                (funcall (sql-get-product-feature product :sqli-comint-func)
                         product
                         (sql-get-product-feature product :sqli-options))
                ;; Set SQLi mode.
                (setq new-sqli-buffer (current-buffer))
                (let ((sql-interactive-product product))
                  (sql-interactive-mode))
                ;; Set the new buffer name
                (when new-name
                  (sql-rename-buffer new-name))
                ;; Set `sql-buffer' in the new buffer and the start buffer
                (setq sql-buffer (buffer-name new-sqli-buffer))
                (with-current-buffer start-buffer
                  (setq sql-buffer (buffer-name new-sqli-buffer))
                  (run-hooks 'sql-set-sqli-hook))
                ;; All done.
                (message "Login...done")
                (pop-to-buffer sql-buffer)
                ;; clear password string, just to be extra safe
                (if (> (length sql-password) 0) (clear-string sql-password))
                ;; restore old login credentials
                (setq sql-user old-sql-user
                      sql-password old-sql-password
                      sql-database old-sql-database
                      sql-server old-sql-server
                      sql-port old-sql-port))))))))

;; debuggers: gdb perldb pdb jdb
(require 'bashdb nil t)
(let ((gudlist '("gdb" "pdb" "perldb" "jdb" "bashdb")))
  (loop for name in gudlist
        for func = (intern-soft name)
        if (and (functionp func) (not (assoc name one-key-regs-processes)))
        do (add-to-list 'one-key-regs-processes
                        (list name
                              `(lambda (cmdline)
                                 (funcall ',func cmdline))
                              `(lambda nil
                                 (let* ((file (if (featurep 'ido)
                                                  (ido-read-file-name "Program to be debugged: " nil nil t)
                                                (read-file-name "Program to be debugged: " nil nil t)))
                                        (args (read-string "Program args: "))
                                        (opts (read-string "Debugger options: "))
                                        (cmdline (concat opts ,(if (equal name "perldb") " -d" nil)
                                                         " " file " " args)))
                                   (read-string "Final check: " (concat ,(if (equal name "perldb")
                                                                             "perl" name)
                                                                        " " cmdline))))))))
;; reverse list since earlier ones are probably more frequently used
(setq one-key-regs-processes (reverse one-key-regs-processes))

(if (require 'bookmark+ nil t)
    (progn
      ;; the following global variable and function are used for creating `one-key' bookmarks+ menus
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
        (one-key-menu "filtered-bookmarks" 'one-key-regs-bookmarks-alist))
      ;; now add a register type for creating `one-key' bookmark menus
      (add-to-list 'one-key-regs-custom-register-types
                   '(bookmarks-one-key-menu
                     `(one-key-regs-bookmarks-menu (lambda (bmk)
                                                     (let ((bmktags (bmkp-get-tags bmk)))
                                                       (catch 'bmkp-b-mu-b-t-an
                                                         (dolist (tag (quote ,(bmkp-read-tags-completing)))
                                                           (unless (assoc-default tag (bmkp-get-tags bmk) nil t)
                                                             (throw 'bmkp-b-mu-b-t-an nil)))
                                                         t))))
                     (lambda (reg) (format "Bookmarks tagged %S" (cadadr (cadar (cddadr (cdaddr (caddr reg)))))))))
      ;; replace 'new-bookmark register type with one that uses bookmarks+
      (setq one-key-regs-custom-register-types (assq-delete-all 'new-bookmark one-key-regs-custom-register-types))
      (add-to-list 'one-key-regs-custom-register-types
                   '(new-bookmark
                     `(bookmark-jump
                       ,(let ((name (bmkp-completing-read-lax "Set bookmark " nil nil nil 'bookmark-history)))
                          (bookmark-set name '(4) t) name))
                     (lambda (reg) (format "Bookmark: %s" (caddr reg)))))
      (if (not (assq 'bookmarks-one-key-menu one-key-regs-colours-alist))
          (add-to-list 'one-key-regs-colours-alist '(bookmarks-one-key-menu . "OrangeRed4")))))

;; if run-assoc is loaded add register for opening files with associated program
(if (require 'run-assoc nil t)
    (progn
      (add-to-list 'one-key-regs-custom-register-types
                 '(open-externally
                   `(run-associated-program ,(if (featurep 'ido) (ido-read-file-name "File: " nil nil t)
                                               (read-file-name "File: " nil nil t)))
                   (lambda (reg) (format "Open externally: %s" (file-name-nondirectory (caddr reg))))))
      (if (not (assq 'open-externally one-key-regs-colours-alist))
          (add-to-list 'one-key-regs-colours-alist '(open-externally . "blue")))))
;; if policy-switch is loaded use that for creating/saving window-config registers (so they can be saved to disk)
(if (require 'policy-switch nil t)
    (progn (add-to-list 'one-key-regs-custom-register-types
                        '(window-config
                          `(let ((config ',(policy-switch-window-info)))
                             (dolist (buffer-data config)
                               (setcar (cdr buffer-data) nil))
                             (delete-other-windows)
                             (setq restorable (policy-switch-config-split-windows config))
                             (message (if (= (length restorable) 0)
                                          "All buffers restored"
                                        "%s buffer(s) failed to restore" (length restorable))))
                          (lambda (reg) "Window config")))
           (setq one-key-regs-reserved-register-types
                 (assq-delete-all 'window-config one-key-regs-reserved-register-types))))

(if (require 'one-key-dir nil t)
    (progn
      (add-to-list 'one-key-regs-custom-register-types
                   '(one-key-dir
                     `(let ((dir ,(if (featurep 'ido)
                                      (ido-read-directory-name "Directory: ")
                                    (read-directory-name "Directory: "))))
                        (one-key-dir-visit dir))
                     (lambda (reg) (format "Dir: %s" (cadar (caddr reg))))))
      (if (not (assq 'one-key-dir one-key-regs-colours-alist))
          (add-to-list 'one-key-regs-colours-alist
                       (cons 'one-key-dir (cdr (assq 'file-or-dir one-key-regs-colours-alist)))))))

(if (require 'desktop nil t)
    (progn (setq desktop-globals-to-save (delq 'register-alist desktop-globals-to-save))
           (add-to-list 'one-key-regs-custom-register-types
                        '(desktop
                          `(let ((desktopstr ,(one-key-regs-desktop-save)))
                             (one-key-regs-desktop-read desktopstr))
                          (lambda (reg) (format "Desktop: %s" (current-time-string)))))
           (if (not (assq 'desktop one-key-regs-colours-alist))
               (add-to-list 'one-key-regs-colours-alist '(desktop . "sienna")))
           (defun one-key-regs-desktop-save nil
             "Return string that can be eval'ed to recreate the current desktop."
             (let* ((buf (generate-new-buffer "*one-key-regs-desktop-save*"))
                    (bufstr (with-current-buffer buf
                              (insert "(progn ")
                              (save-excursion (run-hooks 'desktop-save-hook))
                              (mapc (function desktop-outvar) desktop-globals-to-save)
                              (when (memq 'kill-ring desktop-globals-to-save)
                                (insert "(setq kill-ring-yank-pointer (nthcdr "
                                        (int-to-string (- (length kill-ring)
                                                          (length kill-ring-yank-pointer)))
                                        " kill-ring))\n"))
                              (let ((eager desktop-restore-eager))
                                (dolist (l (mapcar 'desktop-buffer-info (buffer-list)))
                                  (let ((base (pop l)))
                                    (when (apply 'desktop-save-buffer-p l)
                                      (insert "("
                                              (if (or (not (integerp eager))
                                                      (if (zerop eager)
                                                          nil
                                                        (setq eager (1- eager))))
                                                  "desktop-create-buffer"
                                                "desktop-append-buffer-args")
                                              " "
                                              desktop-file-version)
                                      (when (and base (not (string= base "")))
                                        (setcar (nthcdr 1 l) base))
                                      (dolist (e l)
                                        (insert "\n  " (desktop-value-to-string e)))
                                      (insert ")\n")))))
                              (insert ")")
                              (buffer-string))))
               (kill-buffer buf)
               bufstr))
           (defun one-key-regs-desktop-read (bufstr)
             "Load a desktop from a buffer string previously saved with `one-key-regs-desktop-save'."
             (let ((desktop-first-buffer nil)
                   (desktop-buffer-ok-count 0)
                   (desktop-buffer-fail-count 0))
               (desktop-lazy-abort)
               (eval (read desktopstr))
               (mapc 'bury-buffer (nreverse (cdr (memq desktop-first-buffer (nreverse (buffer-list))))))
               (switch-to-buffer (car (buffer-list)))
               (run-hooks 'desktop-delay-hook)
               (setq desktop-delay-hook nil)
               (run-hooks 'desktop-after-read-hook)
               (message "Desktop: %d buffer%s restored%s%s."
                        desktop-buffer-ok-count
                        (if (= 1 desktop-buffer-ok-count) "" "s")
                        (if (< 0 desktop-buffer-fail-count)
                            (format ", %d failed to restore" desktop-buffer-fail-count) "")
                        (if desktop-buffer-args-list
                            (format ", %d to restore lazily" (length desktop-buffer-args-list)) ""))))))

(defun one-key-regs-webjump (name)
  "Jumps to the webjump with name NAME.
This is a wrapper to the `webjump' command, which uses NAME as the webjump item
instead of prompting the user for one."
  (interactive)
  (let* ((completion-ignore-case t)
	 (item (assoc-string name webjump-sites t))
	 (name (car item))
	 (expr (cdr item)))
    (browse-url (webjump-url-fix
		 (cond ((not expr) "")
		       ((stringp expr) expr)
		       ((vectorp expr) (webjump-builtin expr name))
		       ((listp expr) (eval expr))
		       ((symbolp expr)
			(if (fboundp expr)
			    (funcall expr name)
			  (error "WebJump URL function \"%s\" undefined"
				 expr)))
		       (t (error "WebJump URL expression for \"%s\" invalid"
				 name)))))))

(if (require 'webjump nil t)
    (progn
      (add-to-list 'one-key-regs-custom-register-types
                   '(webjump
                     `(one-key-regs-webjump
                       ,(if (featurep 'ido)
                            (ido-completing-read "WebJump to site: " webjump-sites nil t)
                          (completing-read "WebJump to site: " webjump-sites nil t)))
                     (lambda (reg) (format "Web: %s" (caddr reg)))))
      (if (not (assq 'webjump one-key-regs-colours-alist))
          (add-to-list 'one-key-regs-colours-alist '(webjump . "magenta1")))))



(provide 'one-key-regs-extras)
;;; one-key-regs-extras.el ends here

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "one-key-regs-extras.el" (buffer-name) (buffer-string) "update")



