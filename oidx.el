;;; oidx.el --- Regression Tests for org-index.el

;; Copyright (C) 2011-2017 Free Software Foundation, Inc.

;; Author: Marc Ihm <org-index@2484.de>
;; Keywords: outlines, regression-tests, elisp
;; Requires: org, org-index
;; Version: 1.5.0

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Purpose:
;;
;;  Run regression tests on package org-index.el during development cycle.
;;
;; Setup:
;;
;;  None required
;;
;;
;;
;; Further reading:
;;
;;  See org-index.el, which is testet by this package
;;

;;; Code:

(require 'org-index)
(require 'ert)
(require 'cl)


(defvar oidx-saved-state nil "Store state of customizable variables")
(defvar oidx-ert-index-file (concat temporary-file-directory "oidx-ert-index.org"))
(defvar oidx-ert-work-file (concat temporary-file-directory "oidx-ert-work.org"))
(defvar oidx-work-buffer nil)
(defvar oidx-index-buffer nil)
(defvar oidx-saved-id nil)
(defvar oidx-saved-id-locations nil)
(defvar oidx-saved-agenda-files nil)

;;
;; All tests
;;

(ert-deftest oidx-test-aaa-test-test-setup ()
  (oidx-with-test-setup
    (oidx-do "i n d e x <return> SPC")
    (message "Testing test setup")))


(ert-deftest oidx-test-create-and-retrieve-yank ()
  (oidx-with-test-setup
    (oidx-do "y a n k <return> f o o <return> b a r <return>")
    (oidx-do "o c c u r <return> f o o <return>")
    (should (string= "bar" (current-kill 0)))))


(ert-deftest oidx-test-respect-sequence-yank ()
  (oidx-with-test-setup
    (setq org-index-edit-on-yank '(keywords yank))
    (oidx-do "y a n k <return> f o o <return> b a r <return>")
    (oidx-do "o c c u r <return> b a r <return>")
    (should (string= "bar" (current-kill 0)))))


(ert-deftest oidx-test-add-node-without-ref-add-ref-later ()
  (oidx-with-test-setup
    (oidx-do "a d d <return>")
    (oidx-do "i n d e x <return> .")
    (should-not (org-index--get-or-set-field 'ref))
    (org-mark-ring-goto)
    (execute-kbd-macro (kbd "C-u M-x o r g - i n d e x <return> a d d <return>"))
    (forward-char 2)
    (yank)
    (insert " ")
    (beginning-of-line)
    (should (looking-at "* --15-- drei"))))


(ert-deftest oidx-test-add-and-kill-node ()
  (oidx-with-test-setup
    (execute-kbd-macro (kbd "C-e <return> C-u M-x o r g - i n d e x <return> a d d <return>")) 
    (oidx-do "o c c u r <return> d r e i")
    (execute-kbd-macro (kbd "<right> <right> <right> <right>"))
    (should (looking-at "--15--"))
    (org-mark-ring-goto)
    (execute-kbd-macro (kbd "C-e <return>")) 
    (oidx-do "k i l l <return>")
    (oidx-do "o c c u r <return> d r e i")
    (execute-kbd-macro (kbd "<right> <right> <right> <right> <right>"))
    (should (looking-at "--9--"))))


(ert-deftest oidx-test-kill-in-occur-result ()
  (oidx-with-test-setup
    (oidx-do "a d d <return>")
    (should (looking-at "* ?"))))


(ert-deftest oidx-test-goto-column ()
  (oidx-with-test-setup
    (oidx-do "i n d e x <return> .")
    (oidx-do "c o l u m n <return> k")
    (should (= 8 (org-table-current-column)))))


(ert-deftest oidx-test-dispatch ()
  (oidx-with-test-setup
    (global-set-key (kbd "C-c i") 'org-index-dispatch)
    (execute-kbd-macro (kbd "C-c i i <return> t"))
    (should (string= (buffer-name) "oidx-ert-index.org"))))


(ert-deftest oidx-test-short-help ()
  (oidx-with-test-setup
    (global-set-key (kbd "C-c i") 'org-index-dispatch)
    (execute-kbd-macro (kbd "C-c i ? h e l p <return>"))
    (with-current-buffer "*org-index commands*"
      (goto-char (point-max))
      (should (= (line-number-at-pos) 21)))))


(ert-deftest oidx-test-occur-result ()
  (oidx-with-test-setup
    (oidx-do "o c c u r <return> e i n <backspace> n s <return>")
    (should (string= (buffer-name) "oidx-ert-work.org"))
    (should (looking-at "* --13--"))
    (should (oidx-check-buffer "*org-index-occur*" 
                               "22b80bbff1f3cae7e1e51b4f0a556c13"))))


(ert-deftest oidx-test-occur-increment-count ()
  (oidx-with-test-setup
    (oidx-do "o c c u r <return> e i n s <down> SPC")
    (should (string= (buffer-name) "*org-index-occur*"))
    (should (string= "2" (org-index--get-or-set-field 'count)))))


(ert-deftest oidx-test-mark-ring ()
  (oidx-with-test-setup
    (oidx-do "o c c u r <return> z w e i <down> <return>")
    (oidx-do "o c c u r <return> e i n s <return>")
    (should (looking-at ".* --13--"))
    (org-mark-ring-goto)
    (should (looking-at ".* --8--"))))


(ert-deftest oidx-test-focus ()
  (oidx-with-test-setup
    (oidx-do "o c c u r <return> z w e i <down> <return>")
    (oidx-do "s e t - f o c u s <return>")
    (beginning-of-buffer)
    (oidx-do "f o c u s <return>")
    (should (looking-at ".* --8--"))))


(ert-deftest oidx-test-migrate-index ()
  (oidx-with-test-setup
    (oidx-do "o c c u r <return> e i n s <return>")
    (forward-char 2)
    (should (looking-at "--13--"))
    (with-current-buffer "oidx-ert-index.org"
      (org-entry-delete (point)  "max-ref"))
    (oidx-do "o c c u r <return> e i n s <return>")
    (forward-char 2)
    (should (looking-at "--13--"))
    (oidx-do "i n d e x <return> SPC")
    (should (string= (org-entry-get (point) "max-ref") "--14--"))))


(ert-deftest oidx-test-find-ref ()
  (oidx-with-test-setup
    (oidx-do "f i n d - r e f <return> 4 <return>")
    ;; remove variable parts from content
    (forward-line 4)
    (forward-char 20) (let ((inhibit-read-only t))
      (kill-line))
    (should (string= (buffer-name) "*Occur*"))
    (should (oidx-check-buffer "*Occur*"
                               "e5210bd309fc4abf0df550cfd5be7a63"))))


(ert-deftest oidx-test-no-id ()
  (oidx-with-test-setup
    (oidx-save-and-set-state nil)
    (condition-case result
        (oidx-do "c r e a t e <return> y o i d x - e r t - w o r k . o r g <return> f o o <return> # 1 # <return> n")
      (error (should (string-match "^Did not make the id of this new index permanent" (second result))))) 
    (forward-line -1)
    (execute-kbd-macro (kbd "C-u M-x o r g - i n d e x <return> a d d <return>")) 
    (forward-char 2)
    (yank)
    (forward-line 0)
    (should (looking-at ".*#2# drei"))))


(ert-deftest oidx-test-example ()
  (oidx-with-test-setup
    (oidx-do "e x a m p l e <return> y e x a m p l e <return> - 1 - <return>")
    (with-current-buffer "*org-index-example-index*"
      ;; replace own id in index
      (mapc
       (lambda (x)
         (beginning-of-buffer)
         (while (search-forward x nil t)
           (backward-delete-char (length x))
           (insert "replaced")))
       (list (org-entry-get (point) "ID") 
             (with-temp-buffer (org-insert-time-stamp nil nil t))))
      (org-index--go-below-hline)
      (org-table-align))
    (should (oidx-check-buffer "*org-index-example-index*" 
                               "0cd76a72e306ea880f19de339268ede4"))))


(ert-deftest oidx-test-node-above-index ()
  (oidx-with-test-setup
    (pop-to-buffer-same-window "oidx-ert-index.org")
    (beginning-of-buffer)
    (insert "* foo\n")
    (forward-line -1)
    (oidx-do "a d d <return>")
    (oidx-do "i n d e x <return> .")
    (should (string= (org-index--get-or-set-field 'keywords)
                     "foo"))))


(ert-deftest oidx-test-enter-and-return ()
  (oidx-with-test-setup
    (oidx-do "i n d e x <return> SPC M-<")
    (set-mark-command nil)
    (setq initial-point (point))
    (setq initial-mark (mark))
    (oidx-do "i n d e x <return> SPC")
    (execute-kbd-macro (kbd "M-x o r g - m a r k - r i n g - g o t o <return>"))
    (should (= initial-point (point)))
    (should (= initial-mark (mark))))) 


(ert-deftest oidx-test-goto-index-from-occur ()
  (oidx-with-test-setup
    (oidx-do "o c c u r <return> e i n s <S-return>")
    (should (string= "2" (org-index--get-or-set-field 'count))))) 


(ert-deftest oidx-test-find-head-from-index ()
  (oidx-with-test-setup
    (oidx-do "i n d e x <return> SPC")
    (forward-line 6)
    (oidx-do "h e a d <return>")
    (should (looking-at ".* --8--"))))


(ert-deftest oidx-test-find-head-from-number ()
  (oidx-with-test-setup
    (oidx-do "h e a d <return> 8 <return>")
    (should (looking-at ".* --8--"))))


(ert-deftest oidx-test-enter-goto-reference ()
  (oidx-with-test-setup
    (oidx-do "i n d e x <return> 2 <return>")
    (should (looking-at ".* --2--"))))


(ert-deftest oidx-test-enter-goto-line-of-current-node ()
  (oidx-with-test-setup
    (previous-line)
    (oidx-do "i n d e x <return> .")
    (should (string= (org-index--get-or-set-field 'ref) "--2--"))))


(ert-deftest oidx-test-create-new-ref ()
  (oidx-with-test-setup
    (oidx-do "r e f <return> foo <return> bar <return>")
    (oidx-do "o c c u r <return> b a r <right>")
    (should (string= (org-index--get-or-set-field 'keywords)
                     "bar"))
    (execute-kbd-macro (kbd "C-u M-x o r g - i n d e x <return> r e f <return> <return> <return>"))
    (should (string= "--16--" (current-kill 0)))))


(ert-deftest oidx-test-create-new-ref-lisp ()
  (oidx-with-test-setup
    (org-index-new-line 'keywords "foo bar" 'category "baz" 'ref t)
    (oidx-do "i n d e x <return> 1 5 <return>")
    (should (string= (org-index--get-or-set-field 'keywords)
                     "foo bar"))))


(ert-deftest oidx-test-edit-from-index ()
  (oidx-with-test-setup
    (oidx-do "i n d e x <return> .")
    (oidx-do "e d i t <return>")
    (execute-kbd-macro (kbd "C-e f o o C-c C-c"))
    (should (string= (buffer-name) "oidx-ert-index.org"))
    (should (string= (org-index--get-or-set-field 'keywords)
                     "foo"))))


(ert-deftest oidx-test-edit-from-occur ()
  (oidx-with-test-setup
    (oidx-do "o c c u r <return> e i n s")
    (oidx-do "e d i t <return>")
    (execute-kbd-macro (kbd "C-e f o o C-c C-c"))
    (should (string= (buffer-name) org-index--occur-buffer-name))
    (should (string= (org-index--get-or-set-field 'keywords)
                     "einsfoo"))))


(ert-deftest oidx-test-edit-from-node ()
  (oidx-with-test-setup
    (previous-line)
    (oidx-do "e d i t <return>")
    (execute-kbd-macro (kbd "C-e f o o C-c C-c"))
    (should (string= (buffer-name) "oidx-ert-work.org"))))


(ert-deftest oidx-test-yank ()
  (oidx-with-test-setup
    (oidx-do "y a n k <return> q u x <return> f o o b a r")
    (oidx-do "i n d e x <return> .")
    (oidx-do "c o l u m n <return> k b a z")
    (oidx-do "o c c u r <return> b a z <return>")
    (should (string= (current-kill 0)
                     "foobar"))
    (oidx-do "i n d e x <return> <backspace>")
    (should (string= (org-index--get-or-set-field 'yank) "foobar"))))


(ert-deftest oidx-test-delete-yank ()
  (oidx-with-test-setup
    (oidx-do "y a n k <return> f o o <return> b a r <return>")
    (oidx-do "o c c u r <return> f o o <return>")
    (oidx-do "k i l l <return>")
    (oidx-do "o c c u r <return> f o o <return>")
    (should (= org-index--occur-lines-collected 0))))


(ert-deftest oidx-test-get-line-lisp ()
  (oidx-with-test-setup
    (should (string= (plist-get (org-index-get-line 'ref "--12--") 'keywords) "vier-zwei"))))


(ert-deftest oidx-test-sort-index ()
  (oidx-with-test-setup
    (oidx-do "s o r t <return> i n d e x <return> r e f <return>")
    (should (looking-at "--14--"))))


(ert-deftest oidx-test-maintain-duplicates ()
  (oidx-with-test-setup
    (oidx-do "m a i n t a i n <return> d u p l i c a t e s <return>")
    (should (string= org-index--message-text
                     "No duplicate references or ids found."))
    (execute-kbd-macro (kbd "C-a C-k C-k C-y C-y"))
    (oidx-do "m a i n t a i n <return> d u p l i c a t e s <return>")
    (should (string= org-index--message-text
                     "Some references or ids are duplicates."))))


(ert-deftest oidx-test-maintain-statistics ()
  (oidx-with-test-setup
    (oidx-do "m a i n t a i n <return> s t a t i s t i c s <return>")
    (should (string= org-index--message-text
                     "14 Lines in index table. First reference is --1--, last --14--; 14 of them are used (100 percent)."))))


(ert-deftest oidx-test-maintain-clean ()
  (oidx-with-test-setup
    (oidx-do "m a i n t a i n <return> c l e a n <return>")
    (should (string= org-index--message-text
                     "Removed property 'org-index-ref' from 1 lines."))))


(ert-deftest oidx-test-maintain-check ()
  (oidx-with-test-setup
    (oidx-do "m a i n t a i n <return> c h e c k <return>")
    (should (string= org-index--message-text
                     "No problems found."))))


(ert-deftest oidx-test-sort-buffer ()
  (oidx-with-test-setup
    (switch-to-buffer (get-buffer-create "*org-index-scratch*"))
    (erase-buffer)
    (insert "--4-- foo\nbar --2--\n--5-- baz\n")
    (mark-whole-buffer)
    (oidx-do "s o r t <return> b u f f e r <return> y")
    (should (oidx-check-buffer "*org-index-scratch*"
                               "38b8370137849afadb54b553b2090d57"))))


(ert-deftest oidx-test-highlight ()
  (oidx-with-test-setup
    (mark-whole-buffer)
    (oidx-do "h i g h l i g h t <return>")
    (should (string= org-index--message-text "Highlighted references in region."))))


(ert-deftest oidx-test-unhighlight ()
  (oidx-with-test-setup
    (global-set-key (kbd "C-c i") 'org-index-dispatch)
    (mark-whole-buffer)
    (execute-kbd-macro (kbd "C-u C-c i SPC h i g h l i g h t <return> "))
    (should (string= org-index--message-text "Removed highlights for references in region."))))


(ert-deftest oidx-test-add-update ()
  (oidx-with-test-setup
    (kill-new "38401327")
    (oidx-do "a d d <return>")
    (should (string= "38401327" (current-kill 0)))
    (end-of-line)
    (insert " and more text")
    (oidx-do "a d d")
    (oidx-do "i n d e x <return> .")
    (should (string= (org-index--get-or-set-field 'keywords) "drei and more text"))))


(ert-deftest oidx-test-add-without-edit ()
  (oidx-with-test-setup
    (setq org-index-edit-on-add nil)
    (oidx-do "a d d <return>")
    (should (looking-at "* drei"))
    (should (string= org-index--message-text
                     "Added new index line."))))


(ert-deftest oidx-test-add-delete-new-reference ()
  (oidx-with-test-setup
    (execute-kbd-macro (kbd "C-u M-x o r g - i n d e x <return> a d d <return>"))
    (forward-char 2)
    (yank)
    (beginning-of-line)
    (should (looking-at "* --15-- drei"))
    (oidx-do "k i l l <return>")
    (beginning-of-line)
    (should (looking-at "* drei"))))


(ert-deftest oidx-test-kill-from-node ()
  (oidx-with-test-setup
    (execute-kbd-macro (kbd "C-u M-x o r g - i n d e x <return> a d d <return>"))
    (oidx-do "o c c u r <return> - - 1 5 - -")
    (should (= org-index--occur-lines-collected 1))
    (org-mark-ring-goto)
    (oidx-do "k i l l <return>")
    (oidx-do "o c c u r <return> - - 1 5 - -")
    (should (string= (buffer-name) org-index--occur-buffer-name))
    (should (= org-index--occur-lines-collected 0))))


(ert-deftest oidx-test-kill-from-index ()
  (oidx-with-test-setup
    (execute-kbd-macro (kbd "C-u M-x o r g - i n d e x <return> a d d <return>"))
    (oidx-do "i n d e x <return> 1 5")
    (oidx-do "k i l l <return>")
    (oidx-do "o c c u r <return> - - 1 5 - -")
    (should (string= (buffer-name) org-index--occur-buffer-name))
    (should (= org-index--occur-lines-collected 0))))


(ert-deftest oidx-test-kill-from-occur ()
  (oidx-with-test-setup
    (execute-kbd-macro (kbd "C-u M-x o r g - i n d e x <return> a d d <return>"))
    (oidx-do "o c c u r <return> - - 1 5 - -")
    (oidx-do "k i l l <return>")
    (oidx-do "o c c u r <return> - - 1 5 - -")
    (should (string= (buffer-name) org-index--occur-buffer-name))
    (should (= org-index--occur-lines-collected 0))))


(ert-deftest oidx-test-update-from-within-index ()
  (oidx-with-test-setup
    (previous-line 2)
    (forward-char 2)
    (insert "foo ")
    (oidx-do "i n d e x <return> .")
    (oidx-do "a d d <return>")
    (should (string= "foo vier --4--" (org-index--get-or-set-field 'keywords)))))


(ert-deftest oidx-test-enter-lands-on-ref ()
  (oidx-with-test-setup
    (previous-line 2)
    (forward-char 2)
    (oidx-do "i n d e x <return> .")
    (should (eq (org-table-current-column) (org-index--column-num 'ref)))))


(ert-deftest oidx-test-update-all-lines ()
  (oidx-with-test-setup
    (previous-line 2)
    (forward-char 2)
    (insert "foo ")
    (oidx-do "m a i n t a i n <return> u p d a t e <return> y") ; "foo " should be transported to index
    (should (oidx-check-buffer oidx-index-buffer 
                               "1ec880fb4296d3ecb9688ea29c1ea752"))))

(ert-deftest oidx-test-ping ()
  (oidx-with-test-setup
    (previous-line 2)
    (oidx-do "p i n g <return>")
    (should (string= org-index--message-text
                     "'--4--' has been accessed 1 times between [2013-12-19 Do] and nil; category is 'nil', keywords are 'eins-drei' and ready to yank '--4--'."))))


(ert-deftest oidx-test-ping-parent ()
  (oidx-with-test-setup
    (previous-line 2)
    (org-cycle)
    (forward-line 4)
    (insert "** Neu\n")
    (forward-line -1)
    (oidx-do "p i n g <return>")
    (should (string= org-index--message-text
                     "'--4--' (parent node, 1 level up) has been accessed 1 times between [2013-12-19 Do] and nil; category is 'nil', keywords are 'eins-drei' and ready to yank '--4--'."))))


(ert-deftest oidx-test-line-with-prefix-arg ()
  (oidx-with-test-setup
    (previous-line 2)
    (execute-kbd-macro (kbd "C-u 8 M-x o r g - i n d e x <return> p i n g <return>"))
    (should (string= org-index--message-text
                     "'--8--' has been accessed 1 times between [2013-12-19 Do] and nil; category is 'nil', keywords are 'zwei-zwei-eins' and ready to yank '--8--'."))))

(ert-deftest oidx-test-sort-by ()
  (oidx-with-test-setup
    (oidx-do "o c c u r <return> e i n s <return>")    
    (should (equal (oidx-get-refs) '(1 2 3 4 5 6 7 8 9 10 11 12 13 14)))

    (setq org-index-sort-by 'last-accessed)
    (org-index--sort-silent)
    (org-index--parse-table) ; to find hline
    (should (equal (oidx-get-refs) '(1 2 3 4 5 6 7 8 9 10 11 12 14 13)))

    (setq org-index-sort-by 'count)
    (org-index--sort-silent)
    (org-index--parse-table) ; to find hline
    (dotimes (x 5)
      (oidx-do "o c c u r <return> e i n s - d r e i <return>"))
    (should (equal (oidx-get-refs) '(1 2 3 5 6 7 8 9 10 11 12 14 13 4)))))


(ert-deftest oidx-test-edit-on-add ()
  (oidx-with-test-setup
    (setq org-index-edit-on-add nil)
    (execute-kbd-macro (kbd "C-u M-x o r g - i n d e x <return> a d d <return>"))
    (forward-char 2)
    (yank)
    (beginning-of-line)
    (should (looking-at "* --15-- drei"))
    (oidx-do "k i l l <return>")
    (setq org-index-edit-on-add '(keywords))
    (oidx-do "a d d <return> b a r SPC <return>")
    (oidx-do "i n d e x <return> .")
    (should (string= (org-index--get-or-set-field 'keywords) "dreibar"))))

;;
;; Helper functions
;;

(defmacro oidx-with-test-setup (&rest body)
  "Execute body within test setup"
  (declare (indent 0) (debug t))
  `(progn
     (oidx-setup-test)
     (switch-to-buffer oidx-work-buffer)
     (unwind-protect
         (progn ,@body)
       (oidx-teardown-test))))


(defun oidx-do (keys)
  (execute-kbd-macro (kbd (concat "M-x o r g - i n d e x <return> " keys))))


(defun oidx-get-refs ()
    (org-index--on nil nil
      (let (refs ref-field)
        (while (org-at-table-p)
          (setq ref-field (org-index--get-or-set-field 'ref))
          (string-match org-index--ref-regex ref-field)
          (setq refs (cons
                      (string-to-number (match-string 1 ref-field))
                      refs))
          (forward-line))
        (pp refs t)
        refs)))


(defun oidx-check-buffer (buffer expected)
  (let ((found (oidx-hash-buffer buffer)))
    (if (string= found expected)
        t
      (message "Hash for buffer %s expected %s but found %s." buffer expected found)
      nil)))


(defun oidx-hash-buffer (buffer)
  (let (text)
    (with-current-buffer buffer
      (setq text (buffer-substring (point-min) (point-max))))
    (with-temp-buffer
      (beginning-of-buffer)
      (insert text)
      (goto-char (point-min))
      (org-mode)
      (while (search-forward-regexp org-ts-regexp-both nil t)
        (backward-delete-char 15)
        (insert "erased"))
      (org-map-entries
       (lambda () (if (org-entry-get (point) "ID") (org-entry-put (point) "ID" "erased"))))
      (message "--- Start of complete Buffer content %s ---\n%s\n--- End of complete buffer content"
               buffer (buffer-substring-no-properties (point-min) (point-max)))
      ;; calculate hash
      (secure-hash 
       'md5 (buffer-substring-no-properties (point-min) (point-max))))))


(defun oidx-hash-this-buffer () 
  (interactive)
  (let ((hash (oidx-hash-buffer (current-buffer))))
    (message "Ready to yank '%s'" hash)
    (kill-new hash)))


(defun oidx-setup-test ()
  (interactive)
  (if org-index--sort-timer
      (cancel-timer org-index--sort-timer))
  ;; remove any left over buffers
  (oidx-remove-work-buffers)
  ;; create them new
  (with-current-buffer (oidx-create-work-buffer)
    (oidx-prepare-test-index)
    (setq org-index--last-sort org-index-sort-by)
    (org-agenda-file-to-front oidx-ert-work-file)
    (switch-to-buffer oidx-work-buffer)
;;    (basic-save-buffer)
;;    (org-id-update-id-locations (list oidx-ert-work-file) t)
    (delete-other-windows)
    (org-back-to-heading)
    (beginning-of-line)))

  
(defun oidx-teardown-test ()
  (interactive)
  (remove-hook 'before-save-hook 'org-index--sort-silent)
  (progn (oidx-restore-saved-state)
         (with-current-buffer oidx-work-buffer (set-buffer-modified-p nil))
         (with-current-buffer oidx-index-buffer (set-buffer-modified-p nil)))
  (org-remove-file oidx-ert-work-file)
  (setq org-index--head nil))


(defun oidx-remove-work-buffers ()
  "Remove any left over work buffers"
  (mapc (lambda (x)
          (let ((b (get-buffer x)))
            (when b
              (with-current-buffer b
                (set-buffer-modified-p nil))
              (kill-buffer b))))
        (list "oidx-ert-index.org"
              "oidx-ert-work.org"))
  (setq oidx-work-buffer nil
        oidx-index-buffer nil))


(defun oidx-save-and-set-state (new-id)
  (let (customizable)

    ;; get customizable variables (they have property standard-value)
    (mapatoms (lambda (x) (if (and (string-match "^org-index-.*"
                                            (symbol-name x))
                              (custom-variable-p x))
                         (setq customizable (cons x customizable)))))

    ;; save all customizable variables
    (unless oidx-saved-state
      (setq oidx-saved-state
            (mapcar (lambda (x)
                      (cons x (symbol-value x)))
                    customizable)))

    ;; set them all to their standard values
    (mapcar (lambda (x)
              (set x (eval (car (get x 'standard-value)))))
            customizable)

    ;; save some standard org-variables
    (unless oidx-saved-id (setq oidx-saved-id org-index-id))
    (setq org-index-id new-id)

    (unless oidx-saved-id-locations (setq oidx-saved-id-locations org-id-locations))
    (setq org-id-locations nil)

    (unless oidx-saved-agenda-files org-agenda-files)
    (setq org-agenda-files nil)))


(defun oidx-restore-saved-state ()
  (if oidx-saved-state
      (mapc (lambda (x) (set (car x) (cdr x))) oidx-saved-state)
      (setq org-index-id oidx-saved-id)
      (error "No saved state to restore"))

  (when oidx-saved-id
    (setq org-index-id oidx-saved-id)
    (setq oidx-saved-id nil))
  
  (when oidx-saved-id-locations
    (setq org-id-locations oidx-saved-id-locations)
    (setq oidx-saved-id-locations nil))

  (when oidx-saved-agenda-files
    (setq org-agenda-files oidx-saved-agenda-files)
    (setq oidx-saved-agenda-files)))


;;
;; Test data
;;

(defun oidx-prepare-test-index ()
  (let ((test-id "1f44f43c-1a37-4d55-oidx-test-index"))
    (oidx-save-and-set-state test-id)
    (remove-hook 'before-save-hook 'org-index--sort-silent)
    (org-id-add-location test-id oidx-ert-index-file)
    (unless oidx-index-buffer
      (setq oidx-index-buffer (find-file-noselect oidx-ert-index-file)))
    (with-current-buffer oidx-index-buffer
      (setq buffer-save-without-query t)
      (auto-save-mode t) ; actually disables
      (if (file-exists-p buffer-auto-save-file-name)
          (delete-file buffer-auto-save-file-name))
      (erase-buffer)
      (org-mode)
      (insert 

       "* oidx-test-index
  :PROPERTIES:
  :ID:       " test-id "
  :max-ref:  --14--
  :END:
       

  |    ref | id                                   | created         | category | level | count | last-accessed | keywords       | yank | tags |
  |        | <4>                                  |                 |          |       |       |               |                |      |      |
  |--------+--------------------------------------+-----------------+----------+-------+-------+---------------+----------------+------+------|
  | --14-- |                                      | [2013-12-19 Do] |          |       |     1 |               |                |      |      |
  | --13-- | 5a16c863-1f7e-4636-9c47-74e4d49f72df | [2013-12-19 Do] |          |       |     1 |               | eins           |      |      |
  | --12-- |                                      | [2013-12-19 Do] |          |       |     1 |               | vier-zwei      |      |      |
  | --11-- |                                      | [2013-12-19 Do] |          |       |     1 |               | vier-eins      |      |      |
  | --10-- |                                      | [2013-12-19 Do] |          |       |     1 |               | vier           |      |      |
  |  --9-- |                                      | [2013-12-19 Do] |          |       |     1 |               | drei           |      |      |
  |  --8-- | 588bda71-38b7-41a9-90f0-cc9fb39991fa | [2013-12-19 Do] |          |       |     1 |               | zwei-zwei-eins |      |      |
  |  --7-- |                                      | [2013-12-19 Do] |          |       |     1 |               | zwei-zwei      |      |      |
  |  --6-- |                                      | [2013-12-19 Do] |          |       |     1 |               | zwei-eins      |      |      |
  |  --5-- |                                      | [2013-12-19 Do] |          |       |     1 |               | zwei           |      |      |
  |  --4-- | 12ae411f-bdd4-4c92-9e24-75cf7858f586 | [2013-12-19 Do] |          |       |     1 |               | eins-drei      |      |      |
  |  --3-- |                                      | [2013-12-19 Do] |          |       |     1 |               | eins-zwei      |      |      |
  |  --2-- | caac71f6-74fa-4b6a-b732-66c9ceb0c483 | [2013-12-19 Do] |          |       |     1 |               | eins-eins      |      |      |
  |  --1-- | " test-id "                          | [2013-12-15 So] |          |       |     1 |               | This node      |      |      |

")
      (forward-line -1)
;;      (basic-save-buffer)
      ;;      (org-id-update-id-locations (list oidx-ert-work-file) t)
      (setq org-index--head nil)
      (org-table-align))))


(defun oidx-create-work-buffer ()
  (unless oidx-work-buffer
    (setq oidx-work-buffer (find-file-noselect oidx-ert-work-file)))
  (with-current-buffer oidx-work-buffer
    (setq buffer-save-without-query t)
    (auto-save-mode t) ; actually disables
    (if (file-exists-p buffer-auto-save-file-name)
        (delete-file buffer-auto-save-file-name))
    (erase-buffer)
    (insert "* --1-- eins
* --8-- acht
  :PROPERTIES:
  :ID:       588bda71-38b7-41a9-90f0-cc9fb39991fa
  :END:
* --13--
  :PROPERTIES:
  :ID:       5a16c863-1f7e-4636-9c47-74e4d49f72df
  :END:
* vier --4--
  :PROPERTIES:
  :ID:       12ae411f-bdd4-4c92-9e24-75cf7858f586
  :END:
* --2-- zwei --2--
  :PROPERTIES:
  :ID:       caac71f6-74fa-4b6a-b732-66c9ceb0c483
  :org-index-ref: foo
  :END:
* drei
")
    (org-mode)
    oidx-work-buffer))


(provide 'oidx)

;; Local Variables:
;; fill-column: 75
;; comment-column: 50
;; End:

;;; oidx.el ends here
