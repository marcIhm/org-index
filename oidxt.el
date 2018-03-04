;;; oidxt.el --- Regression Tests for org-index.el

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
(require 'cl-lib)
(require 'ert)


(defvar oidxt-saved-state nil "Store state of customizable variables")
(defvar oidxt-ert-index-file (concat temporary-file-directory "oidxt-ert-index.org"))
(defvar oidxt-ert-work-file (concat temporary-file-directory "oidxt-ert-work.org"))
(defvar oidxt-work-buffer nil)
(defvar oidxt-index-buffer nil)
(defvar oidxt-saved-id nil)
(defvar oidxt-saved-id-locations nil)
(defvar oidxt-saved-agenda-files nil)
(defvar oidxt-keep-test-state nil)

;;
;; All tests
;;

(ert-deftest oidxt-test-aaa-test-test-setup ()
  (oidxt-with-test-setup
    (oidxt-do "i n d e x <return> SPC")
    (message "Testing test setup")))


(ert-deftest oidxt-test-create-and-retrieve-yank ()
  (oidxt-with-test-setup
    (oidxt-do "y a n k <return> f o o <return> b a r <return>")
    (oidxt-do "o c c u r <return> f o o <return>")
    (should (string= "bar" (current-kill 0)))))


(ert-deftest oidxt-test-respect-sequence-yank ()
  (oidxt-with-test-setup
    (setq org-index-edit-on-yank '(keywords yank))
    (oidxt-do "y a n k <return> f o o <return> b a r <return>")
    (oidxt-do "o c c u r <return> b a r <return>")
    (should (string= "bar" (current-kill 0)))))


(ert-deftest oidxt-test-add-node-without-ref-add-ref-later ()
  (oidxt-with-test-setup
    (oidxt-do "a d d <return>")
    (oidxt-do "i n d e x <return> .")
    (should-not (oidx--get-or-set-field 'ref))
    (org-mark-ring-goto)
    (oidxt-do "a d d <return>" "C-u")
  (forward-char 2)
  (yank)
  (insert " ")
  (beginning-of-line)
  (should (looking-at "* --15-- drei"))))


(ert-deftest oidxt-test-add-and-kill-node ()
  (oidxt-with-test-setup
    (oidxt-do "a d d <return>" "C-e <return> C-u")
    (oidxt-do "o c c u r <return> d r e i")
    (execute-kbd-macro (kbd "<right> <right> <right> <right>"))
    (should (looking-at "--15--"))
    (org-mark-ring-goto)
    (execute-kbd-macro (kbd "C-e <return>"))
    (oidxt-do "k i l l <return>")
    (oidxt-do "o c c u r <return> d r e i")
    (execute-kbd-macro (kbd "<right> <right> <right> <right> <right>"))
    (should (looking-at "--9--"))))


(ert-deftest oidxt-test-kill-in-occur-result ()
  (oidxt-with-test-setup
    (oidxt-do "a d d <return>")
    (should (looking-at "* ?"))))


(ert-deftest oidxt-test-goto-column ()
  (oidxt-with-test-setup
    (oidxt-do "i n d e x <return> .")
    (oidxt-do "c o l u m n <return> k")
    (should (= 8 (org-table-current-column)))))


(ert-deftest oidxt-test-dispatch ()
  (oidxt-with-test-setup
    (global-set-key (kbd "C-c i") 'org-index-dispatch)
    (execute-kbd-macro (kbd "C-c i i <return> t"))
    (should (string= (buffer-name) "oidxt-ert-index.org"))))


(ert-deftest oidxt-test-short-help ()
  (oidxt-with-test-setup
    (global-set-key (kbd "C-c i") 'org-index-dispatch)
    (execute-kbd-macro (kbd "C-c i ? h e l p <return>"))
    (with-current-buffer "*org-index commands*"
      (goto-char (point-max))
      (should (= (line-number-at-pos) 21)))))


(ert-deftest oidxt-test-occur-days ()
  (let (days digits)
    (setq days (- (time-to-days (current-time))
                  (time-to-days (org-read-date nil t "[2013-12-17 Di]" nil))))
    (setq digits (apply 'concat (mapcar (lambda (x) (format "%c " x)) (number-to-string days))))
    (oidxt-with-test-setup
      (oidxt-do  "o c c u r <return>" (concat "C-u " digits))
      (should (string= "[2013-12-19 Do 10:00]" (oidx--get-or-set-field 'last-accessed))))))


(ert-deftest oidxt-test-clock-into-focus ()
  (oidxt-with-test-setup
    (unwind-protect
	(progn
	  (let ((oidx--after-focus-delay 1) (org-index-clock-into-focus nil))
	    (should (not (org-clock-is-active)))
	    (oidxt-do "o c c u r <return> z w e i <down> <return>")
	    (oidxt-do "f o c u s <return> s" "C-u")
	    (sleep-for 2)
	    (should (not (org-clock-is-active)))
	    
	    (setq org-index-clock-into-focus t)
	    (should (not (org-clock-is-active)))
	    (oidxt-do "o c c u r <return> z w e i <down> <return>")
	    (oidxt-do "f o c u s <return> s" "C-u")
	    (sleep-for 2)
	    (should (org-clock-is-active))))
      (org-clock-out))))

(unless (functionp 'org-duration-from-minutes)
  (defun org-duration-from-minutes (x)))


(ert-deftest oidxt-test-occur-result ()
  (oidxt-with-test-setup
    (oidxt-do "o c c u r <return> e i n <backspace> n s <return>")
    (should (string= (buffer-name) "oidxt-ert-work.org"))
    (should (looking-at "* --13--"))
    (should (oidxt-check-buffer "*org-index-occur*" 
                               "e13d924c0b5b443eab1642fbafa0bd0d"
                               "oidxt-test-occur-result"))))


(ert-deftest oidxt-test-occur-increment-count ()
  (oidxt-with-test-setup
    (oidxt-do "o c c u r <return> e i n s <down> SPC")
    (should (string= (buffer-name) "*org-index-occur*"))
    (should (string= "2" (oidx--get-or-set-field 'count)))))


(ert-deftest oidxt-test-mark-ring ()
  (oidxt-with-test-setup
    (oidxt-do "o c c u r <return> z w e i <down> <return>")
    (oidxt-do "o c c u r <return> e i n s <down> <return>")
    (should (looking-at ".* --13--"))
    (org-mark-ring-goto)
    (should (looking-at ".* --8--"))))


(ert-deftest oidxt-test-focus ()
  (oidxt-with-test-setup
    (oidxt-do "o c c u r <return> z w e i <down> <return>")
    (oidxt-do "f o c u s <return> s" "C-u")
    (beginning-of-buffer)
    (oidxt-do "f o c u s <return>")
    (should (looking-at ".* --8--"))))


(ert-deftest oidxt-test-focus-restore ()
  (oidxt-with-test-setup
    (oidxt-do "o c c u r <return> z w e i <down> <return>")
    (oidxt-do "f o c u s <return> s" "C-u")
    (should (= (length oidx--ids-focused-nodes) 1))
    (oidxt-do "f o c u s <return> d" "C-u")
    (should (= (length oidx--ids-focused-nodes) 0))
    (oidxt-do "f o c u s <return> r" "C-u")
    (should (= (length oidx--ids-focused-nodes) 1))))


(ert-deftest oidxt-test-focus-bottom-head ()
  (oidxt-with-test-setup
    (oidxt-do "o c c u r <return> - - 4 - - <return>")
    (oidxt-do "f o c u s <return> s" "C-u")
    (beginning-of-buffer)
    (oidxt-do "f o c u s <return> b")
    (forward-line)
    (should (looking-at ".* --2--"))
    (forward-line -1)
    (oidxt-do "f o c u s <return> h")
    (should (looking-at ".* --4--"))))


(ert-deftest oidxt-test-double-focus ()
  (oidxt-with-test-setup
    (oidxt-do "o c c u r <return> z w e i <down> <return>")
    (oidxt-do "f o c u s <return> s" "C-u")
    (oidxt-do "o c c u r <return> e i n s <down> <return>")
    (oidxt-do "f o c u s <return> a" "C-u")
    (oidxt-do "f o c u s <return>")
    (should (looking-at ".* --8--"))
    (org-index 'focus)
    (should (looking-at ".* --13--"))
    (setq this-command 'org-index)
    (setq last-command 'org-index)
    (org-index 'focus)
    (should (looking-at ".* --8--"))))


(ert-deftest oidxt-test-nested-focus ()
  (oidxt-with-test-setup
    (oidxt-do "o c c u r <return> v i e r <down> <return>")
    (oidxt-do "f o c u s <return> s" "C-u")
    (search-forward "neun")
    (org-reveal)
    (oidxt-do "f o c u s <return> a" "C-u")
    (should (= (length oidx--ids-focused-nodes) 1))))


(ert-deftest oidxt-test-migrate-index ()
  (oidxt-with-test-setup
    (oidxt-do "o c c u r <return> e i n s <return>")
    (forward-char 2)
    (should (looking-at "--13--"))
    (with-current-buffer "oidxt-ert-index.org"
      (org-entry-delete (point)  "max-ref"))
    (oidxt-do "o c c u r <return> e i n s <return>")
    (forward-char 2)
    (should (looking-at "--13--"))
    (oidxt-do "i n d e x <return> SPC")
    (should (string= (org-entry-get (point) "max-ref") "--14--"))))


(ert-deftest oidxt-test-find-ref ()
  (oidxt-with-test-setup
    (oidxt-do "f i n d - r e f <return> 4 <return>")
    ;; remove variable parts from content
    (forward-line 4)
    (forward-char 20) (let ((inhibit-read-only t))
			(kill-line))
    (should (string= (buffer-name) "*Occur*"))
    (should (oidxt-check-buffer "*Occur*"
                               "e5210bd309fc4abf0df550cfd5be7a63"
                               "oidxt-test-find-ref"))))


(ert-deftest oidxt-test-no-id ()
  (oidxt-with-test-setup
    (oidxt-save-and-set-state nil)
    (condition-case result
        (oidxt-do "c r e a t e <return> y o i d x - e r t - w o r k . o r g <return> f o o <return> # 1 # <return> n")
      (error (should (string-match "^Did not make the id of this new index permanent" (second result))))) 
    (forward-line -1)
    (org-reveal)
    (oidxt-do "a d d <return>" "C-u") 
    (forward-char 2)
    (org-reveal)
    (yank)
    (forward-line 0)
    (org-reveal)
    (should (looking-at ".*#2#  neun"))))


(ert-deftest oidxt-test-example ()
  (oidxt-with-test-setup
    (oidxt-do "e x a m p l e <return> y e x a m p l e <return> - 1 - <return>")
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
      (oidx--go-below-hline)
      (org-table-align))
    (should (oidxt-check-buffer "*org-index-example-index*" 
                               "0cd76a72e306ea880f19de339268ede4"
                               "oidxt-test-example"))))


(ert-deftest oidxt-test-node-above-index ()
  (oidxt-with-test-setup
    (pop-to-buffer-same-window "oidxt-ert-index.org")
    (beginning-of-buffer)
    (insert "* foo\n")
    (forward-line -1)
    (oidxt-do "a d d <return>")
    (oidxt-do "i n d e x <return> .")
    (should (string= (oidx--get-or-set-field 'keywords)
                     "foo"))))


(ert-deftest oidxt-test-enter-and-return ()
  (oidxt-with-test-setup
    (oidxt-do "i n d e x <return> SPC M-<")
    (set-mark-command nil)
    (setq initial-point (point))
    (setq initial-mark (mark))
    (oidxt-do "i n d e x <return> SPC")
    (execute-kbd-macro (kbd "M-x o r g - m a r k - r i n g - g o t o <return>"))
    (should (= initial-point (point)))
    (should (= initial-mark (mark)))))


(ert-deftest oidxt-test-goto-index-from-occur ()
  (oidxt-with-test-setup
    (oidxt-do "o c c u r <return> e i n s <S-return>")
    (should (string= "2" (oidx--get-or-set-field 'count)))))


(ert-deftest oidxt-test-find-head-from-index ()
  (oidxt-with-test-setup
    (oidxt-do "i n d e x <return> SPC")
    (forward-line 6)
    (oidxt-do "h e a d <return>")
    (should (looking-at ".* --8--"))))


(ert-deftest oidxt-test-find-head-from-number ()
  (oidxt-with-test-setup
    (oidxt-do "h e a d <return> 8 <return>")
    (should (looking-at ".* --8--"))))


(ert-deftest oidxt-test-enter-goto-reference ()
  (oidxt-with-test-setup
    (oidxt-do "i n d e x <return> 2 <return>")
    (should (looking-at ".* --2--"))))


(ert-deftest oidxt-test-enter-goto-line-of-current-node ()
  (oidxt-with-test-setup
    (previous-line)
    (oidxt-do "i n d e x <return> .")
    (should (string= (oidx--get-or-set-field 'ref) "--2--"))))


(ert-deftest oidxt-test-create-new-ref ()
  (oidxt-with-test-setup
    (oidxt-do "r e f <return> foo <return> bar <return>")
    (oidxt-do "o c c u r <return> b a r <right>")
    (should (string= (oidx--get-or-set-field 'keywords)
                     "bar"))
    (oidxt-do "r e f <return> <return> <return>" "C-u")
  (should (string= "--16--" (current-kill 0)))))


(ert-deftest oidxt-test-create-new-ref-lisp ()
  (oidxt-with-test-setup
    (org-index-new-line 'keywords "foo bar" 'category "baz" 'ref t)
    (oidxt-do "i n d e x <return> 1 5 <return>")
    (should (string= (oidx--get-or-set-field 'keywords)
                     "foo bar"))))


(ert-deftest oidxt-test-edit-from-index ()
  (oidxt-with-test-setup
    (oidxt-do "i n d e x <return> .")
    (oidxt-do "e d i t <return>")
    (execute-kbd-macro (kbd "C-e f o o C-c C-c"))
    (should (string= (buffer-name) "oidxt-ert-index.org"))
    (should (string= (oidx--get-or-set-field 'keywords)
                     "foo"))))


(ert-deftest oidxt-test-edit-from-occur ()
  (oidxt-with-test-setup
    (oidxt-do "o c c u r <return> e i n s")
    (oidxt-do "e d i t <return>")
    (execute-kbd-macro (kbd "C-e f o o C-c C-c"))
    (should (string= (buffer-name) oidx--occur-buffer-name))
    (should (string= (oidx--get-or-set-field 'keywords)
                     "einsfoo"))
    (should (= (progn (end-of-line) (current-column))
                     (progn (end-of-line 2) (current-column))))))


(ert-deftest oidxt-test-edit-from-node ()
  (oidxt-with-test-setup
    (previous-line)
    (oidxt-do "e d i t <return>")
    (execute-kbd-macro (kbd "C-e f o o C-c C-c"))
    (should (string= (buffer-name) "oidxt-ert-work.org"))))


(ert-deftest oidxt-test-yank ()
  (oidxt-with-test-setup
    (oidxt-do "y a n k <return> q u x <return> f o o b a r")
    (oidxt-do "i n d e x <return> .")
    (oidxt-do "c o l u m n <return> k b a z")
    (oidxt-do "o c c u r <return> b a z <return>")
    (should (string= (current-kill 0)
                     "foobar"))
    (oidxt-do "i n d e x <return> <backspace>")
    (should (string= (oidx--get-or-set-field 'yank) "foobar"))))


(ert-deftest oidxt-test-delete-yank ()
  (oidxt-with-test-setup
    (oidxt-do "y a n k <return> f o o <return> b a r <return>")
    (oidxt-do "o c c u r <return> f o o <right>")
    (oidxt-do "k i l l <return>")
    (oidxt-do "o c c u r <return> f o o <return>")
    (should (= oidx--occur-lines-collected 0))))


(ert-deftest oidxt-test-get-line-lisp ()
  (oidxt-with-test-setup
    (should (string= (plist-get (org-index-get-line 'ref "--12--") 'keywords) "vier-zwei"))))


(ert-deftest oidxt-test-sort-index ()
  (oidxt-with-test-setup
    (oidxt-do "s o r t <return> i n d e x <return> r e f <return>")
    (should (looking-at "--14--"))))


(ert-deftest oidxt-test-maintain-duplicates ()
  (oidxt-with-test-setup
    (oidxt-do "m a i n t a i n <return> d u p l i c a t e s <return>")
    (should (string= oidx--message-text
                     "No duplicate references or ids found."))
    (execute-kbd-macro (kbd "C-a C-k C-k C-y C-y"))
    (oidxt-do "m a i n t a i n <return> d u p l i c a t e s <return>")
    (should (string= oidx--message-text
                     "Some references or ids are duplicate."))))


(ert-deftest oidxt-test-maintain-statistics ()
  (oidxt-with-test-setup
    (oidxt-do "m a i n t a i n <return> s t a t i s t i c s <return>")
    (should (string= oidx--message-text
                     "14 Lines in index table. First reference is --1--, last --14--; 14 of them are used (100 percent)."))))


(ert-deftest oidxt-test-maintain-clean ()
  (oidxt-with-test-setup
    (oidxt-do "m a i n t a i n <return> c l e a n <return>")
    (should (string= oidx--message-text
                     "Removed property 'org-index-ref' from 1 lines."))))


(ert-deftest oidxt-test-maintain-verify ()
  (oidxt-with-test-setup
    (oidxt-do "m a i n t a i n <return> v e r i f y <return>")
    (should (string= oidx--message-text
                     "All ids of index are valid."))))


(ert-deftest oidxt-test-sort-buffer ()
  (oidxt-with-test-setup
    (switch-to-buffer (get-buffer-create "*oidxt-scratch*"))
    (erase-buffer)
    (insert "--4-- foo\nbar --2--\n--5-- baz\n")
    (mark-whole-buffer)
    (oidxt-do "s o r t <return> b u f f e r <return> y")
    (should (oidxt-check-buffer "*oidxt-scratch*"
                               "38b8370137849afadb54b553b2090d57"
                               "oidxt-test-sort-buffer"))))


(ert-deftest oidxt-test-highlight-unhighlight ()
  (oidxt-with-test-setup
    (mark-whole-buffer)
    (oidxt-do "h i g h l i g h t <return>")
    (should (string= oidx--message-text "Highlighted references in region."))))


(ert-deftest oidxt-test-unhighlight ()
  (oidxt-with-test-setup
    (mark-whole-buffer)
    (oidxt-do "SPC h i g h l i g h t <return>" "C-u")
  (should (string= oidx--message-text "Removed highlights for references in region."))))


(ert-deftest oidxt-test-add-update ()
  (oidxt-with-test-setup
    (kill-new "38401327")
    (oidxt-do "a d d <return>")
    (should (string= "38401327" (current-kill 0)))
    (end-of-line)
    (insert " and more text")
    (oidxt-do "a d d")
    (oidxt-do "i n d e x <return> .")
    (should (string= (oidx--get-or-set-field 'keywords) "drei and more text"))))


(ert-deftest oidxt-test-add-without-edit ()
  (oidxt-with-test-setup
    (setq org-index-edit-on-add nil)
    (oidxt-do "a d d <return>")
    (should (looking-at "* drei"))
    (should (string= oidx--message-text
                     "Added new index line."))))


(ert-deftest oidxt-test-add-delete-new-reference ()
  (oidxt-with-test-setup
    (oidxt-do "a d d <return>" "C-u")
    (forward-char 2)
    (yank)
    (beginning-of-line)
    (should (looking-at "* --15-- drei"))
    (oidxt-do "k i l l <return>")
    (beginning-of-line)
    (should (looking-at "* drei"))))


(ert-deftest oidxt-test-kill-from-node ()
  (oidxt-with-test-setup
    (oidxt-do "a d d <return>" "C-u")
    (oidxt-do "o c c u r <return> - - 1 5 - -")
    (should (= oidx--occur-lines-collected 1))
    (org-mark-ring-goto)
    (oidxt-do "k i l l <return>")
    (oidxt-do "o c c u r <return> - - 1 5 - -")
    (should (string= (buffer-name) oidx--occur-buffer-name))
    (should (= oidx--occur-lines-collected 0))))


(ert-deftest oidxt-test-kill-from-index ()
  (oidxt-with-test-setup
    (oidxt-do "a d d <return>" "C-u")
    (oidxt-do "i n d e x <return> 1 5")
    (oidxt-do "k i l l <return>")
    (oidxt-do "o c c u r <return> - - 1 5 - -")
    (should (string= (buffer-name) oidx--occur-buffer-name))
    (should (= oidx--occur-lines-collected 0))))


(ert-deftest oidxt-test-kill-from-occur ()
  (oidxt-with-test-setup
    (oidxt-do "a d d <return>" "C-u")
    (oidxt-do "o c c u r <return> - - 1 5 - -")
    (oidxt-do "k i l l <return>")
    (oidxt-do "o c c u r <return> - - 1 5 - -")
    (should (string= (buffer-name) oidx--occur-buffer-name))
    (should (= oidx--occur-lines-collected 0))))


(ert-deftest oidxt-test-update-from-within-index ()
  (oidxt-with-test-setup
    (previous-line 2)
    (forward-char 2)
    (insert "foo ")
    (oidxt-do "i n d e x <return> .")
    (oidxt-do "a d d <return>")
    (should (string= "foo vier --4--" (oidx--get-or-set-field 'keywords)))))


(ert-deftest oidxt-test-enter-lands-on-ref ()
  (oidxt-with-test-setup
    (previous-line 2)
    (forward-char 2)
    (oidxt-do "i n d e x <return> .")
    (should (eq (org-table-current-column) (oidx--column-num 'ref)))))


(ert-deftest oidxt-test-update-all-lines ()
  (oidxt-with-test-setup
    (previous-line 2)
    (forward-char 2)
    (insert "foo ")
    (oidxt-do "m a i n t a i n <return> u p d a t e <return> y") ; "foo " should be transported to index
    (should (oidxt-check-buffer oidxt-index-buffer 
                               "f7313acc202a0500b82684dcf22182ef"
                               "oidxt-test-update-all-lines"))))

(ert-deftest oidxt-test-ping ()
  (oidxt-with-test-setup
    (previous-line 2)
    (oidxt-do "p i n g <return>")
    (should (string= oidx--message-text
		     "'eins-drei' has been accessed 1 times between [2013-12-19 Do] and nil; category is 'nil', reference is '--4--' and ready to yank '--4--'."))))


(ert-deftest oidxt-test-ping-parent ()
  (oidxt-with-test-setup
    (previous-line 2)
    (org-cycle)
    (forward-line 4)
    (insert "** Neu\n")
    (forward-line -1)
    (oidxt-do "p i n g <return>")
    (should (string= oidx--message-text
		     "'eins-drei' (parent node, 1 level up) has been accessed 1 times between [2013-12-19 Do] and nil; category is 'nil', reference is '--4--' and ready to yank '--4--'."))))


(ert-deftest oidxt-test-line-with-prefix-arg ()
  (oidxt-with-test-setup
    (previous-line 2)
    (oidxt-do "p i n g <return>" "C-u 8")
    (should (string= oidx--message-text
		     "'zwei-zwei-eins' has been accessed 1 times between [2013-12-19 Do] and nil; category is 'nil', reference is '--8--' and ready to yank '--8--'."))))

(ert-deftest oidxt-test-sort-by ()
  (oidxt-with-test-setup
    (oidxt-do "o c c u r <return> e i n s <return>")    
    (should (equal (oidxt-get-refs) '(1 2 3 4 5 6 7 8 9 10 11 12 14 13)))

    (setq org-index-sort-by 'last-accessed)
    (oidx--sort-silent)
    (oidx--parse-table) ; to find hline
    (should (equal (oidxt-get-refs) '(2 4 5 6 7 8 9 10 11 12 14 1 3 13)))

    (setq org-index-sort-by 'count)
    (oidx--sort-silent)
    (oidx--parse-table) ; to find hline
    (dotimes (x 5)
      (oidxt-do "o c c u r <return> e i n s - d r e i <return>"))
    (should (equal (oidxt-get-refs) '(1 2 3 5 6 7 8 9 10 11 12 14 13 4)))))


(ert-deftest oidxt-test-edit-on-add ()
  (oidxt-with-test-setup
    (setq org-index-edit-on-add nil)
    (oidxt-do "a d d <return>" "C-u")
    (forward-char 2)
    (yank)
    (beginning-of-line)
    (should (looking-at "* --15-- drei"))
    (oidxt-do "k i l l <return>")
    (setq org-index-edit-on-add '(keywords))
    (oidxt-do "a d d <return> b a r SPC <return>")
    (oidxt-do "i n d e x <return> .")
    (should (string= (oidx--get-or-set-field 'keywords) "dreibar"))))

;;
;; Helper functions
;;

(defmacro oidxt-with-test-setup (&rest body)
  "Execute body within test setup"
  (declare (indent 0) (debug t))
  `(progn
     (oidxt-setup-test)
     (unwind-protect
         (progn ,@body)
       (oidxt-teardown-test))))


(defun oidxt-do (keys &optional prefix)
  (execute-kbd-macro (kbd (concat prefix (if prefix " " "") "M-x o r g - i n d e x <return> " keys))))


(defun oidxt-get-refs ()
  (let (refs ref-field)
    (with-current-buffer oidx--buffer
      (oidx--go-below-hline)
      (while (org-at-table-p)
        (setq ref-field (oidx--get-or-set-field 'ref))
        (string-match oidx--ref-regex ref-field)
        (setq refs (cons
                    (string-to-number (match-string 1 ref-field))
                    refs))
        (forward-line)))
    (pp refs t)
    refs))


(defun oidxt-check-buffer (buffer expected tname)
  (let ((found (oidxt-hash-buffer buffer tname)))
    (if (string= found expected)
        t
      (message "Hash for buffer %s expected %s but found %s." buffer expected found)
      nil)))


(defun oidxt-hash-buffer (buffer tname)
  (let (text rtext hash pbuf (pbufname "oidxt-ert-output"))
    (with-current-buffer buffer
      (setq text (buffer-substring (point-min) (point-max))))
    (with-temp-buffer
      (beginning-of-buffer)
      (insert text)
      (org-mode)
      (goto-char (point-min))
      (while (search-forward-regexp org-ts-regexp-both nil t)
        (backward-delete-char 15)
        (insert "erased"))
      (goto-char (point-min))
      (while (search-forward-regexp  "[a-z0-9]\\{8\\}-[a-z0-9]\\{4\\}-[a-z0-9]\\{4\\}-[a-z0-9]\\{4\\}-[a-z0-9]\\{12\\}" nil t)
        (backward-delete-char 36)
        (insert "erased"))
      (org-map-entries
       (lambda () (if (org-entry-get (point) "ID") (org-entry-put (point) "ID" "erased"))))
      (setq rtext (buffer-substring-no-properties (point-min) (point-max)))
      ;; calculate hash
      (setq hash (secure-hash 'md5 rtext))
      ;; collect content for reference
      (let ((pbuf (get-buffer-create pbufname)))
        (with-current-buffer pbuf (goto-char (point-max)))
        (princ (format "\n\n\nTest %s:\n" tname) pbuf)
        (princ "START OF COMPLETE BUFFER CONTENT\n" pbuf)
        (princ "-=-=-=-=-=-=-=-=-=-=-=-=-\n" pbuf)
        (princ buffer pbuf)
        (princ rtext pbuf)
        (princ "-=-=-=-=-=-=-=-=-=-=-=-=-\n" pbuf)
        (princ (format "END OF COMPLETE BUFFER CONTENT\nHash value is: %s\n\n" hash) pbuf)
        (message "Content and hash in buffer %s" pbufname))
      hash)))


(defun oidxt-hash-this-buffer () 
  (interactive)
  (let ((hash (oidxt-hash-buffer (current-buffer))))
    (message "Ready to yank '%s'" hash)
    (kill-new hash)))


(defun oidxt-setup-test ()
  (interactive)
  (if oidx--sort-timer
      (cancel-timer oidx--sort-timer))
  (if (get-buffer "*org-index-occur*") (kill-buffer "*org-index-occur*"))
  (setq oidx--last-sort-assumed 'mixed)
  (setq oidx--maxrefnum nil)
  ;; remove any left over buffers
  (oidxt-remove-work-buffers)
  ;; create them new
  (switch-to-buffer oidxt-work-buffer)
  (oidxt-create-work-buffer)
  (oidxt-prepare-test-index)
  (setq oidx--last-sort org-index-sort-by)
  (switch-to-buffer oidxt-work-buffer)
  (basic-save-buffer)
  (org-agenda-file-to-front oidxt-ert-work-file)
  (org-cycle '(16))
  (delete-other-windows)
  (end-of-buffer)
  (forward-line -2))


(defun oidxt-teardown-test ()
  (interactive)
  (remove-hook 'before-save-hook 'oidx--sort-silent)
  (if (not oidxt-keep-test-state) (oidxt-restore-saved-state))
  (with-current-buffer oidxt-work-buffer (set-buffer-modified-p nil))
  (with-current-buffer oidxt-index-buffer (set-buffer-modified-p nil))
  (org-remove-file oidxt-ert-work-file)
  (setq oidx--head nil))


(defun oidxt-remove-work-buffers ()
  "Remove any left over work buffers"
  (mapc (lambda (x)
          (let ((b (get-buffer x)))
            (when b
              (with-current-buffer b
                (set-buffer-modified-p nil))
              (kill-buffer b))))
        (list "oidxt-ert-index.org"
              "oidxt-ert-work.org"))
  (setq oidxt-work-buffer nil
        oidxt-index-buffer nil))


(defun oidxt-save-and-set-state (new-id)
  (let (customizable)

    ;; get customizable variables (they have property standard-value)
    (mapatoms (lambda (x) (if (and (string-match "^org-index-.*"
						 (symbol-name x))
				   (custom-variable-p x))
			      (setq customizable (cons x customizable)))))

    ;; save all customizable variables
    (unless oidxt-saved-state
      (setq oidxt-saved-state
            (mapcar (lambda (x)
                      (cons x (symbol-value x)))
                    customizable)))

    ;; set them all to their standard values
    (mapcar (lambda (x)
              (set x (eval (car (get x 'standard-value)))))
            customizable)

    ;; save some standard org-variables
    (unless oidxt-saved-id (setq oidxt-saved-id org-index-id))
    (setq org-index-id new-id)

    (unless oidxt-saved-id-locations (setq oidxt-saved-id-locations org-id-locations))
    (setq org-id-locations nil)

    (unless oidxt-saved-agenda-files org-agenda-files)
    (setq org-agenda-files nil)))


(defun oidxt-restore-saved-state ()
  (if oidxt-saved-state
      (mapc (lambda (x) (set (car x) (cdr x))) oidxt-saved-state)
    (error "No saved state to restore"))

  (when oidxt-saved-id
    (setq org-index-id oidxt-saved-id)
    (setq oidxt-saved-id nil))
  
  (when oidxt-saved-id-locations
    (setq org-id-locations oidxt-saved-id-locations)
    (setq oidxt-saved-id-locations nil))

  (when oidxt-saved-agenda-files
    (setq org-agenda-files oidxt-saved-agenda-files)
    (setq oidxt-saved-agenda-files)))


;;
;; Test data
;;

(defun oidxt-prepare-test-index ()
  (let ((test-id "1f44f43c-1a37-4d55-oidxt-test-index"))
    (oidxt-save-and-set-state test-id)
    (remove-hook 'before-save-hook 'oidx--sort-silent)
    (org-id-add-location test-id oidxt-ert-index-file)
    (unless oidxt-index-buffer
      (setq oidxt-index-buffer (find-file-noselect oidxt-ert-index-file)))
    (with-current-buffer oidxt-index-buffer
      (setq buffer-save-without-query t)
      (auto-save-mode t) ; actually disables
      (if (file-exists-p buffer-auto-save-file-name)
          (delete-file buffer-auto-save-file-name))
      (erase-buffer)
      (org-mode)
      (insert 

       "* oidxt-test-index
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
  |  --3-- |                                      | [2013-12-19 Do] |          |       |     1 | [2013-12-19 Do 10:00]              | eins-zwei      |      |      |
  |  --2-- | caac71f6-74fa-4b6a-b732-66c9ceb0c483 | [2013-12-19 Do] |          |       |     1 |               | eins-eins      |      |      |
  |  --1-- | " test-id "                          | [2013-12-15 So] |          |       |     1 | [2013-12-15 So 10:00] | This node      |      |      |

")
      (forward-line -1)
      (basic-save-buffer)
      (org-id-update-id-locations (list oidxt-ert-work-file) t)
      (puthash test-id oidxt-ert-index-file org-id-locations)
      (setq oidx--head nil)
      (org-table-align))))


(defun oidxt-create-work-buffer ()
  (unless oidxt-work-buffer
    (setq oidxt-work-buffer (find-file-noselect oidxt-ert-work-file)))
  (with-current-buffer oidxt-work-buffer
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

  Zeile 1

*************** Inline
*************** END

  Zeile 2

* --2-- zwei --2--
  :PROPERTIES:
  :ID:       caac71f6-74fa-4b6a-b732-66c9ceb0c483
  :org-index-ref: foo
  :END:
* drei
** neun
")
    (org-mode)
    oidxt-work-buffer))


(provide 'oidxt)

;; Local Variables:
;; fill-column: 75
;; comment-column: 50
;; End:

;;; oidxt.el ends here
