;;; oidxt.el --- Regression Tests for org-index.el

;; Copyright (C) 2011-2020 Free Software Foundation, Inc.

;; Author: Marc Ihm <org-index@2484.de>
;; Keywords: outlines, regression-tests, elisp
;; Requires: org, org-index

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
;;  Regression tests for package org-index.el.
;;
;; Setup:
;;
;;  None required
;;
;;
;;
;; Further reading:
;;
;;  See org-index.el, which is tested by this package
;;

;;; Code:

(require 'org-index)
(require 'cl-lib)
(require 'ert)

(defvar oidxt-index-buffer-name "oidxt-index.org")
(defvar oidxt-work-buffer-name "oidxt-work.org")
(defvar oidxt-locations-buffer-name "org-id-locations")
(defvar oidxt-index-file (concat base-dir "/tmp/" oidxt-index-buffer-name))
(defvar oidxt-work-file (concat base-dir "/tmp/" oidxt-work-buffer-name))
(defvar oidxt-locations-file (concat base-dir "/tmp/" oidxt-locations-buffer-name))
(defvar oidxt-vars-to-save '(org-index-id oidx--ref-head oidx--ref-tail oidx--ref-regex oidx--ref-format))
(defvar oidxt-saved-state nil)

(defvar oidxt-id-index "366f022c-2c43-4fce-a269-88f04122a5ca")
(defvar oidxt-id-1 "588bda71-38b7-41a9-90f0-cc9fb39991fa")
(defvar oidxt-id-2 "5a16c863-1f7e-4636-9c47-74e4d49f72df")
(defvar oidxt-id-3 "12ae411f-bdd4-4c92-9e24-75cf7858f586")
(defvar oidxt-id-4 "caac71f6-74fa-4b6a-b732-66c9ceb0c483")

;;
;; All tests
;;

(ert-deftest oidxt-test-aaa-test-test-setup ()
  (oidxt-with-test-setup
    (message "Testing test setup")))


(ert-deftest oidxt-test-create-and-retrieve-yank ()
  (oidxt-with-test-setup
    (oidxt-do "y f o o <return> b a r <return>")
    (oidxt-do "o f o o <return>")
    (should (string= "bar" (current-kill 0)))))


(ert-deftest oidxt-test-respect-sequence-yank ()
  (oidxt-with-test-setup
    (setq org-index-edit-on-yank '(keywords yank))
    (oidxt-do "y f o o <return> b a r <return>")
    (oidxt-do "o f o o <return>")
    (should (string= "bar" (current-kill 0)))))


(ert-deftest oidxt-test-add-node-without-ref-add-ref-later ()
  (oidxt-with-test-setup
    (oidxt-do "a")
    (oidxt-do "i .")
    (should-not (oidx--get-or-set-field 'ref))
    (org-mark-ring-goto)
    (oidxt-do "C-u" "a")
  (forward-char 2)
  (yank)
  (insert " ")
  (beginning-of-line)
  (should (looking-at "* --15-- drei"))))


(ert-deftest oidxt-test-add-and-kill-node ()
  (oidxt-with-test-setup
    (oidxt-do "C-u" "a")
    (oidxt-do "o d r e i")
    (execute-kbd-macro (kbd "C-u 4 <right>"))
    (should (looking-at "--15--"))
    (org-mark-ring-goto)
    (oidxt-do "SPC k i l l <return>")
    (oidxt-do "o d r e i")
    (execute-kbd-macro (kbd "C-u 5 <right>"))
    (should (looking-at "--9--"))))


(ert-deftest oidxt-test-kill-in-occur-result ()
  (oidxt-with-test-setup
    (oidxt-do "a d d <return>")
    (should (looking-at "* ?"))))


(ert-deftest oidxt-test-dispatch ()
  (oidxt-with-test-setup
    (global-set-key (kbd "C-c i") 'org-index)
    (execute-kbd-macro (kbd "C-c i i i"))
    (should (string= (buffer-name) "oidxt-index.org"))
    (global-unset-key (kbd "C-c i"))))


(ert-deftest oidxt-test-short-help ()
  (oidxt-with-test-setup
    (global-set-key (kbd "C-c i") 'org-index)
    (execute-kbd-macro (kbd "C-c i ? h e l p <return>"))
    (global-unset-key (kbd "C-c i"))
    (with-current-buffer "*org-index commands*"
      (goto-char (point-max))
      (should (= (line-number-at-pos) 15)))))


(unless (functionp 'org-duration-from-minutes)
  (defun org-duration-from-minutes (x)))


(ert-deftest oidxt-test-occur-result ()
  (oidxt-with-test-setup
    (oidxt-do "o e i n <backspace> n s <return>")
    (should (string= (buffer-name) "oidxt-work.org"))
    (should (looking-at "* --13--"))))


(ert-deftest oidxt-test-occur-m-backspace ()
  (oidxt-with-test-setup
   (oidxt-do "o e i n M-DEL e i n s <return>")
   (should (string= (buffer-name) "oidxt-work.org"))
   (should (looking-at "* --13--"))))


(ert-deftest oidxt-test-occur-increment-count ()
  (oidxt-with-test-setup
    (oidxt-do "o e i n s <down> +")
    (should (string= (buffer-name) "*org-index-occur*"))
    (should (string= "2" (oidx--get-or-set-field 'count)))))


(ert-deftest oidxt-test-inc-count-on-action ()
  (oidxt-with-test-setup
    (oidxt-do "o - - 1 4 - - <return>")
    (oidxt-do "o - - 1 4 - - <right>")
    (oidxt-do "i .")
    (should (string= "2" (oidx--get-or-set-field 'count)))))


(ert-deftest oidxt-test-mark-ring ()
  (oidxt-with-test-setup
    (oidxt-do "o z w e i <down> <return>")
    (oidxt-do "o e i n s <down> <return>")
    (should (looking-at ".* --13--"))
    (org-mark-ring-goto)
    (should (looking-at ".* --8--"))))


(ert-deftest oidxt-test-migrate-index ()
  (oidxt-with-test-setup
    (oidxt-do "o e i n s <return>")
    (forward-char 2)
    (should (looking-at "--13--"))
    (with-current-buffer "oidxt-index.org"
      (org-entry-delete (point)  "max-ref"))
    (oidxt-do "o e i n s <return>")
    (forward-char 2)
    (should (looking-at "--13--"))
    (oidxt-do "i .")
    (should (string= (org-entry-get (point) "max-ref") "--14--"))))


(ert-deftest oidxt-test-no-id ()
  (oidxt-with-test-setup
    (setq org-index-id nil)
    (condition-case result
        (oidxt-do (format "c r e a t e <return> o i d x t - i n d e x . o r g <return> f o o <return> # 1 # <return> %s"
                          (oidxt-y-or-n-ans nil)))
      (error (should (string-match "^Did not make the id of this new index permanent" (cdr result)))))
    (switch-to-buffer "oidxt-work.org")
    (goto-char (point-max))
    (org-reveal)
    (forward-line -1)
    (forward-char 3)
    (oidxt-do "C-u" "a")
    (yank)
    (forward-line 0)
    (should (looking-at "\\*\\* #2# neun"))))


(ert-deftest oidxt-test-example ()
  (oidxt-with-test-setup
   (oidxt-do (format "SPC e x a m p l e <return> %s e x a m p l e <return> - 1 - <return>" (oidxt-y-or-n-ans t)))
    (with-current-buffer "*org-index-example-index*"
      (should (search-forward "Below you find"))
      (oidx--go-below-hline)
      (should (org-at-table-p)))))


(ert-deftest oidxt-test-node-above-index ()
  (oidxt-with-test-setup
    (pop-to-buffer-same-window "oidxt-index.org")
    (beginning-of-buffer)
    (insert "* foo\n")
    (forward-line -1)
    (oidxt-do "a d d <return>")
    (oidxt-do "i n d e x <return> .")
    (should (string= (oidx--get-or-set-field 'keywords)
                     "foo"))))


(ert-deftest oidxt-test-enter-and-return ()
  (let (initial-point initial-mark)
    (oidxt-with-test-setup
      (oidxt-do "i i M-<")
      (set-mark-command nil)
      (setq initial-point (point))
      (setq initial-mark (mark))
      (oidxt-do "i i")
      (org-mark-ring-goto)
      (should (= initial-point (point)))
      (should (= initial-mark (mark))))))


(ert-deftest oidxt-test-enter ()
  (oidxt-with-test-setup
   (oidxt-do "i i")
   (should (looking-at "--14--"))))


(ert-deftest oidxt-test-goto-index-from-occur ()
  (oidxt-with-test-setup
    (oidxt-do "o e i n s <right> i")
    (org-table-next-field)
    (should (looking-at "--13--"))))


(ert-deftest oidxt-test-find-node-from-index ()
  (oidxt-with-test-setup
    (oidxt-do "i i")
    (forward-line 6)
    (oidxt-do "n")
    (should (looking-at ".* --8--"))))


(ert-deftest oidxt-test-enter-goto-line-of-current-node ()
  (oidxt-with-test-setup
    (previous-line)
    (oidxt-do "i n d e x <return> .")
    (should (string= (oidx--get-or-set-field 'ref) "--2--"))))


(ert-deftest oidxt-test-create-new-ref ()
  (oidxt-with-test-setup
    (oidxt-do "r foo <return> bar <return>")
    (oidxt-do "o b a r <right>")
    (should (string= (oidx--get-or-set-field 'keywords)
                     "bar"))
    (oidxt-do "C-u" "r <return> <return>")
  (should (string= "--16--" (current-kill 0)))))


(ert-deftest oidxt-test-edit-from-index ()
  (oidxt-with-test-setup
    (oidxt-do "i i")
    (oidxt-do "e")
    (execute-kbd-macro (kbd "C-e f o o C-c C-c"))
    (should (string= (buffer-name) "oidxt-index.org"))
    (should (string= (oidx--get-or-set-field 'keywords)
                     "foo"))))


(ert-deftest oidxt-test-edit-from-occur ()
  (oidxt-with-test-setup
    (oidxt-do "o e i n s")
    (oidxt-do "e")
    (execute-kbd-macro (kbd "C-e f o o C-c C-c"))
    (should (string= (buffer-name) oidx--o-buffer-name))
    (should (string= (oidx--get-or-set-field 'keywords)
                     "einsfoo"))
    (should (= (progn (end-of-line) (current-column))
               (progn (end-of-line 2) (current-column))))))


(ert-deftest oidxt-test-edit-yank-from-occur ()
  (oidxt-with-test-setup
    (oidxt-do "o y a n k , e i n s <right>")
    (oidxt-do "e")
    (execute-kbd-macro (kbd "C-e f o o C-c C-c"))
    (should (string= (buffer-name) oidx--o-buffer-name))
    (should (string= (oidx--get-or-set-field 'keywords)
                     "zwei-einsfoo"))))


(ert-deftest oidxt-test-edit-from-node ()
  (oidxt-with-test-setup
   (previous-line)
   (oidxt-do "e")
   (execute-kbd-macro (kbd "C-e f o o C-c C-c"))
   (should (string= (buffer-name) "oidxt-work.org"))))


(ert-deftest oidxt-test-details-from-occur ()
  (oidxt-with-test-setup
   (oidxt-do "o e i n s <down> d")
   (other-window 1)
   (should (string= (buffer-name) oidx--details-buffer-name))
   (should (looking-at "\s+ref: --11--"))))


(ert-deftest oidxt-test-yank ()
  (oidxt-with-test-setup
    (oidxt-do "y q u x <return> f o o b a r")
    (oidxt-do "o q u x <return>")
    (should (string= (current-kill 0)
                     "foobar"))
    (oidxt-do "i l")
    (should (string= (oidx--get-or-set-field 'yank) "foobar"))))


(ert-deftest oidxt-test-delete-yank ()
  (oidxt-with-test-setup
    (oidxt-do "y f o o <return> b a r <return>")
    (oidxt-do "o f o o <right>")
    (oidxt-do "SPC k i l l <return>")
    (oidxt-do "o f o o <return>")
    (should (= oidx--o-lines-collected 0))))


(ert-deftest oidxt-test-sort-index ()
  (oidxt-with-test-setup
    (oidxt-do "o - - 1 4 - - <return>")
    (oidxt-do "SPC s o r t")
    (should (looking-at "--14--"))))


(ert-deftest oidxt-test-maintain-duplicates ()
  (oidxt-with-test-setup
    (oidxt-do "m d u p l i c a t e s <return>")
    (should (string= oidx--message-text
                     "No duplicate references or ids found."))
    (execute-kbd-macro (kbd "C-a C-k C-k C-y C-y"))
    (oidxt-do "m d u p l i c a t e s <return>")
    (should (string= oidx--message-text
                     "Some references or ids are duplicate."))))


(ert-deftest oidxt-test-maintain-statistics ()
  (oidxt-with-test-setup
    (oidxt-do "m s t a t i s t i c s <return>")
    (should (string= oidx--message-text
                     "14 Lines in index table. First reference is --1--, last --14--; 14 of them are used (100 percent)."))))


(ert-deftest oidxt-test-maintain-clean ()
  (oidxt-with-test-setup
    (oidxt-do "m c l e a n <return>")
    (should (string= oidx--message-text
                     "Removed property 'org-index-ref' from 1 lines."))))


(ert-deftest oidxt-test-maintain-verify ()
  (oidxt-with-test-setup
    (oidxt-do "m v e r i f y <return>")
    (should (string= oidx--message-text
                     "All ids of index are valid."))))


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
    (oidxt-do "C-u" "a")
    (forward-char 2)
    (yank)
    (beginning-of-line)
    (should (looking-at "* --15-- drei"))
    (oidxt-do "SPC k i l l <return>")
    (beginning-of-line)
    (should (looking-at "* drei"))))


(ert-deftest oidxt-test-kill-from-node ()
  (oidxt-with-test-setup
    (oidxt-do "C-u" "a")
    (oidxt-do "o - - 1 5 - -")
    (should (= oidx--o-lines-collected 1))
    (org-mark-ring-goto)
    (oidxt-do "SPC k i l l <return>")
    (oidxt-do "o - - 1 5 - -")
    (should (string= (buffer-name) oidx--o-buffer-name))
    (should (= oidx--o-lines-collected 0))))


(ert-deftest oidxt-test-kill-from-index ()
  (oidxt-with-test-setup
    (oidxt-do "C-u" "a")
    (oidxt-do "i .")
    (oidxt-do "SPC k i l l <return>")
    (oidxt-do "o - - 1 5 - -")
    (should (string= (buffer-name) oidx--o-buffer-name))
    (should (= oidx--o-lines-collected 0))))


(ert-deftest oidxt-test-kill-from-occur ()
  (oidxt-with-test-setup
    (oidxt-do "C-u" "a" )
    (oidxt-do "o - - 1 5 - -")
    (oidxt-do "SPC k i l l <return>")
    (oidxt-do "o - - 1 5 - -")
    (should (string= (buffer-name) oidx--o-buffer-name))
    (should (= oidx--o-lines-collected 0))))


(ert-deftest oidxt-test-update-from-within-index ()
  (oidxt-with-test-setup
    (previous-line 2)
    (forward-char 2)
    (insert "foo ")
    (oidxt-do "i .")
    (oidxt-do "a")
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
    (oidxt-do "m u p d a t e <return> y")
    ;; String "foo " should have been transported from headline into index; see buffer
    (should (search-forward "foo vier"))))


(ert-deftest oidxt-test-edit-on-add ()
  (oidxt-with-test-setup
    (setq org-index-edit-on-add nil)
    (oidxt-do "C-u" "a")
    (forward-char 2)
    (yank)
    (beginning-of-line)
    (should (looking-at "* --15-- drei"))
    (oidxt-do "SPC k i l l <return>")
    (setq org-index-edit-on-add '(keywords))
    (oidxt-do "a b a r SPC <return>")
    (oidxt-do "i .")
    (should (string= (oidx--get-or-set-field 'keywords) "dreibar"))))


(ert-deftest oidxt-test-customize ()
  (oidxt-with-test-setup
    (setq org-index-key "")
    (global-unset-key (kbd "C-c i"))
    (customize-option 'org-index-key)
    (execute-kbd-macro (kbd "C-u 8 TAB C-k C - c SPC i <return> C-u 4 <backtab> <return>"))
    (should (string= org-index-key "C-c i"))
    (execute-kbd-macro (kbd "C-c i i i"))
    (should (looking-at "--14--"))
    (setq org-index-key "")
    (global-unset-key (kbd "C-c i"))))


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


(defun oidxt-do (one &optional two)
  (let (keys prefix)
    (if two
	(progn
	  (setq keys two)
	  (setq prefix one))
      (setq keys one))
    (execute-kbd-macro (kbd (concat prefix (if prefix " " "") "M-x o r g - i n d e x <return> " keys)))))


(defun oidxt-get-refs ()
  "Collect refs in buffer"
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


(defun oidxt-setup-test ()
  (interactive)

  (if (boundp 'ert--stats)
      (message (format "Executing test %S" (ert-test-name (ert--stats-current-test ert--current-run-stats)))))

  ;; maybe left from previos tests
  (ignore-errors (kill-buffer "*org-index-occur*"))

  ;; set config vars
  (setq org-index-edit-on-add nil)
  (setq org-agenda-files (list oidxt-index-file oidxt-work-file))

  ;; used within org-index.el
  (setq oidx--o-assert-result t)

  ;; create and load locations file
  (oidxt-create-locations)
  (setq org-id-locations-file oidxt-locations-file)
  (org-id-locations-load)

  ;; create and use index
  (oidxt-create-index)
  (setq org-index-id oidxt-id-index)

  ;; create work buffer
  (oidxt-create-work)

  ;; prepare work buffer
  (switch-to-buffer oidxt-work-buffer-name)
  (org-cycle '(16))
  (delete-other-windows)
  (end-of-buffer)
  (forward-line -2)

  ;; save state
  (unless oidxt-saved-state
    (setq oidxt-saved-state (mapcar (lambda (s) (cons s (symbol-value s))) oidxt-vars-to-save))))


(defun oidxt-teardown-test ()
  (interactive)
  ;; avoid having to save buffers
  (with-current-buffer oidxt-work-buffer-name (set-buffer-modified-p nil))
  (with-current-buffer oidxt-index-buffer-name (set-buffer-modified-p nil))

  ;; restore saved state
  (mapcar (lambda (sv) (set (car sv) (cdr sv))) oidxt-saved-state))


(defun oidxt-y-or-n-ans (bool)
  "Create answer suitable for y-or-n-p; different if noninteractive (batch)"
  (concat (if bool "y" "n") (if noninteractive " <return>" "")))


;;
;; Test data
;;

(defun oidxt-create-index ()
  (setq org-index-occur-columns 8)
  (with-current-buffer (get-buffer-create oidxt-index-buffer-name)
    (setq buffer-file-name oidxt-index-file)
    (oidxt-clear-buffer)
    (org-mode)
    (insert 
     "* oidxt-test-index
  :PROPERTIES:
  :ID:       " oidxt-id-index "
  :max-ref:  --14--
  :END:
       

  |    ref | id                                   | created         | category | level | count | last-accessed         | keywords       | yank | tags |
  |        | <4>                                  |                 |          |       |       |                       |                |      |      |
  |--------+--------------------------------------+-----------------+----------+-------+-------+-----------------------+----------------+------+------|
  | --14-- |                                      | [2013-12-19 Do] |          |       |     1 |                       |                |      |      |
  | --13-- | " oidxt-id-2                       " | [2013-12-19 Do] |          |       |     1 |                       | eins           |      |      |
  | --12-- |                                      | [2013-12-19 Do] |          |       |     1 |                       | vier-zwei      |      |      |
  | --11-- |                                      | [2013-12-19 Do] |          |       |     1 |                       | vier-eins      |      |      |
  | --10-- |                                      | [2013-12-19 Do] |          |       |     1 |                       | vier           |      |      |
  |  --9-- |                                      | [2013-12-19 Do] |          |       |     1 |                       | drei           |      |      |
  |  --8-- | " oidxt-id-1                       " | [2013-12-19 Do] |          |       |     1 |                       | zwei-zwei-eins |      |      |
  |  --7-- |                                      | [2013-12-19 Do] |          |       |     1 |                       | zwei-zwei      |      |      |
  |  --6-- |                                      | [2013-12-19 Do] | yank     |       |     1 |                       | zwei-eins      | six  |      |
  |  --5-- |                                      | [2013-12-19 Do] |          |       |     1 |                       | zwei           |      |      |
  |  --4-- | " oidxt-id-3                       " | [2013-12-19 Do] |          |       |     1 |                       | eins-drei      |      |      |
  |  --3-- |                                      | [2013-12-19 Do] |          |       |     1 | [2013-12-19 Do 10:00] | eins-zwei      |      |      |
  |  --2-- | " oidxt-id-4                       " | [2013-12-19 Do] |          |       |     1 |                       | eins-eins      |      |      |
  |  --1-- | " oidxt-id-index                   " | [2013-12-15 So] |          |       |     1 | [2013-12-15 So 10:00] | This node      |      |      |

")
    (forward-line -1)
    (org-table-align)
    (basic-save-buffer)))


(defun oidxt-create-work ()
  (with-current-buffer (get-buffer-create oidxt-work-buffer-name)
    (setq buffer-file-name oidxt-work-file)
    (oidxt-clear-buffer)
    (org-mode)
    (insert
     "* --1-- eins
* --8-- acht
  :PROPERTIES:
  :ID:       " oidxt-id-1 "
  :END:
* --13--
  :PROPERTIES:
  :ID:       " oidxt-id-2 "
  :END:
* vier --4--
  :PROPERTIES:
  :ID:       " oidxt-id-3 "
  :END:

  Zeile 1

*************** Inline
*************** END

  Zeile 2

* --2-- zwei --2--
  :PROPERTIES:
  :ID:       " oidxt-id-4 "
  :org-index-ref: foo
  :END:
* drei
** neun
")
    (basic-save-buffer)))


(defun oidxt-create-locations ()
  (with-current-buffer (get-buffer-create oidxt-locations-buffer-name)
    (setq buffer-file-name oidxt-locations-file)
    (oidxt-clear-buffer)
    (insert
     "((\"" oidxt-index-file "\" \"" oidxt-id-index "\") (\"" oidxt-work-file "\" \"" oidxt-id-4 "\" \"" oidxt-id-3 "\" \"" oidxt-id-2 "\" \"" oidxt-id-1 "\"))
")))


(defun oidxt-clear-buffer ()
  (setq buffer-save-without-query t)
  (auto-save-mode t) ; disables mode
  (ignore-errors (delete-file buffer-auto-save-file-name))
  (erase-buffer)
  (set-buffer-modified-p nil))


(provide 'oidxt)

;; Local Variables:
;; fill-column: 75
;; comment-column: 50
;; End:

;;; oidxt.el ends here
