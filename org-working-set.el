;;; org-working-set.el --- Manage a working-set of org-nodes  -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2019 Free Software Foundation, Inc.

;; Author: Marc Ihm <org-index@2484.de>
;; URL: https://github.com/marcIhm/org-index
;; Version: 5.12.1
;; Package-Requires: ((emacs "24.4"))

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
;;

;;; Commentary:

;; Purpose:
;;
;;  Maintain a small and volatile subset of your org-nodes to visit with ease.
;;
;;  The working-set is a small number of nodes, among which you can
;;  switch rapidly; it is expected to change on a daily or even hourly
;;  basis.  E.g. if during a day you work on three projects in short
;;  intervals and have two meetings, you might add those five nodes to
;;  your working set one after another and switch between them as
;;  needed.
;;  
;;  Please note, that org-working-set uses org-id throughout and
;;  therefore adds an id-property to all nodes in the working-set.
;;
;;
;; Setup:
;;
;;  - org-working-set can be installed with package.el
;;  - Invoke `org-working-set'; on first run it will ask for a
;;    node, where it may save the working-set.
;;
;;  - Optionally invoke `M-x org-customize', group 'Org Working-set',
;;    to tune its settings.
;;

;;; Change Log:

;;   Version 0.99
;;
;;   - Moved functions for working set into its own file
;;   - Show breadcrumbs in working-set-menu
;;
;;  See the package org-index for older news

;;; Code:


(defun org-index-working-set (&optional silent)
  ;; Do NOT edit the part of this help-text before version number. It will
  ;; be overwritten with Commentary-section from beginning of this file.
  ;; Editing after version number is fine.
  ;;
  "Central interactive function to manage a working-set of nodes.

The working-set is a small number of nodes, among whom you switch
rapidly; it is expected to change on a daily or even hourly
basis.  E.g. if, during a day, you work on three projects in
short intervals and have two meetings, you might add five nodes
to your working set one after another and switch between them.

This is version 0.99 of org-working-set.el.

The subcommands allow to:
- Modify the list of nodes (e.g. add new nodes)
- Circle quickly through the nodes
- Show a menu buffer with all nodes currently in the working set

This command is available as a subcommand of ‘org-index’,
but may also be bound to its own key-sequence.
Optional argument SILENT does not issue final message."
  (interactive)
  (let ((char-choices (list ?s ?S ?a ?A ?d ?u ?w ?m ?c ?g ? ??))
        id text more-text char prompt ids-up-to-top)

    (oidx--verify-id)
    (setq prompt (format "Please specify action on working-set of %d nodes (s,S,a,A,d,u,m,w,c,space,g or ? for short help) - " (length oidx--ws-ids)))
    (while (or (not (memq char char-choices))
               (= char ??))
      (setq char (read-char-choice prompt char-choices))
      (setq prompt (format "Actions on working-set of %d nodes:  s)et working-set to this node alone,  S)et but do not clock,  a)ppend this node to set,  A)ppend but do not clock,  d)elete this node from list,  u)ndo last modification of working set,  w),m)enu to edit working set (same as 'w'),  c),space) enter working set circle,  g)o to bottom position in current node.  Please choose - " (length oidx--ws-ids))))

    (when (and (memq char (list ?s ?S ?a ?A ?d))
               (not (string= major-mode "org-mode")))
      (error "Current buffer is not in org-mode"))

    (setq text
          (cond

           ((or (eq char ?s)
                (eq char ?S))
            (setq id (org-id-get-create))
            (setq oidx--ws-ids-saved oidx--ws-ids)
            (setq oidx--ws-ids (list id))
            (if (eq char ?S)
                (setq oidx--ws-ids-do-not-clock (list id))
              (setq oidx--ws-id-last-goto id)
              (if org-index-clock-into-working-set (org-with-limited-levels (org-clock-in))))
            (oidx--update-line id t)
            "working-set has been set to current node (1 node)")

           ((or (eq char ?a)
                (eq char ?A))
            (setq id (org-id-get-create))
            (unless (member id oidx--ws-ids)
              ;; remove any children, that are already in working-set
              (setq oidx--ws-ids
                    (delete nil (mapcar (lambda (x)
                                          (if (member id (org-with-point-at (org-id-find x t)
                                                           (oidx--ws-ids-up-to-top)))
                                              (progn
                                                (setq more-text ", removing its children")
                                                nil)
                                            x))
                                        oidx--ws-ids)))
              (setq oidx--ws-ids-saved oidx--ws-ids)
              ;; remove parent, if already in working-set
              (setq ids-up-to-top (oidx--ws-ids-up-to-top))
              (when (seq-intersection ids-up-to-top oidx--ws-ids)
                (setq oidx--ws-ids (seq-difference oidx--ws-ids ids-up-to-top))
                (setq more-text (concat more-text ", replacing its parent")))
              (setq oidx--ws-ids (cons id oidx--ws-ids)))
            (if (eq char ?A)
                (setq oidx--ws-ids-do-not-clock (cons id oidx--ws-ids-do-not-clock))
              (setq oidx--ws-id-last-goto id)
              (if org-index-clock-into-working-set (org-with-limited-levels (org-clock-in))))
            (oidx--update-line id t)
            "current node has been appended to working-set%s (%d node%s)")

           ((eq char ?d)
            (oidx--ws-delete-from))

           ((memq char '(?m ?w))
            (oidx--ws-menu))

           ((memq char '(?c ? ))
            (oidx--ws-circle-start))

           ((eq char ?g)
            (oidx--ws-bottom-of-node)
            "at bottom of node")

           ((eq char ?u)
            (oidx--ws-nodes-restore))))

    (oidx--ws-nodes-persist)
    
    (setq text (format text (or more-text "") (length oidx--ws-ids) (if (cdr oidx--ws-ids) "s" "")))
    (unless silent (message (concat (upcase (substring text 0 1)) (substring text 1))))
    text))


(defun oidx--ws-circle-start ()
  "Go through working-set, one node after the other."
  (unless oidx--ws-ids (error "No nodes in working-set; need to add some first"))

  (setq oidx--ws-short-help-wanted nil)
  (setq oidx--ws-circle-before-marker (point-marker))
  (setq oidx--ws-circle-win-config (current-window-configuration))

  (let ((kmap (make-sparse-keymap)))
    (mapc (lambda (x)
            (define-key kmap (vector x)
              (lambda () (interactive)
                (setq this-command last-command)
                (oidx--ws-message (oidx--ws-circle-continue)))))
          (list ?c ? ))
    (mapc (lambda (x)
            (define-key kmap (kbd x)
              (lambda () (interactive)
                (oidx--ws-message "Circle done")
                (oidx--ws-circle-finished-helper nil))))
          (list "RET" "<return>" "q"))
    (mapc (lambda (x)
            (define-key kmap (kbd x)
              (lambda () (interactive)
                (setq this-command last-command)
                (oidx--ws-message (oidx--ws-circle-continue nil t)))))
          (list "DEL" "<backspace>"))
    (mapc (lambda (x)
            (define-key kmap (kbd x)
              (lambda () (interactive)
                (oidx--ws-message "Switching to menu")
                (oidx--ws-circle-finished-helper t)
                (run-with-timer 0 nil 'oidx--ws-menu))))
          (list "w" "m"))
    (define-key kmap (vector ?h)
      (lambda () (interactive)
        (oidx--ws-head-of-node)
        (oidx--ws-message "On heading of node from working-set")))
    (define-key kmap (vector ?b)
      (lambda () (interactive)
        (oidx--ws-bottom-of-node)
        (oidx--ws-message "At bottom of node from working-set")))
    (define-key kmap (vector ??)
      (lambda () (interactive)
        (setq oidx--ws-short-help-wanted t)
        (message (oidx--ws-circle-continue t))
        (setq oidx--cancel-ws-wait-function nil)
        (setq oidx--ws-short-help-wanted nil)))
    (define-key kmap (vector ?d)
      (lambda () (interactive)
        (setq this-command last-command)
        (oidx--ws-nodes-persist)
        (oidx--ws-message (concat (oidx--ws-delete-from) " "
                                  (oidx--ws-circle-continue)))
        (setq oidx--cancel-ws-wait-function nil)))
    (define-key kmap (kbd "<escape>")
      (lambda () (interactive)
        (if org-index-clock-into-working-set
            (oidx--ws-message "Bailing out of circle, no clock in"))
        (oidx--ws-circle-finished-helper t)))
    (define-key kmap (kbd "C-g")
      (lambda () (interactive)
        (if oidx--ws-circle-before-marker
            (org-goto-marker-or-bmk oidx--ws-circle-before-marker))
        (if oidx--ws-circle-win-config
            (set-window-configuration oidx--ws-circle-win-config))
        (message "Quit")
        (oidx--ws-circle-finished-helper)))
    
    (setq oidx--cancel-ws-wait-function
          (set-transient-map
           kmap t
           ;; this is run (in any case) on leaving the map
           (lambda () (cancel-timer oidx--ws-cancel-timer)
             (message nil)
             ;; Clean up overlay
             (if oidx--ws-overlay (delete-overlay oidx--ws-overlay))
             (setq oidx--ws-overlay nil)
             (if (and org-index-clock-into-working-set
                      (not (member (org-id-get) oidx--ws-ids-do-not-clock))
                      (not oidx--ws-circle-bail-out))
                 (let (keys)
                   ;; save and repeat terminating key, because org-clock-in might read interactively
                   (if (input-pending-p) (setq keys (read-key-sequence nil)))
                   (ignore-errors (org-with-limited-levels (org-clock-in)))
                   (if keys (setq unread-command-events (listify-key-sequence keys)))))
             (if oidx--ws-circle-before-marker (move-marker oidx--ws-circle-before-marker nil))
             (setq oidx--ws-circle-bail-out nil)
             ;; ignore-errors helps during tear-down of some tests
             (ignore-errors (oidx--update-line (org-id-get) t)))))

    ;; first move
    (oidx--ws-message (oidx--ws-circle-continue t))))


(defun oidx--ws-circle-finished-helper (bail-out)
  "Common steps on finishing of working set circle. Argument bail-out, if t, avoids clocking in."
  (if oidx--ws-overlay (delete-overlay oidx--ws-overlay))
  (setq oidx--ws-overlay nil)
  (setq oidx--ws-circle-bail-out bail-out)
  (setq oidx--cancel-ws-wait-function nil))


(defun oidx--ws-circle-continue (&optional stay back)
  "Continue with working set circle after start.
Optional argument STAY prevents changing location."
  (let (last-id following-id previous-id target-id parent-ids head)

    ;; compute target
    (setq last-id (or oidx--ws-id-last-goto
                      (car (last oidx--ws-ids))))
    (setq following-id (car (or (cdr-safe (member last-id
                                                  (append oidx--ws-ids oidx--ws-ids)))
                                oidx--ws-ids)))
    (if back
        (setq previous-id (car (or (cdr-safe (member last-id
                                                     (reverse (append oidx--ws-ids oidx--ws-ids))))
                                   oidx--ws-ids))))
    (setq target-id (if stay last-id (if back previous-id following-id)))
    (setq parent-ids (oidx--ws-ids-up-to-top)) ; remember this before changing location
    
    ;; bail out on inactivity
    (if oidx--ws-cancel-timer (cancel-timer oidx--ws-cancel-timer))
    (setq oidx--ws-cancel-timer
          (run-at-time 30 nil
                       (lambda () (if oidx--cancel-ws-wait-function
                                 (funcall oidx--cancel-ws-wait-function)))))

    (oidx--ws-goto-id target-id)
    (setq oidx--ws-id-last-goto target-id)

    ;; tooltip-overlay to show current heading
    (setq head (org-with-limited-levels (org-get-heading t t t t)))
    (when org-index-show-working-set-overlay
      (if oidx--ws-overlay (delete-overlay oidx--ws-overlay))
      (setq oidx--ws-overlay (make-overlay (point-at-eol) (point-at-eol)))
      (overlay-put oidx--ws-overlay
                   'after-string
                   (propertize
                    (format " %s (%d of %d) "
                            head
                            (1+ (- (length oidx--ws-ids)
                                   (length (member target-id oidx--ws-ids))))
                            (length oidx--ws-ids))
                    'face 'match))
      (overlay-put oidx--ws-overlay 'priority most-positive-fixnum))

    ;; Compose return message:
    (concat
     ;; title of node
     (format "Node %s, " (propertize head 'face 'org-todo))
     ;; explanation
     (format (cond (stay
                    "returning to %slast")
                   ((member target-id parent-ids)
                    "staying below %scurrent")
                   (t
                    (concat "at %s" (if back "previous" "next"))))
             (if org-index-goto-bottom-in-working-set "bottom of " ""))
     ;; count of nodes
     (if (cdr oidx--ws-ids)
         (format " node (out of %d)" (length oidx--ws-ids))
       (format " single node"))
     ;; help text
     (if oidx--ws-short-help-wanted
         "; type 'c' or space to jump to next node in circle; 'h' for heading, 'b' for bottom of node; type 'd' to delete this node from list; 'q',<return> accepts current position and clocks in, <escape> skips clocking in; <backspace> proceeds in reverse order, 'm' or 'w' switch to working set menu, C-g returns to initial position"
       "; type c,space,h,b,d,q,ret,esc,bs,m,w or ? for short help"))))


(defun oidx--ws-menu ()
  "Show menu to let user choose among working-set nodes."

  (setq  oidx--ws-short-help-wanted nil)
  (pop-to-buffer oidx--ws-menu-buffer-name '((display-buffer-at-bottom)))
  (oidx--ws-menu-rebuild t)

  (oidx--ws-menu-install-keyboard-shortcuts)
  "Buffer with nodes of working-set")


(defun oidx--ws-menu-install-keyboard-shortcuts ()
  "Install keyboard shortcuts for working-set menu.
See `oidx--ws-menu-rebuld' for a list of commands."
  (let (keymap)
    (setq keymap (make-sparse-keymap))
    (set-keymap-parent keymap org-mode-map)

    ;; various keys to jump to node
    (mapc (lambda (x) (define-key keymap (kbd x)
                   (lambda () (interactive)
                     (oidx--ws-menu-action x))))
          (list "<return>" "<S-return>" "RET" "<tab>" "<S-tab>" "h" "H" "b" "B"))

    (define-key keymap (kbd "p")
      (lambda () (interactive)
        (save-window-excursion
          (save-excursion
            (oidx--ws-goto-id (oidx--ws-menu-get-id))
            (delete-other-windows)
            (recenter 1)
            (read-char "Peeking into node, any key to return." nil 10)))))

    (define-key keymap (kbd "d")
      (lambda () (interactive)
        (message (oidx--ws-delete-from (oidx--ws-menu-get-id)))
        (oidx--ws-nodes-persist)
        (oidx--ws-menu-rebuild)))

    (mapc (lambda (x) (define-key keymap (kbd x)
                   (lambda () (interactive)
                     (let ((id (oidx--ws-menu-get-id)))
                       (setq oidx--ws-ids-do-not-clock
                             (if (member id oidx--ws-ids-do-not-clock)
                                 (delete id oidx--ws-ids-do-not-clock)
                               (cons id oidx--ws-ids-do-not-clock)))
                       (oidx--ws-nodes-persist)
                       (oidx--ws-menu-rebuild)))))
          (list "c" "~"))

    (define-key keymap (kbd "u")
      (lambda () (interactive)
        (message (oidx--ws-nodes-restore))
        (oidx--ws-nodes-persist)
        (oidx--ws-menu-rebuild t)))

    (define-key keymap (kbd "q")
      (lambda () (interactive)
        (delete-windows-on oidx--ws-menu-buffer-name)
        (kill-buffer oidx--ws-menu-buffer-name)))

    (define-key keymap (kbd "r")
      (lambda () (interactive)
        (oidx--ws-menu-rebuild)))

    (define-key keymap (kbd "?")
      (lambda () (interactive)
        (setq oidx--ws-short-help-wanted (not oidx--ws-short-help-wanted))
        (oidx--ws-menu-rebuild t)))

    (use-local-map keymap)))


(defun oidx--ws-menu-action (key)
  "Perform some actions for working-set menu.
Argument KEY has been pressed to trigger this function."
  (setq key (intern key))
  (let (id)
    (setq id (oidx--ws-menu-get-id))
    (cl-case key
      ((<return> <S-return> RET h b)
       (delete-window)
       (oidx--ws-goto-id id)
       (recenter 1))
      ((<tab> <S-tab> H B)
       (other-window 1)
       (oidx--ws-goto-id id)))
    (if (or (memq key '(b B))
            (and (memq key '(<return> <S-return> RET))
                 org-index-goto-bottom-in-working-set)) (oidx--ws-bottom-of-node))
    (when (and (not (memq key '(<S-return> <S-tab>)))
               (not (member id oidx--ws-ids-do-not-clock)))
      (setq oidx--ws-id-last-goto id)
      (if org-index-clock-into-working-set (org-with-limited-levels (org-clock-in))))))


(defun oidx--ws-menu-get-id ()
  "Extract id from current line in working-set menu."
  (or (get-text-property (point) 'org-index-id)
      (error "This line does not point to a node from working-set")))


(defun oidx--ws-menu-rebuild (&optional resize)
  "Rebuild content of working-set menu-buffer.
Optional argument RESIZE adjusts window size."
  (let (cursor-here lb)
    (with-current-buffer (get-buffer-create oidx--ws-menu-buffer-name)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (propertize (if oidx--ws-short-help-wanted
                              (oidx--wrap "List of working-set nodes. Pressing <return> on a list element jumps to node in other window and deletes this window, <tab> does the same but keeps this window, <S-return> and <S-tab> do not clock do not clock, `h' and `b' jump to bottom of node unconditionally (with capital letter in other windows), `p' peeks into node from current line, `d' deletes node from working-set immediately, `u' undoes last delete, `q' aborts and deletes this buffer, `r' rebuilds its content, `c' or `~' toggles clocking. Markers on nodes are: `*' for last visited and `~' do not clock.")
                            "Press <return>,<S-return>,<tab>,<S-tab>,h,H,b,B,p,d,u,q,r,c,~,* or ? to toggle short help.")
                          'face 'org-agenda-dimmed-todo-face))
      (insert "\n\n")
      (setq cursor-here (point))
      (if oidx--ws-ids
          (mapconcat (lambda (id)
                       (let (head)
                         (save-window-excursion
                           (save-excursion
                             (org-id-goto id)
                             (setq head (concat (substring-no-properties (org-get-heading))
                                                (propertize (concat " / "
                                                                    (org-format-outline-path (reverse (org-get-outline-path)) most-positive-fixnum nil " / "))
                                                            'face 'org-agenda-dimmed-todo-face)))))
                         (let ((prefix1 " ") (prefix2 " "))
                           (if (member id oidx--ws-ids-do-not-clock)
                               (setq prefix2 "~"))
                           (when (eq id oidx--ws-id-last-goto)
                             (setq prefix1 "*"))
                           (insert (format "%s%s %s" prefix1 prefix2 head)))
                         (setq lb (line-beginning-position))
                         (insert "\n")
                         (put-text-property lb (point) 'org-index-id id)))
                     oidx--ws-ids
                     "\n")
        (insert "  No nodes in working-set.\n"))
      (goto-char cursor-here)
      (when resize
        (fit-window-to-buffer (get-buffer-window))
        (enlarge-window 1))
      (setq buffer-read-only t))))


(defun oidx--ws-goto-id (id)
  "Goto node with given ID and unfold."
  (let (marker)
    (unless (setq marker (org-id-find id 'marker))
      (setq oidx--ws-id-last-goto nil)
      (error "Could not find working-set node with id %s" id))
    
    (pop-to-buffer-same-window (marker-buffer marker))
    (goto-char (marker-position marker))
    (oidx--unfold-buffer)
    (move-marker marker nil)
    (when org-index-goto-bottom-in-working-set
      (oidx--ws-bottom-of-node))))


(defun oidx--ws-message (message)
  "Issue given MESSAGE and append string '(again)' if appropriate."
  (let ((again (if (string= message oidx--last-ws-message) " (again)" "")))
    (setq oidx--last-ws-message message)
    (message (concat message again "."))))


(defun oidx--ws-bottom-of-node ()
  "Goto end of current node, ignore inline-tasks but stop at first child."
  (let (level (pos (point)))
    (when (ignore-errors (org-with-limited-levels (org-back-to-heading)))
      (setq level (outline-level))
      (forward-char 1)
      (if (and (org-with-limited-levels (re-search-forward org-outline-regexp-bol nil t))
               (> (outline-level) level))
          (progn        ; landed on child node
            (goto-char (match-beginning 0))
            (forward-line -1))
        (goto-char pos) ; landed on next sibling or end of buffer
        (org-with-limited-levels
         (org-end-of-subtree nil t)
         (when (org-at-heading-p)
           (forward-line -1))))
      (beginning-of-line)
      (org-reveal))
    (recenter -2)))


(defun oidx--ws-head-of-node ()
  "Goto head of current node."
  (org-with-limited-levels (org-back-to-heading))
  (recenter 2))


(defun oidx--ws-nodes-restore (&optional upcase)
  "Restore previously saved working-set.
Optional argument UPCASE modifies the returned message."
  (let (txt)
    (if oidx--ws-ids-saved
        (progn
          (setq txt (format "Discarded current working set of and restored previous set; now %d node%s in working-set" (length oidx--ws-ids-saved) (if (cdr oidx--ws-ids-saved) "s" "")))
          (setq oidx--ws-ids oidx--ws-ids-saved))
      (setq txt "No saved working-set nodes to restore, nothing to do"))
    (if upcase (concat (upcase (substring txt 0 1))
                       (substring txt 1)
                       ".")
      txt)))


(defun oidx--ws-nodes-persist ()
  "Write working-set to property."
  (with-current-buffer oidx--buffer
    (setq oidx--ws-ids-do-not-clock (cl-intersection oidx--ws-ids-do-not-clock oidx--ws-ids))
    (setq oidx--ws-ids (cl-remove-duplicates oidx--ws-ids :test (lambda (x y) (string= x y))))
    (setq oidx--ws-ids-do-not-clock (cl-remove-duplicates oidx--ws-ids-do-not-clock :test (lambda (x y) (string= x y))))
    (org-entry-put oidx--point "working-set-nodes" (mapconcat 'identity oidx--ws-ids " "))
    (org-entry-put oidx--point "working-set-nodes-do-not-clock" (mapconcat 'identity oidx--ws-ids-do-not-clock " "))))


(defun oidx--ws-delete-from (&optional id)
  "Delete current node from working-set.
Optional argument ID gives the node to delete."
  (setq id (or id (org-id-get)))
  (format
   (if (and id (member id oidx--ws-ids))
       (progn
         (if (string= id oidx--ws-id-last-goto) (setq oidx--ws-id-last-goto nil))
         (setq oidx--ws-ids-saved oidx--ws-ids)
         (setq oidx--ws-ids (delete id oidx--ws-ids))
         "Current node has been removed from working-set (%d node%s)")
     "Current node has not been in working-set (%d node%s)")
   (length oidx--ws-ids) (if oidx--ws-ids "s" "")))


(defun oidx--ws-ids-up-to-top ()
  "Get list of all ids from current node up to top level."
  (when (string= major-mode "org-mode")
    (let (ids id pt)
      (save-excursion
        (ignore-errors
          (while (progn (and (setq id (org-id-get))
                             (setq ids (cons id ids)))
                        (setq pt (point))
                        (outline-up-heading 1)
                        (/= pt (point))))))
      ids)))

(provide 'org-working-set)

;; Local Variables:
;; fill-column: 75
;; comment-column: 50
;; End:

;;; org-working-set.el ends here

