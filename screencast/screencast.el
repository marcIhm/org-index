;;; screencast.el --- helper functions for the screencast of org-index

;;
;;  Please beware: This file redefines some common functions of emacs itself !
;;  Therefore, emacs will no longer work as expected, once, this file is evaled.
;;

(defvar sleepscale 2)
(defvar nosleep nil)

(define-key minibuffer-local-completion-map (kbd "<f6>") 'wait-and-see)
(define-key minibuffer-local-map (kbd "<f6>") 'wait-and-see)
(define-key query-replace-map (kbd "<f6>") 'wait-and-see)
(define-key multi-query-replace-map (kbd "<f6>") 'wait-and-see)
(define-key org-mode-map (kbd "<f6>") 'wait-and-see)

;;
;; Override some internal functions
;;

(defun y-or-n-p (prompt)
  (if (string= "y" (org-completing-read (concat prompt " (y or n) ") (list "y" "n") nil t nil nil "y"))
      t
    nil))


(defun read-key-sequence (prompt &rest ignored)
  "Modified version of read-key-sequence, recurring to read-from-minibuffer."
  (let (char)
    (setq char (read-from-minibuffer (or prompt "")))
    (if (string= char " ")
        " "
      (kbd char))))


(unless (symbol-function 'recenter-orig)
  (fset 'recenter-orig (symbol-function 'recenter)))
(defun recenter (&optional arg))

;;
;; Functions to play a screencast
;;

(defun wait-and-see (arg)
  "Redisplay and wait"
  (interactive "P")
  (unless nosleep
    (with-timeout ((* 0.1
                      (if (listp arg)
                          0.5
                        (prefix-numeric-value arg)) sleepscale))
      (kbd-macro-query t))))


(defun my-sleep (sec)
  (force-window-update)
  (redisplay t)
  (unless nosleep
    (sleep-for (* sec sleepscale))))


(defun prepare-for-screencast ()
  "Set up index-options as used for screencast"
  (interactive)
  (setq org-index-id nil)
  (setq org-id-track-globally t)
  (setq org-index-id-sort-by 'count)
  (setq org-index-edit-on-add nil)
  (setq oidx--recording-screencast t)
  (global-unset-key (kbd "C-c i"))
  (setq org-index-key nil)
  (when (not (and (= (frame-width) 108)
		  (= (frame-height) 38)))
    (error "Wrong frame size, must be 108 x 38, not %d x %d" (frame-width) (frame-height)))

  (ignore-errors
    (with-current-buffer "demo.org"
      (setq buffer-file-name nil)
      (kill-buffer)))
  (find-file "~/org-index/screencast/demo.org")
  (with-current-buffer "demo.org"
    (setq buffer-file-name nil)
    (erase-buffer)))


(defun put-advice (pom property value)
  (if (string= property "ID")
      (save-excursion
	(org-back-to-heading)
	(forward-line)
	(org-cycle))))


(defun play-screencast ()
  "Play Screencast."
  (interactive)
  (prepare-for-screencast)
  (setq sleepscale 1.0)
  (setq enable-recursive-minibuffers t)
  (setq redisplay-dont-pause t)
  (setq redisplay-preemption-period 0)
  (let ((frombuf (get-buffer "screencast.org"))
        (tobuf (get-buffer "demo.org"))
	(inhibit-quit t)
	atmax char as-string
        last-as-string within at-period to-point to-point-stored recenter-pt recenter-long)

    (with-current-buffer frombuf
      (goto-char 0))

    (with-current-buffer tobuf
      (goto-char 0)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq-local cursor-type 'box))

    (pop-to-buffer-same-window tobuf)
    (org-mode)
    (delete-other-windows)

    (while (not atmax)
      (setq to-point (point))
      (when quit-flag
	(setq quit-flag nil)
	(if (y-or-n-p "Terminate execution ? ") (keyboard-quit)))
      (with-current-buffer frombuf
        (setq at-period (and (looking-at "\\. ")
                             (not (string= last-as-string "."))))
        (if (or (looking-at "{\\([^ }]*?\\) \\(.*?\\)}")
                (looking-at "{\\([^ }]*?\\)}"))
            (let ((cmd (match-string 1))
                  (txt (match-string 2))
                  (total (length (match-string 0))))
              (cond
               ((string= (substring cmd 0 1) "#"))
               ((string= cmd "mark")
                (setq to-point-stored to-point))
               ((string= cmd "return")
                (with-current-buffer tobuf
                  (if to-point-stored (goto-char to-point-stored)))
                (setq to-point-stored nil))
               ((string= cmd "sleep")
                (my-sleep (string-to-number txt)))
               ((string= cmd "pause")
                (read-string "Paused, press RET to continue ... "))
               ((string= cmd "nosleep")
                (setq nosleep t))
               ((string= cmd "dosleep")
                (setq nosleep nil))
	       ((string= cmd "search")
                (with-current-buffer tobuf
                  (beginning-of-line)
                  (while (not (string= (buffer-substring (point) (+ (point) (length txt))) txt))
                    (my-sleep 0.1)
                    (forward-line)
                    (while (invisible-p (point))
                      (forward-line)))))
               ((string= cmd "sleepscale")
                (setq sleepscale (string-to-number txt)))
               ((string= cmd "recenter")
		(let ((sleepscale 2))
                (with-current-buffer "demo.org"
                  (select-window (get-buffer-window))
		  (setq recenter-long (< (string-to-number txt) 0))
		  (setq recenter-pt (point))
		  (insert (if recenter-long " (just let me recenter ... " " (recenter)"))
		  (my-sleep (* sleepscale (if recenter-long 0.2 0.1)))
                  (recenter-orig (abs (string-to-number txt)))
		  (if recenter-long (insert "done)"))
		  (my-sleep (* sleepscale (if recenter-long 0.2 0.1)))
		  (while (> (point) recenter-pt)
		    (delete-char -1)
		    (my-sleep (* sleepscale 0.002))))))
	       ((string= cmd "version")
		(insert org-index-version))
               ((string= cmd "start")
                (setq within t))
               ((string= cmd "kbd")
                (setq txt (replace-regexp-in-string
                           "\\(~[0-9]+~\\)"
                           (lambda (x)
                             (concat
                              " C-u "
                              (apply 'concat
                                     (cdr (butlast (mapcar (lambda (x) (concat (char-to-string x) " ")) x))))
                              "<f6> "))
                           txt))
                
                (setq txt (replace-regexp-in-string "~" " <f6> " txt))
                (setq txt (replace-regexp-in-string "," " C-u <f6> " txt))
                (setq txt (replace-regexp-in-string "\\(\\\\[0-9]\\)" (lambda (x) (substring x 1)) txt))
                (setq txt (org-trim txt))
                (with-current-buffer tobuf
		  (save-excursion
                    (execute-kbd-macro (kbd txt)))
		  (let ((inhibit-message t))
		    (message "kbd %s" txt))
                  (goto-char to-point)))
               (t
                (error "Unkown command: %s" cmd)))
              (forward-char total))
          (setq char (char-after))
          (setq atmax (>= (point) (point-max)))
          (unless atmax (forward-char))))
      (when char
        (if within (insert char))
        (setq last-as-string as-string)
        (setq as-string (char-to-string char)))
      (setq char nil)
      (pop-to-buffer-same-window tobuf)
      (force-window-update)
      (redisplay t)
      (if within
          (my-sleep
           (+ 0.02
              (cond 
               (at-period 0.06)
               ((string= as-string "!") 0.2)
               ((string= as-string ",") 0.03)
               ((string= as-string ";") 0.04)
               ((string= as-string ":") 0.05)
               (t 0))))))))
