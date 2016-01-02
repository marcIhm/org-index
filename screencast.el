
(defvar sleepscale nil)
(defvar nosleep nil)
(defvar last-raw-keys nil)

(define-key minibuffer-local-completion-map (kbd "<f6>") 'wait-and-see)
(define-key minibuffer-local-map (kbd "<f6>") 'wait-and-see)
(define-key query-replace-map (kbd "<f6>") 'wait-and-see)
(define-key multi-query-replace-map (kbd "<f6>") 'wait-and-see)
(define-key org-mode-map (kbd "<f6>") 'wait-and-see)
(define-key global-map (kbd "<f7>") (lambda () (interactive)))
(define-key global-map (kbd "<f8>") (lambda () (interactive)))


(defun y-or-n-p (prompt)
  (if (string= "y" (org-completing-read (concat prompt " (y or n) ") (list "y" "n") nil t nil nil "y"))
      t
    nil))


(defun this-single-command-raw-keys ()
  last-raw-keys)


(defun read-key-sequence (prompt &rest ignored)
  (let (char)
    (setq char (read-from-minibuffer (substring prompt 0 -1)))
    (if (string= char "<down>")
        (setq last-raw-keys [down])
      (if (string= char "<return>")
           (setq last-raw-keys "")
        (setq last-raw-keys char)))))


(unless (symbol-function 'recenter-orig)
  (fset 'recenter-orig (symbol-function 'recenter)))
(defun recenter (&optional arg))


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


(defun play-screencast ()
  "Play Screencast."
  (interactive)
  (setq sleepscale 1.0)
  (setq org-index-id nil)
  (setq enable-recursive-minibuffers t)
  (setq org-id-track-globally t)
  (setq org-index-id-sort-by 'count)
  (setq redisplay-dont-pause t)
  (setq redisplay-preemption-period 0)
  (let ((frombuf (get-buffer "screencast.org"))
        (tobuf (get-buffer-create "demo.org"))
        atmax char as-string last-as-string within at-period to-point to-point-stored)

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
                (recenter-orig (string-to-number txt)))
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
                (save-excursion
                  (execute-kbd-macro (kbd txt)))
                (with-current-buffer tobuf
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
