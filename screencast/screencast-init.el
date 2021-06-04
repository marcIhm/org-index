;;; screencast-init.el --- initialize screencast

(add-to-list 'load-path "..")
(add-to-list 'load-path "../../org")

(require 'org)
(require 'org-index)

(setq load-prefer-newer t)
(setq user-init-file "screencast-init.el")
(setq org-id-locations-file ".org-id-locations-screencast")

;(setq debug-on-error t)
;(setq debug-on-quit t)

(setq-default mode-line-format (list "      %b (%m)"))
(setq scroll-conservatively 200)
(setq scroll-step 1)

(menu-bar-mode -1)
(setq-default indent-tabs-mode nil)
(global-font-lock-mode 1)
(toggle-truncate-lines)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(diff-switches "-u")
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(org-index-id "1667e430-d529-4f24-8cdf-74e5c4ef7b5f")
 '(org-index-key "i")
 '(package-selected-packages '(dash s org-id-cleanup ivy helm))
 '(safe-local-variable-values
   '((eval when
           (and
            (buffer-file-name)
            (not
             (file-directory-p
              (buffer-file-name)))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (featurep 'package-build)
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require 'package-build)))
           (unless
               (derived-mode-p 'emacs-lisp-mode)
             (emacs-lisp-mode))
           (package-build-minor-mode)
           (setq-local flycheck-checkers nil)
           (set
            (make-local-variable 'package-build-working-dir)
            (expand-file-name "../working/"))
           (set
            (make-local-variable 'package-build-archive-dir)
            (expand-file-name "../packages/"))
           (set
            (make-local-variable 'package-build-recipes-dir)
            default-directory))))
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(isearch ((t (:background "green" :foreground "cyan1"))))
 '(mode-line ((t (:background "black" :foreground "green" :inverse-video t :weight extra-light)))))
