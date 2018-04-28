(setq load-prefer-newer t)
(add-to-list 'load-path "c:/org-mode/lisp")
(add-to-list 'load-path "c:/org/custom")
(add-to-list 'load-path "c:/org-index")
(add-to-list 'load-path "c:/org-index/test")

(setq package-user-dir "c:/org/custom/elpa")
(package-initialize)

(require 'org)
(require 'org-index)
(require 'oidxt)
(require 'paredit)
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1) (hs-minor-mode)))

(setq oidxt-keep-test-state t)

(setq org-id-locations-file "~\\.emacs.d\\.org-id-locations-for-test")
(setq custom-theme-directory "c:/org/custom/themes")

(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode nil)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(set-variable 'make-backup-files nil)
(setq ring-bell-function 'ignore)
(global-font-lock-mode 1)
(toggle-truncate-lines)
(setq initial-frame-alist '((width . 122) (height . 42) (top . 20) (left . 50) (name . "emacs")))

