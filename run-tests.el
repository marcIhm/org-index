(setq load-prefer-newer t)
(add-to-list 'load-path "c:/org-mode/lisp")
(add-to-list 'load-path "c:/org-mode/contrib/lisp" t)
(add-to-list 'load-path "c:/org-index")

(require 'org)
(require 'org-index)
(require 'oidx)

(setq custom-theme-directory "c:/org/themes")
(load-theme 'madhat2r t)

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

