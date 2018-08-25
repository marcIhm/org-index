;;; screencast-init.el --- initialize screencast

(add-to-list 'load-path "..")
(add-to-list 'load-path "../../org")

(require 'org)
(require 'org-index)

(setq load-prefer-newer t)
(setq user-init-file "screencast-init.el")
(setq org-id-locations-file ".org-id-locations-screencast")

(setq debug-on-error t)
(setq debug-on-quit t)

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
 '(menu-bar-mode nil)
 '(org-index-id "fd6a5627-2ba5-4d84-833f-804e3437759b")
 '(org-index-key "i"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
