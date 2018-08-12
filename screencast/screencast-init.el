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

(global-set-key (kbd "C-c i") 'org-index)
