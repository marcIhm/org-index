(setq load-prefer-newer t)
(setq user-init-file "screencast-init.el")

(setq debug-on-error t)
(setq debug-on-quit t)
(setq org-id-locations-file ".org-id-locations-screencast")
(setq-default mode-line-format (list "      %b (%m)"))
(set-fringe-style 0)

(setq scroll-conservatively 200)
(setq scroll-step 1)

(add-to-list 'load-path "..")
(add-to-list 'load-path "../../org")
(require 'org)
(require 'org-index)

(menu-bar-mode -1)
(tool-bar-mode -1)
(setq-default indent-tabs-mode nil)
(global-font-lock-mode 1)                     ; for all buffers
(toggle-truncate-lines)
