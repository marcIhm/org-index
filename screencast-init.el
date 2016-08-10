(setq load-prefer-newer t)
(setq user-init-file "c:/org/index/screencast-init.el")

(setq debug-on-error t)
(setq debug-on-quit t)
(setq org-id-locations-file "c:/org/index/.org-id-locations-screencast")
(setq-default mode-line-format (list "      %b (%m)"))
(set-fringe-style 0)

(add-to-list 'load-path "c:/org/custom")
(add-to-list 'load-path "c:/git/org-mode/lisp")
(add-to-list 'load-path "c:/git/org-mode/contrib/lisp" t)
(add-to-list 'load-path "c:/org/index")
(add-to-list 'exec-path "c:/cygwin/bin/")

(setq scroll-conservatively 200)
(setq scroll-step 1)

(require 'org-index)
(org-index-default-keybindings)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(canlock-password "55ad2244abadc206ebe62f3137efe042c61fbe92")
 '(column-number-mode t)
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("d99e1c14cfd8054f7875bcece388f06ce58d48d71e06051c71df41efbf159667" "f37d09076188b2e8d2a6847931deec17f640853aedd8ea4ef3ac57db01335008" "f32576813ce2b945f0f6b892f0467acc8372080133d74571922b7c0a87255a8d" "c06241fdef16c336d0cc3a96cf5de5725fad678e9313513e586f0a243289ada5" "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "7a9f392481b6e2fb027ab9d8053ab36c0f23bf5cc1271206982339370d894c74" "65ae93029a583d69a3781b26044601e85e2d32be8f525988e196ba2cb644ce6a" "7ed6913f96c43796aa524e9ae506b0a3a50bfca061eed73b66766d14adfa86d1" "5b3bd478f014d1ff16e1f8ee6e13329c274dd33721f14459d0d2d8f6d93f629d" default)))
 '(delete-selection-mode nil)
 '(fci-rule-color "#383838")
 '(inhibit-startup-screen t)
 '(mark-even-if-inactive t)
 '(org-index-copy-heading-to-keyword t)
 '(org-index-edit-on-add nil)
 '(org-index-edit-on-yank (quote (yank keywords)))
 '(org-index-id "494f510e-d9c4-4113-b62c-1685b7a9f25f")
 '(org-index-sort-by (quote mixed))
 '(org-tag-faces
   (quote
    (("PROJ" :background "indianred3" :foreground "cornsilk2" :weight bold))))
 '(org-todo-keyword-faces
   (quote
    (("DBUG" :background "gold" :foreground "indianred3" :weight bold)
     ("LEAK" :background "gold" :foreground "indianred3" :weight bold)
     ("SEGF" :background "gold" :foreground "indianred3" :weight bold)
     ("CNCL" :background "snow3" :foreground "black" :weight bold))))
 '(scroll-bar-mode nil)
 '(transient-mark-mode 1)
 '(vc-annotate-background "#3b3b3b")
 '(vc-annotate-color-map
   (quote
    ((20 . "#dd5542")
     (40 . "#CC5542")
     (60 . "#fb8512")
     (80 . "#baba36")
     (100 . "#bdbc61")
     (120 . "#7d7c61")
     (140 . "#6abd50")
     (160 . "#6aaf50")
     (180 . "#6aa350")
     (200 . "#6a9550")
     (220 . "#6a8550")
     (240 . "#6a7550")
     (260 . "#9b55c3")
     (280 . "#6CA0A3")
     (300 . "#528fd1")
     (320 . "#5180b3")
     (340 . "#6380b3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(menu-bar-mode -1)
(tool-bar-mode -1)
(setq-default indent-tabs-mode nil)
(global-font-lock-mode 1)                     ; for all buffers
(toggle-truncate-lines)

; turn off the 3d formatting of the mode-line.
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

;; paredit mode
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1) (hs-minor-mode)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1) (hs-minor-mode)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1) (hs-minor-mode)))

(require 'ido)
(ido-mode t)
(ido-everywhere 1)


