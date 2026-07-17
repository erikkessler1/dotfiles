
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(org-babel-load-file "~/.emacs.d/configuration.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(company-show-quick-access t nil nil "Easy candidate navigation with M-<n>")
 '(css-indent-offset 2)
 '(custom-safe-themes
   '("599f1561d84229e02807c952919cd9b0fbaa97ace123851df84806b067666332"
     "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d"
     "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e"
     "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a"
     default))
 '(helm-mode t)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(js2-strict-missing-semi-warning nil)
 '(key-chord-one-key-delay 0.18)
 '(key-chord-two-keys-delay 0.05)
 '(magit-fetch-arguments '("--prune"))
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3"
     "#94BFF3" "#DC8CC3"))
 '(package-selected-packages
   '(ace-window ag aggressive-indent clojure-mode company compat
                counsel-projectile diff-hl diminish doom-modeline
                expand-region flx flycheck-color-mode-line flymd
                git-commit go-mode graphql-mode json-mode kele magit
                nerd-icons projectile-rails rspec-mode rubocop
                smartparens smex transient use-package-chords
                use-package-ensure-system-package web-mode xref-js2
                yaml-mode yasnippet zenburn-theme))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(standard-indent 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:background "MediumPurple1" :foreground "gray100")))))
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
