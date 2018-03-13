
(defun sensible-defaults/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if
there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(setq gc-cons-threshold 50000000) ;; GC limit
(setq sentence-end-double-space nil)
(add-hook 'before-save-hook
            (lambda ()
              (when buffer-file-name
                (let ((dir (file-name-directory buffer-file-name)))
                  (when (and (not (file-exists-p dir))
                             (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                    (make-directory dir t))))))
(delete-selection-mode t) ;; delete selected text when type something else
(setq require-final-newline t) ;; add newlines to files
(setq confirm-kill-emacs 'y-or-n-p)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(setq-default dired-listing-switches "-alh")
(fset 'yes-or-no-p 'y-or-n-p)
(global-font-lock-mode t) ;; syntax highlight whenever possible
(global-auto-revert-mode t) ;; refresh files when changed
(show-paren-mode t)
(setq show-paren-delay 0.0)
(setq ns-pop-up-frames nil) ;; opening files from finder opens in new buffer
(global-set-key (kbd "M-;") 'sensible-defaults/comment-or-uncomment-region-or-line)

(menu-bar-mode -1)
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(setq ispell-program-name "/usr/local/bin/aspell")
(add-to-list 'load-path "~/.emacs.d/resources")

(require 'mouse) ;; needed for iterm2 compatibility
(xterm-mouse-mode t)
(unless window-system
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

(server-start)

(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(require 'package)
(unless (package-installed-p 'use-package) (package-refresh-contents) (package-install 'use-package))
(eval-when-compile (require 'use-package))

(use-package diminish
  :ensure t)

(set-face-attribute 'default nil :height 160)
(global-linum-mode t)
(global-hl-line-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t))

(set-face-attribute 'region nil :background "MediumPurple1" :foreground "gray100")

(add-to-list 'default-frame-alist
             '(font . "Source Code Pro-18"))

(use-package diff-hl
  :ensure t
  :config 
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(setq frame-title-format '((:eval (projectile-project-name))))

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files))
  :config
  (helm-mode t)
  (set 'helm-ff-auto-update-initial-value 1))

(use-package helm-projectile
  :ensure t
  :bind 
  ("s-p" . helm-projectile-find-file)
  ("M-p" . helm-projectile-grep)
  :config
  (helm-projectile-on))

(add-to-list 'load-path "~/.emacs.d/resources/helm-swoop")
(require 'helm-swoop)
(global-set-key (kbd "M-i") 'helm-swoop)
(setq helm-swoop-use-fuzzy-match t)

(use-package helm-flyspell
  :ensure t
  :bind
  ("M-;" . helm-flyspell-correct))

(add-hook 'enh-ruby-mode-hook
  (lambda () (flyspell-prog-mode)))

(use-package ag :ensure t)
(use-package projectile
  :ensure projectile
  :config 
  (projectile-global-mode t)
  (setq projectile-enable-caching t)
  :diminish projectile-mode)

(defun projectile-use-magit-if-possible ()
  "If the project being switched to is a git repository, invoke
magit-status on the project root directory. Use dired otherwise."
  (interactive)
  (if (and (executable-find "git")
           (eq (projectile-project-vcs) 'git))
      (magit-status (projectile-project-root))
    (dired (projectile-project-root))))

(setq projectile-switch-project-action 'projectile-use-magit-if-possible)

(use-package avy
  :ensure t
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-char))
  :config
  (setq avy-background t))

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)

  :config
  (add-hook 'git-commit-mode-hook 'turn-on-flyspell))

(use-package smartparens-config
    :ensure smartparens
    :config
    (progn
      (smartparens-global-mode)
      (show-smartparens-global-mode t)))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package company               
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)

    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t)
    (setq company-dabbrev-downcase nil))
  :diminish company-mode)

(use-package vue-mode
  :ensure t
  )

;;(require 'drag-stuff)
;;(drag-stuff-global-mode 1)
;;(drag-stuff-define-keys)

;;(use-package which-key
;;  :diminish which-key-mode
;;  :config (which-key-mode))

(use-package whitespace
  :commands (whitespace-mode))

(defun other-window-kill-buffer ()
  "Kill the buffer in the other window"
  (interactive)
  ;; Window selection is used because point goes to a different window
  ;; if more than 2 windows are present
  (let ((win-curr (selected-window))
        (win-other (next-window)))
    (select-window win-other)
    (kill-this-buffer)
    (select-window win-curr)))
(global-set-key (kbd "C-x K") 'other-window-kill-buffer)

(require 'chruby)
(chruby "2.5.0")

(use-package rubocop
  :ensure t
  :defer t
  :init (add-hook 'ruby-mode-hook 'rubocop-mode))

(use-package rspec-mode
  :ensure t
  :defer t
  :init 
  (add-hook 'ruby-mode-hook 'rspec-mode)
  (add-hook 'projectile-rails-mode 'rspec-mode))

(use-package projectile-rails
  :ensure t
  :init (projectile-rails-global-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
