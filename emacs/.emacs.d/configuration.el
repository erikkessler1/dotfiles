
;;; -*- lexical-binding: t -*-

(setq gc-cons-threshold 50000000)

(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(require 'package)
(unless (package-installed-p 'use-package) 
  (package-refresh-contents) 
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(use-package diminish
  :ensure t)

(use-package use-package-ensure-system-package
  :ensure t)

(use-package use-package-chords
  :ensure t
  :custom ((key-chord-two-keys-delay .05)
           (key-chord-one-key-delay .18))
  :config (key-chord-mode 1))

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(fset 'yes-or-no-p 'y-or-n-p)

(setq confirm-kill-emacs 'y-or-n-p)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(add-to-list 'load-path (concat user-emacs-directory "resources"))

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      backup-by-copying t
      version-control t ; use version numbers on backups
      delete-old-versions t 
      kept-new-versions 20
      kept-old-versions 5)

(server-start)

(global-unset-key (kbd "C-x C-b"))

(menu-bar-mode -1)
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(global-font-lock-mode t)

(show-paren-mode t)
(setq show-paren-delay 0.0)

(setq-default column-number-mode t)

(global-hl-line-mode t)

(use-package diff-hl
  :ensure t
  :config 
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t))

(set-face-attribute 'region nil :background "MediumPurple1" :foreground "gray100")
(set-face-attribute 'default nil :height 160)
(add-to-list 'default-frame-alist
             '(font . "Source Code Pro-18"))

(setq sentence-end-double-space nil)

(delete-selection-mode t)

(setq require-final-newline t)

(global-auto-revert-mode t)

(setq ns-pop-up-frames nil)

(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

(require 'mouse) ;; needed for iterm2 compatibility

(xterm-mouse-mode t)
(unless window-system
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(use-package avy
  :ensure t
  :chords (("jj" . avy-goto-word-or-subword-1)
          ("jk" . avy-goto-char-timer))
  :custom (avy-background t "darken the background"))

(use-package ivy
  :after counsel
  :diminish
  :bind (("C-c Cr" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom ((ivy-count-format "(%d/%d) ")
           (ivy-use-virtual-buffers t))
  :config
  (ivy-mode t)
  (define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (projectile-find-file . ivy--regex-fuzzy)
          (t . ivy--regex-plus))))

(use-package counsel
  :ensure t
  :diminish
  :chords ("xx" . counsel-M-x)
  :config (counsel-mode t))

(use-package swiper
  :after counsel
  :bind (("C-s" . swiper)))

(use-package flx
  :ensure t)

(use-package smex
  :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package ag 
  :ensure t
  :ensure-system-package ag)

(setq-default dired-listing-switches "-alh")

(defun ek-toggle-maximize-buffer ()
  "Maximize buffer or return to previous configuration"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_) 
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(global-set-key (kbd "<s-return>") 'ek-toggle-maximize-buffer)

(defun ek-copy-file-name ()
  "Copy the current filename to the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :chords (" o" . ace-window)
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config (add-hook 'git-commit-mode-hook 'turn-on-flyspell))

(defun ek-comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if
  there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(global-set-key (kbd "M-;") 'ek-comment-or-uncomment-region-or-line)

(use-package aggressive-indent
  :ensure t)

(use-package smartparens
  :config 
  (smartparens-global-mode)
  (show-smartparens-global-mode t)
  :bind (("C-]" . sp-select-next-thing-exchange)
         ("C-c s r" . sp-rewrap-sexp)
         ("C-M-u" . sp-up-sexp)
         ("C-M-d" . sp-down-sexp)))

(use-package smartparens-config 
  :ensure smartparens)

(use-package company        
  :ensure t
  :diminish company-mode
  :init (global-company-mode)
  :bind ([remap completion-at-point] . company-complete)
  :custom ((company-tooltip-align-annotations t)
           (company-show-numbers t "Easy candidate navigation with M-<n>")
           (company-idle-delay 0 "Show right away")
           (company-dabbrev-downcase nil "Don't downcase stuff")))

(use-package projectile
  :ensure projectile
  :diminish projectile-mode
  :chords (("pp" . projectile-switch-project)
          ("pf" . projectile-find-file))
  :custom ((projectile-enable-caching t)
           (projectile-keymap-prefix (kbd "C-c p"))
           (projectile-completion-system 'ivy))
  :config 
  (projectile-global-mode t)
  (setq frame-title-format '((:eval (projectile-project-name)))))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode t))

(counsel-projectile-modify-action
 'counsel-projectile-switch-project-action
 '((default counsel-projectile-switch-project-action-vc)))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package ruby-mode
  :ensure t
  :custom ((ruby-insert-encoding-magic-comment nil))
  :config
  (add-hook 'ruby-mode-hook 'subword-mode))

(require 'chruby)
(chruby "2.5.1")
(add-hook 'projectile-after-switch-project-hook 'chruby-use-corresponding)

(use-package rubocop
  :ensure t
  :defer t
  :init (add-hook 'ruby-mode-hook 'rubocop-mode))

(defun ek-add-bundle (command)
  (let ((exe (car command))
        (opts (cdr command)))
    (if (string-match "rubocop\\'" exe)
        (if (member "--config" opts)
            (append '("bundle" "exec" "rubocop") opts)
          (cons exe (append opts '("--config" "/Users/ekessler/.my_rubocop.yml"))))
      command)))

(setq flycheck-command-wrapper-function (lambda (command)
                                          (let ((modified-command (ek-add-bundle command))
                                                (inhibit-message t))
                                            (progn
                                              (message "Original Command: %s" command)
                                              (message "Modified Command: %s" modified-command)
                                              modified-command))))

(use-package rspec-mode
  :ensure t
  :init 
  (add-hook 'ruby-mode-hook 'rspec-mode)
  (add-hook 'projectile-rails-mode 'rspec-mode))

(use-package projectile-rails
  :ensure t
  :init (projectile-rails-global-mode))

(use-package web-mode
  :ensure t
  :defer t)

(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))

(use-package flymd 
  :ensure t)

(defun my-flymd-browser-function (url)
  (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "firefox " url)
           nil
           "/usr/bin/open"
           (list "-a" "firefox" url))))

(setq flymd-browser-open-function 'my-flymd-browser-function)

(use-package yaml-mode
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook 'subword-mode))
