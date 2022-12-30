(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
;; Setup use-package https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)

;; M-x
(ido-mode t)
(setq ido-enable-flex-matching t)
;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(use-package smex
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex))

;; layout
;; show line numbers
(global-linum-mode)
(menu-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; autosave
(setq auto-save-default nil)
(auto-save-visited-mode 1)
(global-auto-revert-mode t)
(setq auto-revert-use-notify nil)

;; recover last point position when reopening a buffer
(save-place-mode 1)

;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-startup-banner 'logo
	;; TODO: try project.el
        ;; dashboard-projects-backend 'projectile
        ;; dashboard-items '((projects . 5)
        ;;                   (recents  . 5)
        ;;                   (bookmarks . 3))
        dashboard-set-navigator t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t))

(use-package all-the-icons
  :if (display-graphic-p))

(load-theme 'modus-vivendi t)
;;(load-theme 'deeper-blue t)

;; packages
(use-package no-littering)
(use-package try)

(setq custom-file "~/oviramcs/vanilla/custom.el")
(load custom-file)
