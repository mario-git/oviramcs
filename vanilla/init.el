(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
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

(show-paren-mode 1)

;; y/s rather than yes/no
(setq use-short-answers t)

;; swapped isearch/isearch-regex bindings
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; ibuffer is better than list-buffers (here overwritten)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; interactions with OS
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t)

;; no more lockfiles (temprary files starting with .#)
(setq create-lockfiles nil)

;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-startup-banner 'logo
	;; TODO: re enable & amend this
        ;; dashboard-projects-backend 'projectile
        ;; dashboard-items '((projects . 5)
        ;;                   (recents  . 5)
        ;;                   (bookmarks . 3))
        dashboard-set-navigator t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t))

(use-package evil
  :init
  (setq evil-want-C-u-scroll t
	evil-disable-insert-state-bindings t
	;; precondition for evil-collection
	evil-want-keybinding nil)
  :config
  (use-package evil-escape :config (evil-escape-mode t))
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config (evil-collection-init))

(use-package all-the-icons
  :if (display-graphic-p))

(load-theme 'modus-vivendi t)
;;(load-theme 'deeper-blue t)

;; packages
(use-package no-littering)
(use-package try)

(use-package which-key :config (which-key-mode))

(setq custom-file "~/oviramcs/vanilla/custom.el")
(load custom-file)

;; TODOs:
;; handier way to close popup windows (for example help menus)
;; treemacs
;; cleverparens (or anything similar)
;; surround
;; ivy
;; clojure!
;; items with number in dashboard, spacemacs like (using current dashboard package or writing custom code)
