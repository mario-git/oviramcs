;; layout
;; show line numbers
(global-linum-mode)
(menu-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(load-theme 'modus-vivendi t)

;; focus on help window
(setq help-window-select t)

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

(setq custom-file "~/oviramcs/vanilla/custom.el")
(load custom-file)

;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; Setup use-package https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t
      use-package-always-ensure t)
(require 'use-package)

(use-package all-the-icons :if (display-graphic-p))

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
  ;; TODO: find another way to set leader
  ;; (evil-set-leader 'normal (kbd "SPC"))
  ;; (evil-set-leader 'visual (kbd "SPC"))
  ;; (evil-set-leader 'normal "," t)
  ;; (evil-set-leader 'visual "," t)
  (evil-set-undo-system 'undo-redo))

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config (evil-collection-init))

;; M-x & completion juice
(use-package ivy
  :config
  (ivy-mode)
  ;; not 100% sure why evil-collection doesn't fix these 2 bindings
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") #'ivy-previous-line)
  (use-package swiper))

(use-package no-littering)

;; TODO: bring in treemacs-projectile
(use-package treemacs
  :init
  (global-set-key (kbd "C-c t") 'treemacs-select-window)
  :defer t
  :config
  (setq treemacs-project-follow-cleanup t
	treemacs-show-hidden-files nil
	treemacs-text-scale 0.2)
  (treemacs-resize-icons 16))

(use-package treemacs-evil
  :after (evil evil-collection treemacs)
  ;; :config
  ;; (evil-define-key 'normal 'global (kbd "<leader>to") 'treemacs-select-window)
  )

(use-package try)
(use-package which-key :config (which-key-mode))

;; TODOs:
;; folding
;; cleverparens (or anything similar)
;; surround
;; clojure!
