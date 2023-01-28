(defvar is-mac-os-p (string-equal system-type "darwin"))

;; CamelCase as separate words everywhere
(global-subword-mode 1)

(when is-mac-os-p
  ;; fix for € on mac keyboard, to make it work like a Brit PC one. 8364 -> €
  (global-set-key (kbd "s-4") (lambda () (interactive) (insert-char 8364)))
  ;; Windows/Linux like positioning, starting with option and command already flipped via Karabiner.
  ;; This is not necessary with Emacs for Mac OS X (which I was using), it seems necessary for emacs-plus
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (setq ns-function-modifier 'hyper))

;; layout
;; highlight current line
(global-hl-line-mode 1)
(global-display-line-numbers-mode)
(menu-bar-mode -1)
(setq use-dialog-box nil)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(load-theme 'modus-vivendi t)
;; different font size for Mac and others
(set-face-attribute 'default nil :height (if is-mac-os-p 140 100))
(setq-default show-trailing-whitespace t)
(dolist (hook '(minibuffer-setup-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace nil))))
;; folding
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; focus on help window
(setq help-window-select t)

;; autosave & buffer sync with file system
(setq auto-save-default nil)
(auto-save-visited-mode 1)
(global-auto-revert-mode t)
;; dired and other buffers
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-use-notify nil)

(savehist-mode 1)
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

(use-package clojure-mode)

(use-package cider
  :config
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-repl-display-help-banner nil)
  (setq cider-show-error-buffer t)
  (use-package clj-refactor))

(use-package company :config (global-company-mode t))

;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-startup-banner 'logo
        dashboard-projects-backend 'projectile
        dashboard-items '((projects . 6) (recents . 8))
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
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-set-undo-system 'undo-redo))

(use-package evil-cleverparens
  :config
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  (evil-define-key 'normal evil-cleverparens-mode-map
      (kbd "[") nil
      (kbd "]") nil
      (kbd "[[") 'evil-cp-previous-opening
      (kbd "]]") 'evil-cp-next-closing))

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config (evil-collection-init))

(use-package evil-surround :config (global-evil-surround-mode 1))

(use-package general
  :config
  (general-create-definer leader-bindings :prefix "SPC")
  (general-create-definer local-leader-bindings :prefix ",")
  ;; for more examples: https://github.com/noctuid/general.el#evil-examples
  (leader-bindings :keymaps 'override :states '(normal visual)
    "b" 'ivy-switch-buffer
    "nd" 'deer
    "nr" 'ranger
    "pd" 'projectile-find-dir
    "pf" 'projectile-find-file
    "pp" 'projectile-switch-project
    "pr" 'projectile-replace
    "/" 'counsel-projectile-rg
    "ta" 'treemacs-add-and-display-current-project
    "tt" 'treemacs))

;; M-x & completion juice
(use-package ivy
  :config
  (ivy-mode)
  ;; not 100% sure why evil-collection doesn't fix these 2 bindings
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") #'ivy-previous-line)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  ;; no more search starting with ^ by default
  (setq ivy-initial-inputs-alist nil)
  (use-package amx :config (amx-mode))
  (use-package ivy-rich
    :config
    (ivy-rich-mode 1)
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))
  (use-package swiper))

(use-package no-littering)

(use-package org
  :config
  (setq org-todo-keyword-faces '(("TODO" . "yellow")
				 ("NEXT" . "aqua")
				 ("IN PROGRESS" . "pink")
				 ("DONE" . "green")
				 ("CANCELLED" . "purple"))
	org-todo-keywords '((sequence "TODO" "NEXT" "IN PROGRESS" "WAIT" "|" "DONE" "CANCELLED"))))

(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package evil-org
  :after org
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode . evil-org-set-key-theme)))

(use-package hungry-delete
  :config
  (global-hungry-delete-mode)
  (setq hungry-delete-join-reluctantly t))

(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-project-search-path '("~/code"))
  (use-package counsel-projectile :config (counsel-projectile-mode)))

(use-package ranger
  :config
  (setq ranger-show-preview t
        ranger-show-hidden t
        ranger-cleanup-eagerly t
        ranger-cleanup-on-disable t
        ranger-ignored-extensions '("mkv" "flv" "iso" "mp4")))

(use-package treemacs
  :init
  (global-set-key (kbd "C-c t") 'treemacs-select-window)
  :defer t
  :config
  (setq treemacs-project-follow-cleanup t
	treemacs-show-hidden-files nil
	treemacs-text-scale 0.2
	treemacs-width 30)
  (treemacs-resize-icons 16)
  ;; name function in case we need to add the same hook somewhere else
  (add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))
  (use-package treemacs-evil)
  (use-package treemacs-projectile :after projectile))

;; TODO: paredit instead?
(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-strict-mode t))
(use-package try)
(use-package which-key :config (which-key-mode))

;; TODOs:
;; cider bindings
;; magit, git gutter?
;; multiple cursor (?)
;; iedit?
;; move away from packages listed alphabetically, create more sensible blocks or extract packages
;; implement a sensible modeline :)
;; evil search / to always show current VS total hits
;; better treemacs - mimic spacemacs/treemacs-project-toggle behaviour
;; verify the following leftovers from brave config
;; (use-package auto-package-update)
;; (use-package dumb-jump
;;   ;; MEMO: M-. is gd in evil mode
;;   :init
;;   (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;;   (setq dumb-jump-prefer-searcher 'rg))
;; ;; https://github.com/magnars/expand-region.el
;; (use-package expand-region
;;   :bind ("C-=" . er/expand-region))
