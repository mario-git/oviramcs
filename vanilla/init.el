(defvar ov/is-mac-os-p (string-equal system-type "darwin"))

(defun ov/comment-or-uncomment-line-or-region ()
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

;; CamelCase as separate words everywhere
(global-subword-mode 1)

(when ov/is-mac-os-p
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
(set-face-attribute 'default nil :height (if ov/is-mac-os-p 140 100))

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

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; packages
(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

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

(use-package general
  :demand t
  :config
  (general-evil-setup)
  (general-create-definer ov/leader-bindings :prefix "SPC" :global-prefix "C-SPC" :states '(normal visual emacs)
    "SPC" 'counsel-M-x
    "be" 'eval-buffer
    "fm" 'toggle-frame-maximized
    "fn" 'make-frame
    "fd" 'delete-frame
    "ff" 'other-frame
    "oi" (lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory)))
    "w/" 'split-window-right
    "w-" 'split-window-below
    "ww" 'other-window)
  (general-create-definer ov/local-leader-bindings :prefix "," :global-prefix "SPC m" :states '(normal visual)))

(use-package emacs
  :general
  (ov/local-leader-bindings :keymaps 'emacs-lisp-mode-map :states '(normal visual)
    "c" 'ov/comment-or-uncomment-line-or-region
    "ee" 'eval-last-sexp
    "ef" 'eval-defun
    "ja" 'avy-goto-word-0
    "jc" 'avy-goto-char
    "js" 'avy-goto-char-2
    "jj" 'avy-goto-char-timer
    "jl" 'avy-goto-line
    "jw" 'avy-goto-word-1)
  :init (setq warning-minimum-level :error)
  :config
  (dolist (pattern '("zprofile\\'" "zprofile.d" "zshrc\\'" "zshrc.d"))
    (add-to-list 'auto-mode-alist (cons pattern 'sh-mode)))
  (use-package color-theme-sanityinc-tomorrow
    :config
    (load-theme 'sanityinc-tomorrow-bright t)
    (set-cursor-color "white"))

  (use-package all-the-icons :if (display-graphic-p))
  (use-package company :config (global-company-mode t))
  (use-package doom-modeline :demand :config (doom-modeline-mode 1))
  (use-package yaml-mode)
  (use-package no-littering)
  (use-package terraform-mode)
  (use-package which-key :config (which-key-mode)))

(use-package exec-path-from-shell
  :config
  (when ov/is-mac-os-p
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs '("PATH"))))

(use-package clojure-mode
  :general
  (ov/local-leader-bindings :keymaps 'clojure-mode-map
    "eb" 'cider-eval-file ;; b as buffer
    "ee" 'cider-eval-last-sexp
    "ef" 'cider-eval-defun-at-point
    "el" 'cider-eval-list-at-point
    "ep" 'cider-eval-defun-up-to-point
    "er" 'cider-eval-region
    "es" 'cider-eval-sexp-at-point
    "ev" 'cider-eval-sexp-at-point ;; kept as orig
    "ea" 'cider-load-all-project-ns
    "ena" 'cider-load-all-project-ns ;; kept also as Spacemacs
    "rr" 'cider-jack-in
    "rn" 'cider-repl-set-ns
    "rq" 'cider-quit
    "ta" 'cider-test-run-project-tests
    "tt" 'cider-test-run
    "tr" 'cider-test-rerun-failed-tests))

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
  (evil-set-undo-system 'undo-redo)
  (use-package evil-anzu :config (global-anzu-mode))
  (use-package evil-surround :config (global-evil-surround-mode 1)))

(use-package evil-cleverparens
  :config
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (define-key evil-insert-state-map (kbd "C-<") 'evil-cp-<)
  (define-key evil-insert-state-map (kbd "C->") 'evil-cp->)
  (evil-define-key 'normal evil-cleverparens-mode-map
    (kbd "[") nil
    (kbd "]") nil
    (kbd "[[") 'evil-cp-previous-opening
    (kbd "]]") 'evil-cp-next-closing))

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config (evil-collection-init))

(use-package cider
  :config
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-repl-display-help-banner nil)
  (setq cider-show-error-buffer t)
  (use-package clj-refactor))

;; M-x & completion juice
(use-package ivy
  :bind
  (:map ivy-minibuffer-map
	("C-j" . ivy-next-line)
	("C-k" . ivy-previous-line)
   :map ivy-switch-buffer-map
	("C-k" . ivy-previous-line)
	("C-d" . ivy-switch-buffer-kill)
   :map ivy-reverse-i-search-map
	("C-k" . ivy-previous-line)
	("C-d" . ivy-reverse-i-search-kill))
  :general
  (ov/leader-bindings :keymaps 'override :states '(normal visual) "bb" 'ivy-switch-buffer)
  :config
  (ivy-mode)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  ;; no more search starting with ^ by default
  (setq ivy-initial-inputs-alist nil)
  (use-package amx :config (amx-mode))
  (use-package ivy-rich
    :config
    (ivy-rich-mode 1)
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))
  (use-package swiper))

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

(use-package markdown-mode
  :general
  (ov/local-leader-bindings :keymaps 'markdown-mode-map
    "mm" 'markdown-mode
    "mv" 'markdown-view-mode))

(use-package projectile
  :general
  (ov/leader-bindings :keymaps 'override :states '(normal visual)
    "pd" 'projectile-find-dir
    "pf" 'projectile-find-file
    "pp" 'projectile-switch-project
    "pr" 'projectile-replace
    "/" 'counsel-projectile-rg)
  :config
  (projectile-global-mode)
  (setq projectile-project-search-path '("~/code"))
  (use-package counsel-projectile :config (counsel-projectile-mode)))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
	 (clojure-mode . rainbow-delimiters-mode)))

(use-package ranger
  :general
  (ov/leader-bindings :keymaps 'override :states '(normal visual)
    "nd" 'deer
    "nr" 'ranger)
  :config
  (setq ranger-show-preview t
	ranger-show-hidden t
	ranger-cleanup-eagerly t
	ranger-cleanup-on-disable t
	ranger-ignored-extensions '("mkv" "flv" "iso" "mp4")))

(use-package treemacs
  :general
  (ov/leader-bindings :keymaps 'override :states '(normal visual)
    "ta" 'treemacs-add-and-display-current-project
    "tt" 'treemacs)
  :init
  (global-set-key (kbd "C-c t") 'treemacs-select-window)
  :defer t
  :config
  (setq treemacs-project-follow-cleanup t
	treemacs-text-scale 0.2)
  (treemacs-resize-icons 16)
  ;; name function in case we need to add the same hook somewhere else
  (add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))
  (use-package treemacs-evil)
  (use-package treemacs-projectile :after projectile))
