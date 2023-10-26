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
  (defun ov/euro () (interactive) (insert-char 8364))
  (global-set-key (kbd "s-4") 'ov/euro)
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
(set-face-attribute 'default nil :height (if ov/is-mac-os-p 130 100))
(blink-cursor-mode 0)

(setq-default show-trailing-whitespace t)
(dolist (hook '(minibuffer-setup-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace nil))))
;; folding
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; focus on help window
(setq help-window-select t)

;; autosave & buffer sync with file system
(setq auto-save-default nil
      auto-save-include-big-deletions t
      auto-save-interval 0
      auto-save-timeout 1
      auto-save-visited-interval 1)
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

(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; ibuffer is better than list-buffers (here overwritten)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; interactions with OS
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t)

(setq create-lockfiles nil) ;; temp files starting with .#
(setq make-backup-files nil) ;; ending with ~

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
  (general-create-definer ov/space-bindings :prefix "SPC" :states '(normal visual)
    "fm" 'toggle-frame-maximized
    "fn" 'make-frame
    "fd" 'delete-frame
    "ff" 'other-frame ;; [f and ]f
    "r" 'repeat
    "sg" 'find-grep-dired
    "sf" 'find-name-dired
    ;; following not used now, kept as example
    ;; "sc" (lambda (regexp) (interactive (list (read-string "regexp to grep in ~/code: " ))) (find-grep-dired "~/code" regexp))
    ;; add more bindings for redisizing windows both ways
    "w/" 'split-window-right
    "w-" 'split-window-below
    "wd" 'delete-window
    "ww" 'other-window ;; [w and ]w and hydra
    "W" 'delete-other-windows)
  (general-create-definer ov/comma-bindings :prefix "," :states '(normal visual) :keymaps 'override
    "c" 'ov/comment-or-uncomment-line-or-region
    "gg" 'evil-goto-definition
    "ja" 'avy-goto-word-0 ;; a as all/any
    "jj" 'avy-goto-char-timer
    "jl" 'avy-goto-line)
  (general-nmap
    "s-[" #'evil-cp-previous-opening
    "s-]" #'evil-cp-next-closing))

(use-package emacs
  :general
  (ov/comma-bindings :keymaps 'emacs-lisp-mode-map :states '(normal visual)
    "eb" 'eval-buffer
    "ee" 'eval-last-sexp
    "ef" 'eval-defun)
  :init (setq warning-minimum-level :error)
  :config
  (global-set-key (kbd "C-x _") 'shrink-window)
  (dolist (pattern '("zprofile\\'" "zprofile.d" "zshenv" "zshrc\\'" "zshrc.d"))
    (add-to-list 'auto-mode-alist (cons pattern 'sh-mode)))
  (load-theme 'modus-vivendi t)
  (add-to-list 'image-types 'svg) ; for treemacs complaining about it
  (use-package all-the-icons :if (display-graphic-p))
  (use-package company :config (global-company-mode t))
  (use-package dockerfile-mode)
  (use-package doom-modeline :demand :config (doom-modeline-mode 1))
  (use-package nix-mode :mode "\\.nix\\'")
  (use-package no-littering)
  (use-package terraform-mode)
  ;; w as write
  (use-package wgrep)
  (defadvice split-window (after split-window-after activate) (other-window 1))
  (setq exec-path (append '("~/.nix-profile/bin/") exec-path)))

(use-package exec-path-from-shell
  :config
  (when ov/is-mac-os-p
    (add-to-list 'exec-path-from-shell-variables "LSP_USE_PLISTS")
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs '("PATH"))))

(use-package clojure-mode
  :general
  (ov/comma-bindings :keymaps 'clojure-mode-map
    "eb" 'cider-eval-file
    "ee" 'cider-eval-last-sexp
    "ef" 'cider-eval-defun-at-point
    "el" 'cider-eval-list-at-point
    "ep" 'cider-eval-defun-up-to-point
    "er" 'cider-eval-region
    "es" 'cider-eval-sexp-at-point
    "ea" 'cider-load-all-project-ns
    "k" 'cider-kill-last-result
    "rc" 'cider-connect-clj
    "ro" 'cider-switch-to-repl-buffer
    "rr" 'cider-jack-in
    "rn" 'cider-repl-set-ns
    "rq" 'cider-quit
    "ta" 'cider-test-run-project-tests
    "tt" 'cider-test-run-test))

(defun ov/vnoremap-dot-impl ()
  "(almost) implements .vimrc config like \"vnoremap . :norm.<CR>\".
It populates ex mode with the right stuff, then you have to press . and CR to repeat the last operation over visual selection.
.vimrc would allow to press . only "
  (interactive)
  (evil-ex  "'<,'>norm."))

(use-package evil
  :init
  (setq evil-want-C-u-scroll t
	evil-disable-insert-state-bindings t
	evil-symbol-word-search t
	;; precondition for evil-collection
	evil-want-keybinding nil)
  :config
  (use-package evil-escape :config (evil-escape-mode t))
  (evil-mode 1)
  (setq evil-cross-lines t
	evil-kill-on-visual-paste nil
	evil-move-beyond-eol t)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-set-undo-system 'undo-redo)
  (eval-after-load "evil-maps" (define-key evil-motion-state-map (kbd "C-e") nil))
  (define-key evil-motion-state-map (kbd "C-S-y") 'evil-scroll-line-down)
  (define-key evil-visual-state-map "." 'ov/vnoremap-dot-impl)
   ;; to display current/out-of-total when navigating search results
  (use-package evil-anzu :config (global-anzu-mode))
  (use-package evil-surround :config (global-evil-surround-mode 1)))

(use-package evil-cleverparens
  :config
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  ;; add somewhere bindigs for paredit slurp/barf
  ;; maybe ,b ,B ,s ,S or ,[ ,] ,{ ,}
  ;; or any of the above with space
  (setq evil-cleverparens-use-regular-insert t)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  ;; https://docs.cider.mx/cider/troubleshooting.html#pressing-ret-in-the-repl-does-not-evaluate-forms
  (define-key paredit-mode-map (kbd "RET") nil))

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config (evil-collection-init))

(defun ov/evil-mc-toggle-frozen ()
  (interactive)
  (if evil-mc-frozen
    (progn
      (setq evil-mc-frozen nil)
      (message "evil-mc - cursors unfrozen"))
    (progn
      (setq evil-mc-frozen t)
      (message "evil-mc - cursors frozen"))))

(use-package evil-mc
  :after evil
  :demand
  :general
  (ov/space-bindings
    "ca" '(evil-mc-make-all-cursors :which-key "evil-mc all matches")
    "cc" 'ov/evil-mc-toggle-frozen
    "ch" '(evil-mc-make-cursor-here :which-key "evil-mc cursor here")
    ; would be nice a hydra for next two
    "cj" '(evil-mc-make-cursor-move-next-line :which-key "evil-mc make & next line")
    "ck" '(evil-mc-make-cursor-move-prev-line :which-key "evil-mc make & prev line")
    "cp" '(evil-mc-pause-cursors :which-key "evil-mc pause")
    "cr" '(evil-mc-resume-cursors :which-key "evil-mc resume")
    "cn" '(evil-mc-make-and-goto-next-match :which-key "evil-mc make & go next match")
    "cm" '(:keymap evil-mc-cursors-map)
    "cq" '(evil-mc-undo-all-cursors :which-key "evil-mc undo all"))
  (general-nmap
    "Q" '(evil-mc-undo-all-cursors :which-key "evil-mc undo all"))
  (general-vmap
    "A" '(evil-mc-make-cursor-in-visual-selection-end :which-key "evil-mc end visual selection")
    "I" '(evil-mc-make-cursor-in-visual-selection-beg :which-key "evil-mc begin visual selection"))
  :config (global-evil-mc-mode 1))

(use-package cider
  :config
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-repl-display-help-banner nil)
  (setq cider-show-error-buffer t)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (evil-set-initial-state 'cider-repl-mode 'emacs))

(defun ov/open-init-el () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory)))
(defun ov/open-stuff-file () (interactive) (find-file (expand-file-name "~/code/stuff.txt")))

(use-package git-gutter
  :config
  (global-git-gutter-mode t)
  (setq git-gutter:update-interval 0.02
	git-gutter:modified-sign " "
	git-gutter:added-sign " "
	git-gutter:deleted-sign " "))

(use-package git-timemachine
  :hook (git-time-machine-mode . evil-normalize-keymaps)
  :init (setq git-timemachine-show-minibuffer-details t)
  :general
  (general-nmap "SPC g t" 'git-timemachine-toggle)
  (git-timemachine-mode-map
   "C-k" 'git-timemachine-show-previous-revision
   "C-j" 'git-timemachine-show-next-revision
   "q" 'git-timemachine-quit))

(use-package magit
  :general
  (ov/space-bindings
    "g b" 'magit-blame
    "g G" 'magit-status
    "g l" 'magit-log))

;; M-x & completion juice
(use-package ivy
  :bind
  (:map ivy-minibuffer-map
	("C-j" . ivy-next-line)
	("C-S-j" . ivy-next-line-and-call)
	("C-k" . ivy-previous-line)
	("C-S-k" . ivy-previous-line-and-call)
   :map ivy-switch-buffer-map
	("C-k" . ivy-previous-line)
	("C-d" . ivy-switch-buffer-kill)
   :map ivy-reverse-i-search-map
	("C-k" . ivy-previous-line)
	("C-d" . ivy-reverse-i-search-kill))
  :general
  (ov/space-bindings :keymaps 'override :states '(normal visual)
    "o" '(:ignore t :which-key "open")
    "bb" 'ivy-switch-buffer
    "oi" 'ov/open-init-el
    "os" 'ov/open-stuff-file)
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

(use-package lsp-mode
  :commands
  (lsp lsp-deferred)
  :general
  (ov/comma-bindings :keymaps 'lsp-mode-map :states '(normal)
    ;; why on earth these need a space in between...
    "g r" 'lsp-find-references
    "g e" 'lsp-treemacs-errors-list
    "R" 'lsp-rename)
  :hook ((clojure-mode . lsp-mode))
  :config
  (use-package lsp-treemacs)
  ;; 1mb
  (setq read-process-output-max (* 1024 1024))
  ;; 100mb
  (setq gc-cons-threshold 100000000))

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
  (ov/comma-bindings :keymaps 'markdown-mode-map
    "mm" 'markdown-mode
    "mv" 'markdown-view-mode))

(use-package projectile
  :general
  (ov/space-bindings :keymaps 'override :states '(normal visual)
    "pl" '(projectile-discover-projects-in-search-path :which-key "load/refresh project list")
    "pf" 'projectile-find-file
    "pp" 'projectile-switch-project
    "pr" 'projectile-replace
    "pR" 'projectile-recentf
    "/" 'counsel-projectile-git-grep
    "sl" (general-simulate-key "SPC / M-p" :name last-search)
    "*" (general-simulate-key "SPC / M-n" :name search-thing-under-point))
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq projectile-project-search-path '("~/code"))
  (setq projectile-globally-ignored-directories
	(append projectile-globally-ignored-directories
		'(".clj-kondo" ".cpcache" ".lsp" ".shadow-cljs" ".git" "node_modules" "public" "target")))
  (setq projectile-globally-ignored-files
	(append projectile-globally-ignored-files '("package-lock.json" "p10k")))
  (use-package counsel-projectile :config (counsel-projectile-mode)))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
	 (clojure-mode . rainbow-delimiters-mode)))

(use-package treemacs
  :general
  (ov/space-bindings :keymaps 'override :states '(normal visual treemacs)
    "ta" 'treemacs-add-and-display-current-project
    "tt" 'treemacs)
  :defer t
  :config
  (setq treemacs-project-follow-cleanup t
	treemacs-text-scale 0.2)
  (treemacs-resize-icons 16)
  ;; name function in case we need to add the same hook somewhere else
  (add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode -1)))
  (use-package treemacs-evil)
  (use-package treemacs-projectile :after projectile))

(use-package which-key
  :demand
  :general
  (ov/space-bindings "?" 'which-key-show-top-level)
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.4))

(use-package yaml-mode)
(use-package yaml-pro
  :general
  (general-nmap
    "zc" 'yaml-pro-fold-at-point
    "zo" 'yaml-pro-unfold-at-point))
