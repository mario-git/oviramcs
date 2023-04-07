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
(set-face-attribute 'default nil :height (if ov/is-mac-os-p 140 100))
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

;; TODO: double check if this is worth - swapped isearch/isearch-regex bindings
;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)
;; (global-set-key (kbd "C-M-s") 'isearch-forward)
;; (global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; ibuffer is better than list-buffers (here overwritten)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; interactions with OS x-select-enable-clipboard & primary don't play well with Meow
(setq save-interprogram-paste-before-kill t
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

;; (use-package general
;;   :demand t
;;   :config
;;   (general-evil-setup)
;;   (general-create-definer ov/space-bindings :prefix "SPC" :states '(normal visual emacs)
;;     "SPC" 'counsel-M-x
;;     "fm" 'toggle-frame-maximized ;; Mf - M maximise
;;     "fn" 'make-frame ;; mf - m make; or F
;;     "fd" 'delete-frame ;; df - d as delete
;;     "ff" 'other-frame ;; [f and ]f
;;     "w/" 'split-window-right ;; / (instead of search); or with comma bindings, as ,/
;;     "w-" 'split-window-below ;; see line above
;;     "wd" 'delete-window ;; dw
;;     "ww" 'other-window ;; [w and ]w
;;     "W" 'delete-other-windows
;;     ;; more binding for redisizing windows both ways
;;     )
;;   (general-create-definer ov/comma-bindings :prefix "," :states '(normal visual emacs)
;;     ;; for all of the following, switch to SPC leader
;;     "c" 'ov/comment-or-uncomment-line-or-region
;;     "D" 'delete-blank-lines ;; db
;;     "gg" 'evil-goto-definition
;;     "ja" 'avy-goto-word-0
;;     "jc" 'avy-goto-char
;;     "js" 'avy-goto-char-2
;;     "jj" 'avy-goto-char-timer
;;     "jl" 'avy-goto-line
;;     "jw" 'avy-goto-word-1))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
	meow-use-clipboard t)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(use-package meow
  :config
  (meow-setup)
  (meow-global-mode 1))

(use-package emacs
  ;; :general
  ;; (ov/comma-bindings :keymaps 'emacs-lisp-mode-map :states '(normal visual)
  ;;   "eb" 'eval-buffer
  ;;   "ee" 'eval-last-sexp
  ;;   "ef" 'eval-defun)
  :init (setq warning-minimum-level :error)
  :config
  (global-set-key (kbd "C-x _") 'shrink-window)
  (dolist (pattern '("zprofile\\'" "zprofile.d" "zshrc\\'" "zshrc.d"))
    (add-to-list 'auto-mode-alist (cons pattern 'sh-mode)))
  (load-theme 'modus-vivendi t)
  (use-package all-the-icons :if (display-graphic-p))
  (use-package company :config (global-company-mode t))
  (use-package doom-modeline :demand :config (doom-modeline-mode 1))
  (use-package no-littering)
  (use-package terraform-mode)
  (use-package yaml-mode)
  (use-package wgrep)
  (defadvice split-window (after split-window-after activate) (other-window 1)))

(use-package exec-path-from-shell
  :config
  (when ov/is-mac-os-p
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs '("PATH"))))

(use-package clojure-mode
;;   :general
;;   (ov/comma-bindings :keymaps 'clojure-mode-map
;;     "eb" 'cider-eval-file
;;     "ee" 'cider-eval-last-sexp
;;     "ef" 'cider-eval-defun-at-point
;;     "el" 'cider-eval-list-at-point
;;     "ep" 'cider-eval-defun-up-to-point ;; eu - eval up to
;;     "er" 'cider-eval-region
;;     "es" 'cider-eval-sexp-at-point ;; ep - eval point
;;     "ev" 'cider-eval-sexp-at-point ;; kept as orig
;;     "ea" 'cider-load-all-project-ns
;;     "ena" 'cider-load-all-project-ns ;; kept as Spacemacs
;;     "k" 'cider-kill-last-result
;;     "rc" 'cider-connect-clj
;;     "rr" 'cider-jack-in
;;     "rn" 'cider-repl-set-ns
;;     "rq" 'cider-quit
;;     "ta" 'cider-test-run-project-tests
;;     "tt" 'cider-test-run)
  )

;; TODO: so evil and cleverparens are gone. Paredit or lispy?

(use-package cider
  :config
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-repl-display-help-banner nil)
  (setq cider-show-error-buffer t)
  ;; TODO: check if I still need this
  ;;   (use-package clj-refactor)
  )

(defun ov/open-init-el () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory)))
(defun ov/open-dashboard () (interactive) (switch-to-buffer (get-buffer-create "*dashboard*")))
(defun ov/open-stuff-file () (interactive) (find-file (expand-file-name "~/code/stuff.txt")))
(defun ov/open-scratch () (interactive) (switch-to-buffer (get-buffer-create "*scratch*")))

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
;; :general
;; (general-nmap "SPC g t" 'git-timemachine-toggle)
;; (git-timemachine-mode-map
;;  "C-k" 'git-timemachine-show-previous-revision
;;  "C-j" 'git-timemachine-show-next-revision
;;  "q" 'git-timemachine-quit)
)

;; (use-package magit
;;   :general
;;   (ov/space-bindings
;;     "g b" 'magit-blame
;;     "g G" 'magit-status
;;     "g l" 'magit-log))

;; M-x & completion juice
(use-package ivy
  ;; :bind
  ;; (:map ivy-minibuffer-map
  ;; 	("C-j" . ivy-next-line)
  ;; 	("C-S-j" . ivy-next-line-and-call)
  ;; 	("C-k" . ivy-previous-line)
  ;; 	("C-S-k" . ivy-previous-line-and-call)
  ;;  :map ivy-switch-buffer-map
  ;; 	("C-k" . ivy-previous-line)
  ;; 	("C-d" . ivy-switch-buffer-kill)
  ;;  :map ivy-reverse-i-search-map
  ;; 	("C-k" . ivy-previous-line)
  ;; 	("C-d" . ivy-reverse-i-search-kill))
  ;; :general
  ;; (ov/space-bindings :keymaps 'override :states '(normal visual emacs)
  ;;   "o" '(:ignore t :which-key "open")
  ;;   "ob" 'ivy-switch-buffer
  ;;   "oi" 'ov/open-init-el
  ;;   "os" 'ov/open-stuff-file)
  :config
  (ivy-mode)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  ;; no more search starting with ^ by default
  (setq ivy-initial-inputs-alist nil)
  ;; (use-package amx :config (amx-mode))
  ;; (use-package ivy-rich
  ;;   :config
  ;;   (ivy-rich-mode 1)
  ;;   (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))
  (use-package swiper))

;; (use-package lsp-mode
;;   :commands
;;   (lsp lsp-deferred)
;;   ;; :general
;;   ;; (ov/comma-bindings :keymaps 'lsp-mode-map :states '(normal)
;;   ;;   ;; why on earth these need a space in between...
;;   ;;   ;; ... SPC bindings?
;;   ;;   "g r" 'lsp-find-references
;;   ;;   "g e" 'lsp-treemacs-errors-list
;;   ;;   "R" 'lsp-rename)
;;   :hook ((clojure-mode . lsp-mode))
;;   ;; :config (use-package lsp-treemacs)
;;   )

;; (use-package org
;;   :config
;;   (setq org-todo-keyword-faces '(("TODO" . "yellow")
;; 				 ("NEXT" . "aqua")
;; 				 ("IN PROGRESS" . "pink")
;; 				 ("DONE" . "green")
;; 				 ("CANCELLED" . "purple"))
;; 	org-todo-keywords '((sequence "TODO" "NEXT" "IN PROGRESS" "WAIT" "|" "DONE" "CANCELLED"))))

;; (use-package org-bullets
;;   :after org
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; (use-package markdown-mode
;;   ;; :general
;;   ;; (ov/comma-bindings :keymaps 'markdown-mode-map
;;   ;;   "mm" 'markdown-mode
;;   ;;   "mv" 'markdown-view-mode)
;;   )

(use-package projectile
  ;; :general
  ;; (ov/space-bindings :keymaps 'override :states '(normal visual emacs)
  ;;   "pl" '(projectile-discover-projects-in-search-path :which-key "load/refresh project list") ;; lp or P
  ;;   "pf" 'projectile-find-file ;; f or ff or fp
  ;;   "pp" 'projectile-switch-project
  ;;   "pr" 'projectile-replace ;; rp
  ;;   "pR" 'projectile-recentf ;; lr - as list recent?
  ;;   "/" 'counsel-projectile-rg ;; S or sn or sb (search blank)
  ;;   "sl" (general-simulate-key "SPC / M-p" :name last-search) ;; ss?
  ;;   ;; standard * without leader in Spacemacs matches symbol with ahs, replicate it
  ;;   "*" (general-simulate-key "SPC / M-n" :name search-thing-under-point))
  :config
  (projectile-global-mode)
  (setq projectile-project-search-path '("~/code"))
  (use-package counsel-projectile :config (counsel-projectile-mode)))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
	 (clojure-mode . rainbow-delimiters-mode)))

;; barely used, ditch this alltogether and learn better dired?
(use-package ranger
  ;; :general
  ;; (ov/space-bindings :keymaps 'override :states '(normal visual emacs)
  ;;   "nd" 'deer
  ;;   "nr" 'ranger)
  :config
  (setq ranger-show-preview t
	ranger-show-hidden t
	ranger-cleanup-eagerly t
	ranger-cleanup-on-disable t
	ranger-ignored-extensions '("mkv" "flv" "iso" "mp4")))

(use-package treemacs
  ;; :general
  ;; (ov/space-bindings :keymaps 'override :states '(normal visual emacs treemacs)
  ;;   "ta" 'treemacs-add-and-display-current-project ;; at or ap, a as add
  ;;   "tt" 'treemacs)
  :defer t
  :config
  (setq treemacs-project-follow-cleanup t
	treemacs-text-scale 0.2)
  (treemacs-resize-icons 16)
  name function in case we need to add the same hook somewhere else
  (add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))
  (use-package treemacs-projectile :after projectile))

(use-package which-key
  :demand
  ;; :general
  ;; (ov/space-bindings "?" 'which-key-show-top-level)
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.4))
