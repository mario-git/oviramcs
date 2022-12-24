;; Define package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; This can be removed from Emacs 28
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)

(defvar is-mac-os-p (string-equal system-type "darwin"))

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Setup use-package
;; https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)

(use-package auto-package-update)

;; https://github.com/Fuco1/dired-hacks#dired-subtree
(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

(use-package dumb-jump
  ;; MEMO: M-. is gd in evil mode
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-prefer-searcher 'rg))

(use-package evil
  :init
  (setq evil-cross-lines t)
  (setq evil-want-C-u-scroll t)
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1)
  (setq evil-move-cursor-back nil)
  (setq evil-default-state 'emacs)
  (use-package undo-fu :config (setq evil-redo-function 'undo-fu-only-redo)))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. This library works around this problem.
;; https://github.com/purcell/exec-path-from-shell
(when is-mac-os-p
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs '("PATH"))))

;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package magit)

(use-package no-littering)
(use-package page-break-lines)
(use-package paredit)
(use-package rainbow-delimiters)

(use-package try)

(add-to-list 'load-path "~/oviramcs/brave/core")
(load "navigation.el")
(load "ui.el")
(load "editing.el")
(load "misc.el")
(load "elisp-editing.el")
(load "setup-clojure.el")

;; Dedicated file for Custom AKA Emacs generating custom code automatically
(setq custom-file "~/oviramcs/brave/custom.el")
(load custom-file)
