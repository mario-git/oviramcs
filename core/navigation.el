;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.


;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Gems from http://whattheemacsd.com/
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq ns-function-modifier 'hyper)
;; fix for € on mac keyboard, to make it work like a Brit PC one. 8364 -> €
(when is-mac-os-p
  (global-set-key (kbd "s-4") (lambda () (interactive) (insert-char 8364))))

;; https://github.com/emacsmirror/multiple-cursors
(setq mc/always-run-for-all t)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(use-package smex
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex))

(use-package ivy
  :config
  (ivy-mode)
  (use-package swiper))

(use-package projectile
  :config
  (projectile-global-mode)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "s-d") 'projectile-find-dir)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-switch-project)
  (define-key projectile-mode-map (kbd "s-f") 'projectile-find-file)
  (define-key projectile-mode-map (kbd "s-g") 'projectile-grep)
  (define-key projectile-mode-map (kbd "s-r") 'projectile-replace)
  ;; Workaround for projectile-grep, as per
  ;; https://github.com/bbatsov/projectile/issues/1075#issuecomment-1003794929
  (setq projectile-use-git-grep t)
  (use-package counsel-projectile
    :config (counsel-projectile-mode)))
