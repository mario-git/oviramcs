;; These customizations change the way emacs looks and disable/enable
;; some user interface elements. Some useful customizations are
;; commented out, and begin with the line "CUSTOMIZE". These are more
;; a matter of preference and may require some fiddling to match your
;; preferences

;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)

;; Show line numbers
(global-linum-mode)

;; Removed the graphical toolbar at the top
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Default Theme
;; TODO: find a way to move from the files in FS, taken probably from here
;; https://github.com/ChrisKempson/Tomorrow-Theme
;; to using the ones provided on base16-emacs
;; https://github.com/belak/base16-emacs/blob/master/build/base16-bright-theme.el
;; https://github.com/belak/base16-emacs/blob/master/build/base16-tomorrow-night-theme.el
;; I'm not sure if these two can be combined to rebuild tomorrow-night-bright.
;; Alternatively, load this from init.el, not sure why doing this breaks the cursor.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'tomorrow-night-bright t)

;; different font size for Mac (darwin) and others
(set-face-attribute 'default nil :height (if (eq system-type 'darwin) 140 100))

;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t

      ;; I'm actually not sure what this does but it's recommended?
      x-select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;; Cursor
(blink-cursor-mode 0)
(set-cursor-color "White")

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)

;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq dashboard-startup-banner 'logo)
(setq dashboard-projects-backend 'projectile
      dashboard-items '((projects . 5)
                        (recents  . 5)
                        (bookmarks . 3)
                        (agenda . 5)))
(setq dashboard-set-navigator t)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

;; https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons
  :if (display-graphic-p))
