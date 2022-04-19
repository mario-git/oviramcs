(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("node" . js2-mode)
  :init (setq js-indent-level 2)
  :config
  (use-package js2-refactor
    :diminish
    :hook (js2-mode . js2-refactor-mode)
    :config (js2r-add-keybindings-with-prefix "C-c C-m"))
  (use-package xref-js2
    :config
    (define-key js2-mode-map (kbd "M-.") nil)
    (add-hook 'js2-mode-hook (lambda ()
                               (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
    (setq xref-js2-search-program 'rg)))
