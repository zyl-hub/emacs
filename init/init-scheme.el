(use-package macrostep-geiser
  :after geiser-mode
  :config (add-hook 'geiser-mode-hook #'macrostep-geiser-setup))

(use-package macrostep-geiser
  :after geiser-repl
  :config (add-hook 'geiser-repl-mode-hook #'macrostep-geiser-setup))

(setq geiser-active-implementations '(guile))

(provide 'init-scheme)
