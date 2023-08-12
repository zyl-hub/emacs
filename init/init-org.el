;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (scheme . t)))

(setq org-confirm-babel-evaluate nil)
(provide 'init-org)
