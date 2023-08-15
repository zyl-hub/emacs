;;; init.el --- Where all the magic begins
;;
;; This file loads Org and then loads the rest of our Emacs initialization from Emacs lisp
;; embedded in literate Org files.

;; Load up Org Mode and (now included) Org Babel for elisp embedded in Org Mode files

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(company-mode vertico-directory drivish zenburn-theme yasnippet
		  which-key vertico super-save smartparens rust-mode
		  projectile pdf-tools org-yt orderless olivetti
		  ob-async meow markdown-mode marginalia
		  macrostep-geiser htmlize geiser-mit geiser-guile
		  flycheck-rust flycheck-guile f dirvish dashboard
		  company)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(let* ((org-dir (expand-file-name
                 "lisp" (expand-file-name
                         "org" (expand-file-name
                                "src" dotfiles-dir))))
       (org-contrib-dir (expand-file-name
                         "lisp" (expand-file-name
                                 "contrib" (expand-file-name
                                            ".." org-dir))))
       (load-path (append (list org-dir org-contrib-dir)
                          (or load-path nil))))
  ;; load up Org and Org-babel
  (require 'org)
  (require 'ob-tangle))

;; load up all literate org-mode files in this directory
(mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))

;; init.el ends here
