(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(geiser-guile projectile super-save pdf-tools dirvish zenburn-theme dashboard geiser-mit ob-async macrostep-geiser orderless vertico marginalia geiser company which-key))
 '(package-vc-selected-packages
   '((org-yt :vc-backend Git :url "https://github.com/TobiasZawada/org-yt"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(defmacro eat/pkg (package &optional vc)
  `(unless (package-installed-p ',package)
     (unless (memq ',package package-archive-contents)
       (package-refresh-contents))
     (if ,vc (package-vc-install ,vc)
       (package-install ',package))))

;; (eat/pkg org-yt "https://github.com/TobiasZawada/org-yt")

;; (unless (package-installed-p 'org-yt)
;;   (package-vc-install "https://github.com/Tobiaszawada/org-yt"))

(require 'init-emacs)
(require 'init-scheme)
(require 'init-docs)
(require 'init-org)
