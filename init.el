  (setq lexical-binding t)
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          (or (bound-and-true-p straight-base-dir)
              user-emacs-directory)))
        (bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (progn
    (set-locale-environment "en_US.UTF-8")
    (setq system-time-locale "C")  ;; 这个是为了在 org mode 中用英文显示日期，默认是中文
    )

  (defun font-installed-p (font-name)
    "Check if font with FONT-NAME is available."
    (find-font (font-spec :name font-name)))

  (defun nasy-set-font ()
   (cl-loop for font in '("Source Code Pro" "Cascadia Code" "SF Mono"
                           "Fira Code" "Menlo" "Monaco" "Dejavu Sans Mono"
                           "Lucida Console" "Consolas" "SAS Monospace")
             when (font-installed-p font)
             return (set-face-attribute
                     'default nil
                     :font (font-spec :family font
                                      :weight 'normal
                                      :slant 'normal
                                      :size (cond ((eq system-type 'gnu/linux) 18.0)
                                                  ((eq system-type 'windows-nt) 16.0)))))
    (cl-loop for font in '("OpenSansEmoji" "Noto Color Emoji" "Segoe UI Emoji"
                           "EmojiOne Color" "Apple Color Emoji" "Symbola" "Symbol")
             when (font-installed-p font)
             return (set-fontset-font t 'unicode
                                      (font-spec :family font
                                                 :size (cond ((eq system-type 'gnu/linux) 20.0)
                                                             ((eq system-type 'windows-nt) 18.0)))
                                      nil 'prepend))
    (cl-loop for font in '("LXGWWenKai" "Microsoft Yahei UI" "思源黑体 CN" "思源宋体 CN" "微软雅黑 CN"
                           "Source Han Sans CN" "Source Han Serif CN"
                           "WenQuanYi Micro Hei" "文泉驿等宽微米黑"
                           "Microsoft Yahei")
             when (font-installed-p font)
             return (set-fontset-font t '(#x4e00 . #x9fff)
                                      (font-spec :name font
                                                 :weight 'normal
                                                 :slant 'normal
                                                 :size (cond ((eq system-type 'gnu/linux) 20.0)
                                                             ((eq system-type 'windows-nt) 18.0)))))
    (cl-loop for font in '("HanaMinB" "SimSun-ExtB")
             when (font-installed-p font)
             return (set-fontset-font t '(#x20000 . #x2A6DF)
                                      (font-spec :name font
                                                 :weight 'normal
                                                 :slant 'normal
                                                 :size (cond ((eq system-type 'gnu/linux) 20.0)
                                                             ((eq system-type 'windows-nt) 18.0))))))
  (when (display-graphic-p)
    (nasy-set-font))

  (add-hook 'window-setup-hook #'nasy-set-font)
  (add-hook 'server-after-make-frame-hook #'nasy-set-font)


  ;; (set-frame-parameter nil 'bottom-divider-width 0)
  ;; remove strike through line in modeline
  (setq x-underline-at-descent-line t)

  (define-key global-map (kbd "C-z") (make-sparse-keymap))
  (global-set-key (kbd "C-z i") 'package-install)

  ;; remove bars
  ;; (menu-bar-mode -1)
  (tool-bar-mode -1)
  ;; (scroll-bar-mode -1)

  ;; remove ~ files
  ;; (setq make-backup-files nil)
  (setq backup-directory-alist '((".*" . "/tmp/emacs")))

  ;; save place
  (save-place-mode 1)

  ;; auto fill
  ;; (setq-default auto-fill-function 'do-auto-fill)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)

  ;; scroll by one line kbd
  (global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
  (global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

(keymap-global-set "C-x C-a" '(lambda ()
				(interative)
				(eval-region (point-min) (point))))

(straight-use-package 'setup)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

(use-package super-save
  :init
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)
  (setq auto-save-default nil))

(use-package which-key
  :init
  (which-key-mode))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :bind ("C-z d" . 'dirvish-dwim))
  ;;(global-set-key (kbd "C-z d") 'dirvish-dwim))

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))
;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; orderless in 
;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Configure vertico directory extension.
(use-package vertico-directory
  :straight f
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package avy
  :bind ("C-:" . 'avy-goto-char))

(use-package company
  :hook ((prog-mode . company-mode)
	 (text-mode . company-mode))
  :custom
  (company-dabbrev-downcase 0)
  (company-idle-delay 0.1 "respond faster"))

(use-package flycheck
  :init
  (global-flycheck-mode))

(setq treesit-font-lock-level 4)
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(add-hook 'prog-mode-hook 'electric-pair-mode)

(use-package format-all)

(use-package lispy
  :hook (scheme-mode . lispy-mode))

(use-package yasnippet)

(use-package markdown-mode)

(use-package geiser
  :mode ("\\.scm\\'" . scheme-mode)
  :config
  (setq geiser-active-implementations '(mit)))

;; (use-package geiser-guile)

(use-package geiser-mit
  :mode ("\\.scm\\'" . scheme-mode))

(use-package macrostep-geiser
  :after geiser-mode
  :hook (geiser-mode-hook #'macrostep-geiser-setup))

(use-package macrostep-geiser
  :after geiser-repl
  :hook (geiser-repl-mode-hook #'macrostep-geiser-setup))

;; (use-package flycheck-guile
;;   :ensure t)

;; (require 'flycheck-guile)

(eval-after-load 'scheme-mode '(require 'smartparens-scheme))

(use-package rust-mode
  :hook ((rust-mode . (lambda () (setq indent-tabs-mode nil)))))

(use-package flycheck-rust
  :mode ("\\.rs\\'" . rust-mode))

(add-hook 'rust-mode-hook 'eglot-ensure)

(setq org-src-fontify-natively t
      org-src-window-setup 'current-window ;; edit in current window
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation t ;; do not put two spaces on the left
      org-src-tab-acts-natively t)

(use-package htmlize)

(use-package org-download
  :init
  :hook ((dired-mode-hook . org-download-enable)
	 (org-mode . org-download-enable)
	 (org-mode . (lambda ()
		       (setq org-download-screenshot-method "powershell -c Add-Type -AssemblyName System.Windows.Forms;$image = [Windows.Forms.Clipboard]::GetImage();$image.Save('%s', [System.Drawing.Imaging.ImageFormat]::Png)")))))

(use-package ob-async
  :mode ("\\.org\\'" . org-mode))

(use-package toc-org
  :mode ("\\.org\\'" . org-mode))

(if (require 'toc-org nil t)
    (progn
      (add-hook 'org-mode-hook 'toc-org-mode)

      ;; enable in markdown, too
      (add-hook 'markdown-mode-hook 'toc-org-mode))
      ;; (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point))
  (warn "toc-org not found"))
    
;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (scheme . t)))

(setq org-confirm-babel-evaluate nil)

;; src block indentation / editing / syntax highlighting

;; latex preview size
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

;; auto indent
(setq org-startup-indented 1)

(setq org-default-notes-file (concat org-directory "/journal.org"))
(global-set-key (kbd "C-c c") #'org-capture)
(setq org-capture-templates nil)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
	("r" "Things to read" entry (file+datetree "~/org/read.org")
         "* %?\nEntered on %U\n  %i\n")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
	("p" "Paper" entry (file+datetree "~/org/paper.org")
	 "* %?\nEntered on %U\n %i\n %a")))
;; (add-to-list 'org-capture-templates
;;             '("j" "Journal" entry (file "~/org/journal.org")
;;                "* %U - %^{heading}\n  %?"))

(global-set-key (kbd "C-c a") #'org-agenda)

;; (add-to-list 'treesit-language-source-alist
;;              '(typst "https://github.com/uben0/tree-sitter-typst"))
;; (treesit-install-language-grammar 'typst)
(use-package typst-ts-mode
  :straight (:type git :host sourcehut :repo "meow_king/typst-ts-mode" :files (:defaults "*.el"))
  :custom
  ;; don't add "--open" if you'd like `watch` to be an error detector
  (typst-ts-mode-watch-options "--open")
  
  ;; experimental settings (I'm the main dev, so I enable these)
  (typst-ts-mode-enable-raw-blocks-highlight t)
  (typst-ts-mode-highlight-raw-blocks-at-startup t))

(use-package f)

(use-package typit)

(use-package magit)
(setq magit-view-git-manual-method 'man)

(use-package treemacs
  :straight t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(use-package beancount
  :straight f
  :load-path "site-lisp/beancount-mode"
  :mode ("\\.beancount\\'" . beancount-mode)
  :hook ((beancount-mode . (lambda () (setq-local electric-indent-chars nil)))
	 (beancount-mode . outline-minor-mode))
  :bind (:map beancount-mode-map
	      ("C-c C-n" . outline-next-visible-heading)
	      ("C-c C-p" . outline-previous-visible-heading)))
(use-package flymake-bean-check
  :straight f
  :load-path "site-lisp/beancount-mode"
  :hook (beancount-mode . flymake-bean-check-enable))

(use-package leetcode
  :hook (leetcode-solution-mode . (lambda() (flycheck-mode -1)))
  :config
  (setq leetcode-prefer-language "cpp")
  (setq leetcode-prefer-sql "mysql")
  (setq leetcode-save-solution t)
  (setq leetcode-directory "~/leetcode"))
