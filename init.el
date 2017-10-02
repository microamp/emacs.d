;;; .emacs.d/init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Starting up anew

;;; Code:

;; Set up package archives
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Left Command to be Meta on Mac
(when (equal system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-right-command-modifier 'super))

;; Set up some custom key bindings
(global-set-key (kbd "C-x k") #'kill-this-buffer)

(global-set-key (kbd "C-x l") #'delete-other-windows)
(global-set-key (kbd "C-x q") #'delete-window)

(global-set-key (kbd "C-x \\") (lambda ()
                                (interactive)
                                (split-window-horizontally)
                                (other-window 1)))
(global-set-key (kbd "C-x -") (lambda ()
                                (interactive)
                                (split-window-vertically)
                                (other-window 1)))

;; Stop cluttering with backup files
(setq backup-directory-alist '((".*" . "~/.emacs.d/.tmp")))

;; Tabs are evil
(setq-default indent-tabs-mode nil)

;; Split windows vertically by default
(setq split-width-threshold nil)

;; Select *help* buffer automatically
(setq help-window-select t)

;; Hide things you don't want to see
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))

;; Set default font
(set-frame-font "Office Code Pro-11" nil t)

;; Prefer y/n to yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Die whitespaces, die!
(add-to-list 'write-file-functions #'delete-trailing-whitespace)

;; Fullscreen after init
(add-hook 'after-init-hook #'toggle-frame-fullscreen)

;; Record how much it took to load Emacs and be ashamed
(add-hook 'after-init-hook (lambda ()
                             (message (format "Init time: %s" (emacs-init-time)))))

;; Load theme
(add-hook 'after-init-hook (lambda ()
                             (load-theme 'microamp)))

;; Highlight matching parens
(use-package paren
  :init
  (show-paren-mode 1))

;; "How can I replace highlighted text with what I type?" (Emacs FAQ)
(use-package delsel
  :config
  (delete-selection-mode 1))

;; Refresh buffer when changed on disk
(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode 1))

;; Try packages without installing them
(use-package try
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode)

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOROOT")
  (exec-path-from-shell-copy-env "GOPATH"))

(use-package expand-region
  :ensure t
  :defer t
  :bind (("C-c ;" . er/expand-region))
  :init
  (setq expand-region-contract-fast-key "l"))

(use-package hydra
  :ensure t)

;; Enable autocomplete in programming modes
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (setq-default company-idle-delay 0.2
                company-minimum-prefix-length 3)
  :config
  (add-hook 'prog-mode-hook #'company-mode))

;; Enable syntax highlighting in programming modes
(use-package flycheck
  :ensure t
  :after hydra
  :init
  (setq-default flycheck-idle-change-delay 0.5)
  :config
  (defhydra hydra-flycheck-error (global-map "C-c ! !")
    "Flycheck errors"
    ("n" flycheck-next-error "next error")
    ("p" flycheck-previous-error "previous error"))
  (add-hook 'prog-mode-hook #'flycheck-mode))

(use-package page
  :config
  (defhydra hydra-shrink-enlarge-window-horizontally (global-map "C-x }")
    "Enlarge/Shrink window horizontally"
    ("}" enlarge-window-horizontally "enlarge-window-horizontally")
    ("{" shrink-window-horizontally "shrink-window-horizontally"))
  (defhydra hydra-shrink-enlarge-window (global-map "C-x ^")
    ("^" enlarge-window "enlarge-window")
    ("%" shrink-window "shrink-window")))

;; Now you're free of distraction
(use-package olivetti
  :ensure t
  :defer t
  :bind (("C-c w l" . olivetti-mode))
  :init
  (setq olivetti-hide-mode-line t
        olivetti-body-width 0.6))

(use-package elfeed
  :ensure t
  :defer t
  :bind (("C-x M-f" . elfeed)
         :map elfeed-search-mode-map
         ("u" . elfeed-update)
         :map elfeed-show-mode-map
         ("M-n" . elfeed-show-next)
         ("M-p" . elfeed-show-prev)
         ("n" . next-line)
         ("p" . previous-line))
  :init
  (setq-default elfeed-feeds'("http://nullprogram.com/feed/"
                              "http://irreal.org/blog/?feed=rss2")))

;; Simple workspace management
(use-package eyebrowse
  :ensure t
  :defer t
  :bind (("C-c w ." . eyebrowse-switch-to-window-config)
         ("C-c w ," . eyebrowse-rename-window-config)
         ("C-c w DEL" . eyebrowse-close-window-config)
         ("C-c w n" . eyebrowse-create-window-config))
  :init
  (setq eyebrowse-wrap-around t
        eyebrowse-mode-line-style 'always
        eyebrowse-mode-line-separator "|")
  (eyebrowse-mode t))

;; Eshell is an Emacs shell
(use-package eshell
  :ensure t
  :defer t
  :bind (("M-SPC" . eshell))
  :init
  (setq-default eshell-buffer-maximum-lines 5000
                eshell-history-size 200
                eshell-hist-ignoredups t)
  :config
  (defun eshell-init ()
    (defun eshell-clear ()
      ;; NOTE: This erases shell content!
      (interactive)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (eshell-send-input))
    (define-key eshell-mode-map (kbd "C-l") #'eshell-clear))
  (add-hook 'eshell-mode-hook #'eshell-init))

(use-package eldoc
  :diminish eldoc-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

;; Such extensive editor, many key binding, wow
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

;; My entire life in Org-mode
(use-package org
  :pin org
  :ensure org-plus-contrib
  :defer t
  :bind (:map org-mode-map
              ("C-c a" . org-agenda)
              ("C-c C-;" . org-todo)
              ("M-H" . org-metaleft)
              ("M-J" . org-metadown)
              ("M-K" . org-metaup)
              ("M-L" . org-metaright)
              ("M-n" . org-next-visible-heading)
              ("M-p" . org-previous-visible-heading))
  :init
  (setq-default org-agenda-files '("~/.emacs.d/notes/todo.org")
                org-agenda-start-with-follow-mode t
                org-clock-into-drawer t
                org-clock-persist 'history
                org-log-done 'time
                org-log-into-drawer t)
  (org-babel-do-load-languages
   'org-babel-load-languages '((dot . t)
                               (emacs-lisp . t)
                               (js . t)
                               (python . t)
                               (shell . t)))
  :config
  (defun org-clock-in-if-started ()
    (when (and (string= org-state "STARTED")
               (not (string= org-last-state "STARTED")))
      (org-clock-in)))
  (add-hook 'org-after-todo-state-change-hook #'org-clock-in-if-started)
  (advice-add 'org-clock-in :after (lambda (&rest args) (org-todo "STARTED")))
  (add-hook 'org-mode-hook (lambda ()
                             (setq-local auto-save-timeout nil))))

(use-package ob-http
  :ensure t
  :after org
  :init
  (add-to-list 'org-babel-load-languages '(http . t)))

(use-package ox-jira
  :ensure t
  :after org)

(use-package ox-confluence
  :ensure org-plus-contrib
  :after org)

;; Prettify the bullets
(use-package org-bullets
  :ensure t
  :after t
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode))

(use-package deft
  :ensure t
  :bind (("C-c n" . deft)
         :map deft-mode-map
         ("C-g" . bury-buffer)
         ("C-k" . deft-filter-clear))
  :init
  (setq deft-directory "~/.emacs.d/notes"
        deft-extensions '("markdown" "md" "org")
        deft-recursive t
        deft-use-filename-as-title t)
  :config
  (advice-add 'deft :before #'deft-filter-clear)
  (advice-add 'deft :before #'deft-refresh))

;; NOTE: Requires `pandoc'
(use-package pandoc-mode
  :ensure t)

;; NOTE: Requires `pandoc'
(use-package ox-pandoc
  :ensure t
  :after pandoc-mode)

(use-package graphviz-dot-mode
  :ensure t
  :mode ("\\.dot\\'" . graphviz-dot-mode))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

;; NOTE: Requires `vmd'
(use-package vmd-mode
  :ensure t
  :after markdown-mode
  :bind (:map markdown-mode-map
              ("C-c C-v" . vmd-mode)))

;; Visibile bookmarks in buffer
(use-package bm
  :ensure t
  :bind (("C-M-;" . bm-toggle)
         ("C-M-," . bm-next)
         ("C-M-<" . bm-previous)))

(use-package highlight-thing
  :ensure t
  :diminish highlight-thing-mode
  :init
  (setq highlight-thing-delay-seconds 0.5
        highlight-thing-limit-to-defun t
        highlight-thing-case-sensitive-p t)
  (add-hook 'prog-mode-hook #'highlight-thing-mode))

;; Sometimes the best fix is turning it off and on again
(use-package restart-emacs
  :ensure t
  :defer t
  :bind (("C-x M-r" . restart-emacs)))

(use-package mwim
  :ensure t
  :defer t
  :bind (("C-a" . mwim-beginning-of-code-or-line)))

;; Win at windowing
(use-package ace-window
  :ensure t
  :defer t
  :bind (("C-x o" . ace-window))
  :init
  (setq aw-keys '(?h ?j ?k ?l ?a ?s ?d ?f)))

;; Pin your eyes to the centre of the screen (except when you don't)
(use-package centered-cursor-mode
  :ensure t
  :diminish centered-cursor-mode
  :config
  (define-global-minor-mode my-global-centered-cursor-mode centered-cursor-mode
    (lambda ()
      (when (not (memq major-mode (list 'eshell-mode
                                        'magit-mode
                                        'org-mode)))
        (centered-cursor-mode))))
  (my-global-centered-cursor-mode 1))

;; Some deserve to be highlighted more than others
(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1))

;; Text-only web browsing because that's how we roll
(use-package w3m
  :ensure t
  :defer t
  :bind (:map w3m-mode-map
              ("n" . next-line)
              ("p" . previous-line))
  :init
  (setq w3m-command "w3m"))

(use-package password-generator
  :ensure t)

;; Add JS support
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :init
  (setq-default js2-basic-offset 2))

(use-package tern
  :ensure t
  :after js2-mode)

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode)
  :init
  (setq-default js-indent-level 2))

;; Add Go support
(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :after counsel
  :defer t
  :bind (:map go-mode-map
              ("C-c h h" . godoc-at-point)
              ("M-." . godef-jump)
              ("M-," . pop-tag-mark)
              ("C-c C-j" . counsel-imenu))
  :init
  ;; Use `goimports' instead of `gofmt'
  (setq gofmt-command "goimports")
  ;; Use 2 spaces for tab instead of 8
  (add-hook 'go-mode-hook (lambda () (setq-local tab-width 2)))
  :config
  ;; Enable autoformat
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package go-rename
  :ensure t
  :after go-mode
  :defer t
  :bind (:map go-mode-map
              ("C-c r v" . go-rename)))

(use-package go-guru
  :ensure t
  :after go-mode
  :defer t
  :bind (:map go-mode-map
              ("C-c a d" . go-guru-describe)
              ("C-c a f" . go-guru-freevars)
              ("C-c a i" . go-guru-implements)
              ("C-c a c" . go-guru-peers)
              ("C-c a r" . go-guru-referrers)
              ("C-c a j" . go-guru-definition)
              ("C-c a p" . go-guru-pointsto)
              ("C-c a s" . go-guru-callstack)
              ("C-c a e" . go-guru-whicherrs)
              ("C-c a <" . go-guru-callers)
              ("C-c a >" . go-guru-callees)
              ("C-c a o" . go-guru-set-scope)))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :after counsel
  :bind (:map python-mode-map
              ("C-c C-j" . counsel-imenu))
  :init
  (setq python-shell-interpreter "ipython"
        python-shell-virtualenv-path "~/pyvenv"
        python-shell-virtualenv-root "~/pyvenv"
        python-shell-completion-native-disabled-interpreters '("ipython" "pypy")
        fci-rule-column 99)
  :config
  (add-hook 'python-mode-hook #'fci-mode))

(use-package company-jedi
  :ensure t
  :after python
  :bind (:map python-mode-map
              ("M-." . jedi:goto-definition)
              ("M-," . jedi:goto-definition-pop-marker))
  :init
  (setq jedi:complete-on-dot t)
  :config
  (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi))))

(use-package yapfify
  :ensure t
  :after python)

(use-package treemacs
  :ensure t)

(use-package sql
  :mode ("\\.sql\\'" . sql-mode))

(use-package sqlup-mode
  :ensure
  :after sql
  :config
  (add-hook 'sql-mode-hook #'sqlup-mode))

;; Search elastically
(use-package es-mode
  :ensure t
  :mode ("\\.es\\'" . es-mode)
  :bind (("C-c C-f" . json-pretty-print)))

(use-package undo-tree
  :ensure t
  :defer t
  :diminish undo-tree-mode
  :bind (("C-x u" . undo-tree-visualize))
  :init
  (setq undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode)
  :config
  ;; NOTE: `undo-tree-visualizer-diff' is disabled on exit for some reason, turn it back on!
  (advice-add 'undo-tree-visualizer-quit :after (lambda () (setq undo-tree-visualizer-diff t))))

;; Magit is magical
(use-package magit
  :ensure t
  :bind (("C-c m b" . magit-blame)
         ("C-c m l" . magit-log-current)
         ("C-c m m" . magit-show-refs-head)
         ("C-c m s" . magit-status))
  :init
  (setq magit-diff-refine-hunk 'all)
  (advice-add 'magit-show-refs-head :after #'delete-other-windows)
  (advice-add 'magit-status :after #'delete-other-windows))

(use-package git-timemachine
  :ensure t
  :defer t
  :bind (("C-c m t" . git-timemachine)))

(use-package vc-git
  :defer t
  :bind (("C-c m a" . vc-annotate)))

(use-package browse-at-remote
  :ensure t
  :defer t
  :bind (("C-c m w" . browse-at-remote)))

;; Smart parentheses are smart
(use-package smartparens
  :ensure t
  :defer t
  :diminish smartparens-mode
  :bind (("C-k" . sp-kill-hybrid-sexp)
         :map smartparens-mode-map
         ("<C-M-backspace>" . sp-splice-sexp)
         ("C-M-]" . sp-select-next-thing)
         ("C-M-}" . sp-select-previous-thing)
         ("C-M-a" . sp-backward-down-sexp)
         ("C-M-b" . sp-backward-sexp)
         ("C-M-d" . sp-down-sexp)
         ("C-M-e" . sp-up-sexp)
         ("C-M-f" . sp-forward-sexp)
         ("C-M-n" . sp-next-sexp)
         ("C-M-p" . sp-previous-sexp)
         ("C-M-u" . sp-backward-up-sexp)
         ("C-]" . sp-select-next-thing-exchange))
  :init
  ;; Use it everywhere
  (smartparens-global-mode 1)
  :config
  ;; Use default config
  (use-package smartparens-config)
  ;; Apply strict mode to all Lisp modes
  (mapc (lambda (mode)
          (add-hook (intern (format "%s-hook" (symbol-name mode)))
                    'smartparens-strict-mode))
        sp-lisp-modes))

;; Dumb jump is smart too!
(use-package dumb-jump
  :ensure t
  :defer t
  :bind (("C-c C-." . dumb-jump-go)
         ("C-c C-," . dumb-jump-back))
  :init
  (setq-default dumb-jump-selector 'ivy
                dumb-jump-prefer-search 'rg))

;; Use `browse-kill-ring' instead of `counsel-yank-pop'
(use-package browse-kill-ring
  :ensure t
  :defer t
  :bind (("C-c r y" . browse-kill-ring)
         :map browse-kill-ring-mode-map
         ("C-g" . browse-kill-ring-quit))
  :init
  (setq browse-kill-ring-highlight-current-entry t
        browse-kill-ring-highlight-inserted-item t
        browse-kill-ring-display-duplicates nil
        browse-kill-ring-resize-window '(25 . 25)
        browse-kill-ring-show-preview nil))

;; Make M-x smart again
(use-package smex
  :ensure t
  :init
  (smex-initialize))

(use-package avy
  :ensure t
  :defer t
  :bind (("C-'" . avy-goto-word-1))
  :config
  (avy-setup-default))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :defer t
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-c r r" . ivy-resume))
  :init
  (setq enable-recursive-minibuffers t
        ivy-display-style 'fancy
        ivy-fixed-height-minibuffer t
        ivy-height 25
        ivy-use-virtual-buffers t
        ivy-wrap t)
  :config
  (ivy-mode 1))

(use-package ivy-hydra
  :ensure t)

(use-package counsel
  :ensure t
  :defer t
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h M-f" . counsel-describe-face)
         ("C-x r b" . counsel-bookmark)
         ("C-c C-j" . counsel-imenu)
         ("C-x M-t" . counsel-load-theme)))

(use-package swiper
  :ensure t
  :defer t
  :bind (("C-s" . swiper))
  :after ivy
  :config
  (defun swiper-at-point ()
    (interactive)
    (swiper (thing-at-point 'symbol))))

(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-S-c C-S-a" . mc/mark-all-like-this)))

;; Project management at its best
(use-package projectile
  :ensure t
  :after magit
  :diminish projectile-mode
  :init
  (setq-default projectile-switch-project-action 'magit-show-refs-head
                projectile-completion-system 'ivy)
  :config
  (projectile-mode))

;; When Counsel meets Projectile (NOTE: Requires `rg')
(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :defer t
  :bind (("C-c p p" . counsel-projectile-switch-project)
         ("C-c p d" . counsel-projectile-find-dir)
         ("C-c p f" . counsel-projectile-find-file)
         ("C-c p s s" . counsel-projectile-ag)
         :map projectile-mode-map
         ([remap projectile-ag] . counsel-projectile-rg))
  :config
  (counsel-projectile-on))

;;; init.el ends here
