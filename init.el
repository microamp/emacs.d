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
  (setq-default mac-command-modifier 'meta
                mac-right-command-modifier 'super))

;; Set up some custom key bindings
(global-set-key (kbd "C-x k") #'kill-this-buffer)

;; Quickly switch between two most recently visited buffers
(global-set-key (kbd "C-'") #'mode-line-other-buffer)

;; Allow 30MB before GC kicking in
(setq gc-cons-threshold 30000000)

;; Set scroll margin at top/bottom
(setq scroll-margin 0)

;; You don't close Emacs, Emacs closes you
(setq confirm-kill-emacs 'y-or-n-p)

;; Follow symlinks without asking
(setq vc-follow-symlinks t)

;; No startup messages please
(setq inhibit-splash-screen t
      inhibit-startup-message t)

(setq use-file-dialog nil)

;; I need space, so do you
(set-frame-parameter nil 'internal-border-width 3)

(use-package zoom-window
  :ensure t
  :defer t
  :bind (("C-x l" . zoom-window-zoom)
         ("C-x j" . zoom-window-next))
  :init
  (setq-default zoom-window-mode-line-color "honeydew1"))

(global-set-key (kbd "C-x L") #'delete-other-windows)
(global-set-key (kbd "C-x q") #'delete-window)

(global-set-key (kbd "C-x \\") (lambda () (interactive) (split-window-horizontally) (other-window 1)))
(global-set-key (kbd "C-x -") (lambda () (interactive) (split-window-vertically) (other-window 1)))

;; Stop cluttering with backup files
(setq backup-directory-alist '((".*" . "~/.emacs.d/.tmp")))

;; Default web browser
(setq browse-url-browser-function 'browse-url-chromium)

;; Tabs are evil
(setq-default indent-tabs-mode nil)

;; No alarm bells
(setq ring-bell-function 'ignore)

;; Split windows vertically by default
;;(setq split-width-threshold nil)

;; Select *help* buffer automatically
(setq help-window-select t)

;; Hide things you don't want to see
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))

;; Set default font
;;(set-frame-font "DejaVu Sans Mono-10" nil t)
;;(set-frame-font "Office Code Pro Light-10" nil t)
(set-frame-font "Source Code Pro-10" nil t)

;; I am from Korea, but not from North
(set-fontset-font "fontset-default" 'hangul (font-spec :family "Source Han Sans KR" :size 15))

;; Prefer y/n to yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Die whitespaces, die!
(add-to-list 'write-file-functions #'delete-trailing-whitespace)

;; Fullscreen after init
;;(add-hook 'after-init-hook #'toggle-frame-fullscreen)

;; Record how much it took to load Emacs and be ashamed
(add-hook 'after-init-hook (lambda () (message (format "Init time: %s" (emacs-init-time)))))

;; Show row and column numbers
;;(column-number-mode 1)

;; Load theme
(use-package shinmunji-theme
  :load-path "themes/shinmunji-theme"
  :config
  (load-theme 'shinmunji t))

(use-package view
  :bind (("C-x C-r" . view-mode)
         :map view-mode-map
         ("n" . next-line)
         ("p" . previous-line)))

;; Highlight matching parens
(use-package paren
  :init
  (show-paren-mode 1))

;; No ugly compilation buffer please
(use-package ansi-color
  :config
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

;; "How can I replace highlighted text with what I type?" (Emacs FAQ)
(use-package delsel
  :config
  (delete-selection-mode 1))

;; Refresh buffer when changed on disk
(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode 1))

(use-package recentf
  :init
  (setq-default recentf-max-menu-items 100
                recentf-max-saved-items 100))

(use-package compile
  :init
  (setq compilation-ask-about-save nil
        compilation-auto-jump-to-first-error t
        compilation-skip-threshold 2))

;; Try packages without installing them
(use-package try
  :ensure t)

(use-package diminish
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode)

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "GOBIN"))

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
  (setq-default company-echo-delay 0
                company-idle-delay 0.2
                company-minimum-prefix-length 1)
  :config
  (add-hook 'prog-mode-hook #'company-mode))

(use-package company-quickhelp
  :ensure t
  :hook (company-mode . company-quickhelp-mode))

;; Enable syntax highlighting in programming modes
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (setq-default flycheck-idle-change-delay 0.5
                flycheck-check-syntax-automatically '(mode-enabled save))
  :config
  (add-hook 'prog-mode-hook #'flycheck-mode))

(use-package flymake
  :diminish flymake-mode
  :config
  (defhydra hydra-flymake-error (global-map "C-c ! !")
    "Flymake errors"
    ("n" flymake-goto-next-error "next error")
    ("p" flymake-goto-prev-error "previous error")))

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
        olivetti-body-width 0.4))

(use-package elfeed
  :disabled
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
  (setq-default elfeed-feeds '("http://nullprogram.com/feed/"
                               "http://irreal.org/blog/?feed=rss2")))

;; Simple workspace management
(use-package eyebrowse
  :ensure t
  :defer t
  :bind (("C-c w ." . eyebrowse-switch-to-window-config)
         ("C-c w ," . eyebrowse-rename-window-config)
         ("C-c w DEL" . eyebrowse-close-window-config)
         ("C-c w k" . eyebrowse-close-window-config)
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
  :init
  (setq-default eshell-buffer-maximum-lines 5000
                eshell-history-size 200
                eshell-hist-ignoredups t)
  (defun eshell-cwd ()
    (interactive)
    (if (string= (buffer-name) "*eshell*")
        (bury-buffer)
      (if (get-buffer "*eshell*")
          (let ((path (file-name-directory (or (buffer-file-name) default-directory))))
            (with-current-buffer "*eshell*"
              (cd path)
              (eshell-send-input)
              (switch-to-buffer "*eshell*")))
        (eshell))))
  (global-set-key (kbd "M-SPC") #'eshell-cwd)
  :config
  (defun eshell-clear ()
    ;; NOTE: This erases shell content!
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (eshell-send-input))
  (defun eshell-init ()
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
         ("C-c ;" . nil)
         ("C-'" . mode-line-other-buffer)
         ("C-c C-;" . org-todo)
         ("C-c a" . org-agenda)
         ("M-H" . org-metaleft)
         ("M-J" . org-metadown)
         ("M-K" . org-metaup)
         ("M-L" . org-metaright)
         ("M-n" . org-next-visible-heading)
         ("M-p" . org-previous-visible-heading))
  :init
  (setq-default org-agenda-files '("~/.emacs.d/notes/todo.org"
                                   "~/.emacs.d/notes/todo-personal.org")
                org-agenda-start-with-follow-mode t
                org-clock-into-drawer t
                org-clock-persist 'history
                org-log-done 'time
                org-log-into-drawer t
                org-startup-truncated nil)
  (org-babel-do-load-languages
   'org-babel-load-languages '((dot . t)
                               (emacs-lisp . t)
                               (ipython . t)
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
  (add-hook 'org-mode-hook (lambda () (setq-local auto-save-timeout nil)))
  (add-hook 'org-mode-hook
            (lambda () (setq org-structure-template-alist
                             '(("n" . "notes")
                               ("a" . "export ascii")
                               ("c" . "center")
                               ("C" . "comment")
                               ("e" . "example")
                               ("E" . "export")
                               ("h" . "export html")
                               ("l" . "export latex")
                               ("q" . "quote")
                               ("s" . "src")
                               ("v" . "verse"))))))

(use-package ob-core
  :init
  (setq org-confirm-babel-evaluate nil))

(use-package ob-http
  :ensure t
  :after org
  :init
  (add-to-list 'org-babel-load-languages '(http . t)))

(use-package ox-gfm
  :ensure t)

(use-package ox-jira
  :ensure t
  :after org)

(use-package ox-confluence
  :ensure org-plus-contrib
  :after org)

;; Prettify the bullets
(use-package org-bullets
  :ensure t
  :hook
  (org-mode . org-bullets-mode))

;; Prettify the priorities
(use-package org-fancy-priorities
  :ensure t
  :diminish org-fancy-priorities-mode
  :hook
  (org-mode . org-fancy-priorities-mode)
  :init
  (setq-default org-fancy-priorities-list '("⬆" "⬇")))

(use-package org-radiobutton
  :ensure t
  :diminish org-radiobutton-mode
  :hook
  (org-mode . org-radiobutton-mode))

(use-package org-src
  :diminish org-src-mode)

(use-package deft
  :disabled
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

(use-package eglot
  :ensure t
  :hook ((python-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (cc-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pyls")))
  (add-to-list 'eglot-server-programs '(rust-mode . ("rls")))
  (add-to-list 'eglot-server-programs '(cc-mode . ("ccls")))
  (add-to-list 'eglot-server-programs '(js2-mode . ("javascript-typescript-stdio")))
  (add-to-list 'eglot-server-programs '(typescript-mode . ("javascript-typescript-stdio")))
  (add-to-list 'eglot-server-programs '(javascript-mode . ("javascript-typescript-stdio"))))

;; Terraform makes you suck less at ops
(use-package terraform-mode
  :ensure t
  :init
  (setq terraform-indent-level 2))

(use-package company-terraform
  :ensure t
  :config
  (company-terraform-init))

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode))

;; NOTE: Requires `pandoc'
(use-package pandoc-mode
  :ensure t)

;; NOTE: Requires `pandoc'
(use-package ox-pandoc
  :ensure t
  :after pandoc-mode)

(use-package ox-reveal
  :ensure t
  :after org)

(use-package htmlize
  :ensure t)

(use-package graphviz-dot-mode
  :ensure t
  :mode ("\\.dot\\'" . graphviz-dot-mode))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

;; NOTE: Requires `vmd' (Not supported under GNU/Linux, use `markdown-preview' instead)
(use-package vmd-mode
  :disabled
  :ensure t
  :after markdown-mode
  :bind (:map markdown-mode-map
         ("C-c C-v" . vmd-mode)))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :defer t
  :bind (:map nov-mode-map
         ("n" . next-line)
         ("p" . previous-line)
         ("M-n" . nov-next-document)
         ("M-p" . nov-previous-document))
  :init
  (setq nov-variable-pitch nil))

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
  (setq highlight-thing-delay-seconds 0.2
        highlight-thing-limit-to-defun t
        highlight-thing-case-sensitive-p t)
  (add-hook 'prog-mode-hook #'highlight-thing-mode))

(use-package focus
  :disabled
  :ensure t
  :hook
  (prog-mode . focus-mode))

;; Sometimes the best fix is turning it off and on again
(use-package restart-emacs
  :ensure t
  :defer t
  :bind (("C-x M-r" . restart-emacs))
  :init
  (setq restart-emacs-restore-frames t))

(use-package mwim
  :ensure t
  :defer t
  :bind (("C-a" . mwim-beginning-of-code-or-line)))

;; Win at windowing
(use-package ace-window
  :ensure t
  :defer t
  :bind (("C-x O" . ace-window))
  :init
  (setq aw-keys '(?h ?j ?k ?l ?a ?s ?d ?f)))

;; Pin your eyes to the centre of the screen (except when you don't)
(use-package centered-cursor-mode
  :disabled
  :ensure t
  :diminish centered-cursor-mode
  :init
  (setq ccm-recenter-at-end-of-file t)
  :config
  (define-global-minor-mode my-global-centered-cursor-mode centered-cursor-mode
    (lambda ()
      (when (not (memq major-mode (list 'compilation-mode
                                        'dired-mode
                                        'ein:notebook-multilang-mode
                                        'eshell-mode
                                        'eww-mode
                                        'inferior-python-mode
                                        'inferior-sml-mode
                                        'magit-mode
                                        'messages-buffer-mode
                                        'org-mode
                                        'sql-interactive-mode
                                        'w3m-mode)))
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

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode)

;; Add JS support
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :defer t
  :bind (:map js2-mode-map
         ("C-c C-j" . counsel-semantic-or-imenu)
         ("M-." . xref-find-definitions)
         ("C-c C-c C-d" . eglot-help-at-point))
  :init
  (setq-default js2-basic-offset 2)
  :config
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))

(use-package company-tern
  :disabled
  :ensure t
  :diminish term-mode
  :after js2-mode
  :init
  (add-hook 'js2-mode-hook (lambda () (add-to-list 'company-backend 'company-tern)))
  :config
  (add-hook 'js2-mode-hook #'tern-mode))

(use-package js2-refactor
  :ensure t
  :diminish js2-refactor-mode
  :after js2-mode
  :defer t
  :bind (:map js2-mode-map
         ("C-k" . js2r-kill))
  :init
  (js2r-add-keybindings-with-prefix "C-c C-r")
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode))

(use-package xref-js2
  :disabled
  :ensure t
  :after js2-mode
  :defer t
  :bind (:map js-mode-map
         ("M-." . nil))
  :config
  (add-hook 'js2-mode-hook (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package json-mode
  :ensure t
  :mode ("\\.[json|tpl]\\'" . json-mode)
  :init
  (setq-default js-indent-level 2))

(use-package nodejs-repl
  :ensure t
  :after js2-mode
  :defer t
  :bind (:map js2-mode-map
         ("C-c C-p" . nodejs-repl)))

(use-package prettier-js
  :ensure t
  :diminish prettier-js-mode
  :after js2-mode
  :config
  (add-hook 'js2-mode-hook #'prettier-js-mode))

(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" . typescript-mode))

(use-package tide
  :ensure t
  :diminish tide-mode
  :after typescript-mode
  :config
  (add-hook 'typescript-mode-hook #'tide-setup))

;; Add Go support
(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :defer t
  :bind (:map go-mode-map
         ("C-c h h" . godoc-at-point)
         ("M-." . godef-jump)
         ("M-," . pop-tag-mark)
         ("C-c C-j" . counsel-semantic-or-imenu)
         ("C-c C-d" . godoc-at-point))
  :init
  ;; Use `goimports' instead of `gofmt'
  (setq gofmt-command "goimports")
  ;; Use 2 spaces for tab instead of 8
  (add-hook 'go-mode-hook (lambda () (setq-local tab-width 2)))
  :config
  ;; Enable autocomplete
  (add-hook 'go-mode-hook (lambda () (add-to-list 'company-backends 'company-go)))
  ;; Enable autoformat
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup)
  :init
  ;; Run fast linters only, otherwise zzz...
  (setq flycheck-golangci-lint-fast t))

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
         ("C-c o d" . go-guru-describe)
         ("C-c o f" . go-guru-freevars)
         ("C-c o i" . go-guru-implements)
         ("C-c o c" . go-guru-peers)
         ("C-c o r" . go-guru-referrers)
         ("C-c o j" . go-guru-definition)
         ("C-c o p" . go-guru-pointsto)
         ("C-c o s" . go-guru-callstack)
         ("C-c o e" . go-guru-whicherrs)
         ("C-c o <" . go-guru-callers)
         ("C-c o >" . go-guru-callees)
         ("C-c o o" . go-guru-set-scope)))

(use-package godoctor
  :ensure t
  :after go-mode
  :bind (:map go-mode-map
         ("C-c d e" . godoctor-extract)
         ("C-c d d" . godoctor-godoc)
         ("C-c d r" . godoctor-rename)
         ("C-c d t" . godoctor-toggle)))

(use-package ob-go
  :ensure t
  :after org
  :init
  (add-to-list 'org-babel-load-languages '(go . t)))

(use-package docker
  :ensure t
  :bind ("C-c k" . docker))

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; "Have you tried Rust?"
(use-package rust-mode
  :ensure t
  :interpreter
  ("rust" . rust-mode)
  :init
  (setq rust-format-on-save t))

(use-package flycheck-rust
  :ensure t
  :after rust-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package cargo
  :ensure t
  :diminish cargo-minor-mode
  :hook (rust-mode . cargo-minor-mode))

(use-package racer
  :ensure t
  :diminish racer-mode
  :hook (rust-mode . racer-mode)
  :bind (:map rust-mode-map
         ("<Tab>" . company-indent-or-complete-common)
         ("C-c C-d" . racer-describe))
  :init
  (setq company-tooltip-align-annotations t)
  :config
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package company-racer
  :ensure t
  :init
  (add-hook 'rust-mode-hook (lambda () (add-to-list 'company-backends 'company-racer))))

;; Standard ML, the father of all MLs
(use-package sml-mode
  :ensure t
  :bind (:map sml-mode-map
         ("C-c s a" . sml-form-abstype)
         ("C-c s c" . sml-form-case)
         ("C-c s d" . sml-form-datatype)
         ("C-c s f f" . sml-form-fn)
         ("C-c s f F" . sml-form-fun)
         ("C-c s f M-f" . sml-form-functor)
         ("C-c s i" . sml-form-if)
         ("C-c s l l" . sml-form-let)
         ("C-c s l c" . sml-form-local)
         ("C-c s s i s" . sml-form-sig)
         ("C-c s s i S" . sml-form-signature)
         ("C-c s s t s" . sml-form-struct)
         ("C-c s s t S" . sml-form-structure)
         ("C-c s v" . sml-form-val)
         :map inferior-sml-mode-map
         ("C-l" . comint-clear-buffer)))

(use-package ob-sml
  :ensure t)

;; How safe is your type safety
(use-package ensime
  :ensure t
  :pin melpa
  :bind (:map ensime-mode-map
         ("C-c C-z" . sbt-switch-to-active-sbt-buffer))
  :init
  (setq-default ensime-search-interface 'ivy
                ensime-eldoc-hints 'all)
  :config
  (add-hook 'ensime-mode-hook #'eldoc-mode))

(use-package sbt-mode
  :pin melpa
  :commands sbt-start sbt-command
  :config
  ;; NOTE: Workaround (https://github.com/ensime/emacs-sbt-mode/issues/31)
  (substitute-key-definition 'minibuffer-complete-word
                             'self-insert-command
                             minibuffer-local-completion-map))

(use-package scala-mode
  :pin melpa
  :interpreter
  ("scala" . scala-mode)
  :bind (:map scala-mode-map
         ("M-[" . scala-syntax:beginning-of-definition)
         ("M-]" . scala-syntax:end-of-definition)
         ("C-c C-b C-b" . sbt-command)
         ("C-c C-v C-z" . ensime-inf-switch)
         ("C-c C-k" . ensime-inf-eval-buffer)
         ("C-M-x" . ensime-inf-eval-region)))

(use-package ob-scala
  :after org
  :init
  (add-to-list 'org-babel-load-languages '(scala . t)))

(use-package cider
  :ensure t
  :bind (:map clojure-mode-map
         ("C-c C-s" . cider-scratch)
         ("C-c M-f" . cider-format-buffer)
         ("C-S-q" . prog-indent-sexp))
  :init
  (setq cider-prompt-for-symbol nil
        cider-repl-pop-to-buffer-on-connect 'display-only
        cider-repl-result-prefix ""
        cider-repl-use-pretty-printing t)
  ;; Enable autocomplete
  (add-hook 'cider-mode-hook #'company-mode)
  ;; Enable fuzzy searching
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  ;; Enable eldoc
  (add-hook 'cider-mode-hook #'eldoc-mode))

(use-package cider-repl
  :bind (:map cider-repl-mode-map
         ("C-l" . cider-repl-clear-buffer)
         ("RET" . cider-repl-newline-and-indent)
         ("C-<return>" . cider-repl-return))
  :init
  ;; Enable autocomplete
  (add-hook 'cider-repl-mode-hook #'company-mode)
  ;; Enable fuzzy searching
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  ;; Enable eldoc
  (add-hook 'cider-repl-mode-hook #'eldoc-mode))

(use-package clj-refactor
  :ensure t
  :diminish clj-refactor-mode
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package clojure-snippets
  :ensure t
  :hook (clojure-mode . yas-minor-mode))

(use-package kibit-helper
  :ensure t
  :bind (("C-x C-`" . kibit-accept-proposed-change)))

(use-package groovy-mode
  :ensure t
  :interpreter
  ("groovy" . groovy-mode)
  :init
  (setq groovy-indent-offset 2))

(use-package vlf
  :ensure t
  :pin melpa
  :init
  (setq vlf-batch-size 100000))

(use-package d-mode
  :ensure t)

(use-package company-dcd
  :disabled
  :ensure t
  :after d-mode
  :config
  (add-hook 'd-mode-hook #'company-dcd-mode))

(use-package dfmt
  :ensure t
  :after d-mode
  :defer t
  :bind (:map d-mode-map
              ("C-x C-s" . dfmt-save-buffer))
  :init
  (setq dfmt-flags '("--brace_style=stroustrup"
                     "--indent_size=2")))

(use-package ob-D
  :disabled
  :after org
  :init
  (add-to-list 'org-babel-load-languages '(D . t)))

(use-package fill-column-indicator
  :disabled
  :ensure t
  :init
  (setq fci-rule-column 99)
  (add-hook 'python-mode-hook #'fci-mode))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :defer t
  :bind (:map python-mode-map
         ("C-c C-j" . counsel-semantic-or-imenu)
         ("M-[" . python-nav-backward-defun)
         ("M-]" . python-nav-forward-defun)
         :map inferior-python-mode-map
         ("C-l" . comint-clear-buffer))
  :init
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i --pprint --nosep --no-banner --classic --no-confirm-exit"
        python-shell-virtualenv-root "~/.venv"
        python-shell-completion-native-disabled-interpreters '("ipython" "pypy"))
  :config
  (add-hook 'python-mode-hook (lambda () (flycheck-mode -1))))

(use-package company-jedi
  :disabled
  :ensure t
  :after python
  :defer t
  :bind (:map python-mode-map
         ("M-." . jedi:goto-definition)
         ("M-," . jedi:goto-definition-pop-marker)
         ("C-c C-d" . jedi:show-doc)
         :map comint-mode-map
         ("C-l" . comint-clear-buffer))
  :init
  (setq jedi:complete-on-dot t)
  (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi))))

(use-package company-anaconda
  :disabled
  :ensure t
  :diminish anaconda-mode
  :after python
  :defer t
  :bind (:map anaconda-mode-map
         ("M-." . anaconda-mode-find-assignments)
         ("M-," . anaconda-mode-go-back))
  :config
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook #'anaconda-eldoc-mode)
  (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-anaconda))))

(use-package pyenv-mode
  :disabled
  :ensure t
  :after python
  :config
  :hook (python-mode-hook . pyenv-mode))

(use-package pyvenv
  :ensure t
  :after python
  :bind (:map python-mode-map
         ("C-c v a" . pyvenv-activate)
         ("C-c v d" . pyvenv-deactivate))
  :hook (python-mode-hook . pyvenv-mode))

(use-package yapfify
  :diminish yapf-mode
  :hook (python-mode . yapf-mode))

(use-package ein
  :ensure t
  :bind (("C-c I l" . ein:notebooklist-login)
         ("C-c I o" . ein:notebooklist-open))
  :config
  (advice-add 'ein:edit-cell-save :before #'yapfify-buffer))

(use-package ob-ipython
  :ensure t
  :diminish ob-ipython-mode
  :defer t
  :bind (:map ob-ipython-mode-map
              ("C-c i" . ob-ipython-inspect)
              ("C-M-." . company-ob-ipython))
  :config
  (add-hook 'ob-ipython-mode (lambda () (add-to-list 'company-backends 'company-ob-ipython))))

(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-mode))

(use-package meghanada
  :disabled
  :ensure t)

(use-package csv-mode
  :ensure t
  :mode ("\\.csv\\'" . csv-mode)
  :bind (("C-c C-h" . csv-header-line)))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode))

(use-package treemacs
  :disabled
  :ensure t)

(use-package know-your-http-well
  :ensure t)

(use-package sql
  :mode ("\\.sql\\'" . sql-mode)
  :init
  (add-hook 'sql-interactive-mode-hook #'smartparens-mode))

(use-package sqlup-mode
  :ensure t
  :diminish sqlup-mode
  :init
  (add-hook 'sql-mode-hook #'sqlup-mode)
  (add-hook 'sql-interactive-mode-hook #'sqlup-mode))

(use-package inf-mongo
  :ensure t
  :bind (:map inf-mongo-mode-map
         ("C-l" . comint-clear-buffer))
  :init
  (setq inf-mongo-command "mongo --host 127.0.0.1"))

;; Search elastically
(use-package es-mode
  :ensure t
  :mode ("\\.es\\'" . es-mode)
  :bind (("C-c C-f" . json-pretty-print)))

(use-package ob-elasticsearch
  :after (org es-mode)
  :init
  (add-to-list 'org-babel-load-languages '(elasticsearch . t)))

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
  :bind (("C-c m b" . magit-blame-addition)
         ("C-c m l" . magit-log-current)
         ("C-c m L" . magit-log-buffer-file)
         ("C-c m m" . magit-show-refs-head)
         ("C-c m s" . magit-status))
  :init
  (setq-default magit-diff-refine-hunk 'all
                projectile-switch-project-action 'magit-show-refs-head)
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

(use-package git-gutter
  :disabled
  :ensure t
  :bind (("C-c m g" . git-gutter-mode))
  :init
  (setq git-gutter:always-show-separator t
        git-gutter:window-width 1)
  (global-git-gutter-mode 1))

(use-package edit-server
  :ensure t)

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
         ("C-]" . sp-select-next-thing-exchange)
         ("C-)" . sp-forward-slurp-sexp)
         ("C-M-)" . sp-forward-barf-sexp)
         ("C-}" . sp-forward-barf-sexp)
         ("C-(" . sp-backward-slurp-sexp)
         ("C-M-(" . sp-backward-barf-sexp)
         ("C-{" . sp-backward-barf-sexp))
  :init
  ;; Use it everywhere
  (smartparens-global-mode 1)
  (show-smartparens-mode 1)
  :config
  ;; Use default config
  (use-package smartparens-config)
  ;; Apply strict mode to all Lisp modes
  (mapc (lambda (mode)
          (add-hook (intern (format "%s-hook" (symbol-name mode)))
                    'smartparens-strict-mode))
        sp-lisp-modes))

(use-package which-func
  :disabled
  :init
  (setq which-func-modes '(clojure-mode
                           go-mode
                           python-mode
                           scala-mode))
  :config
  (which-function-mode 1))

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
  :bind (("C-:" . avy-goto-word-1))
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
        ivy-height 20
        ivy-use-virtual-buffers t
        ivy-wrap t)
  :config
  (ivy-mode 1))

(use-package ivy-hydra
  :ensure t
  :config
  (define-key ivy-minibuffer-map "\C-o"
    (defhydra soo-ivy (:hint nil :color pink)
      "
 Move     ^^^^^^^^^^ | Call         ^^^^ | Cancel^^ | Options^^ | Action _w_/_s_/_a_: %s(ivy-action-name)
----------^^^^^^^^^^-+--------------^^^^-+-------^^-+--------^^-+---------------------------------
 _g_ ^ ^ _p_ ^ ^ _u_ | _f_orward _o_ccur | _i_nsert | _c_alling: %-7s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
 ^↨^ _h_ ^+^ _l_ ^↕^ | _RET_ done     ^^ | _q_uit   | _m_atcher: %-7s(ivy--matcher-desc) _t_runcate: %-11`truncate-lines
 _G_ ^ ^ _n_ ^ ^ _d_ | _TAB_ alt-done ^^ | ^ ^      | _<_/_>_: shrink/grow
"
      ;; arrows
      ("n" ivy-next-line)
      ("p" ivy-previous-line)
      ("l" ivy-alt-done)
      ("h" ivy-backward-delete-char)
      ("g" ivy-beginning-of-buffer)
      ("G" ivy-end-of-buffer)
      ("d" ivy-scroll-up-command)
      ("u" ivy-scroll-down-command)
      ("e" ivy-scroll-down-command)
      ;; actions
      ("q" keyboard-escape-quit :exit t)
      ("C-g" keyboard-escape-quit :exit t)
      ("<escape>" keyboard-escape-quit :exit t)
      ("C-o" nil)
      ("i" nil)
      ("TAB" ivy-alt-done :exit nil)
      ("C-j" ivy-alt-done :exit nil)
      ;; ("d" ivy-done :exit t)
      ("RET" ivy-done :exit t)
      ("C-m" ivy-done :exit t)
      ("f" ivy-call)
      ("c" ivy-toggle-calling)
      ("m" ivy-toggle-fuzzy)
      (">" ivy-minibuffer-grow)
      ("<" ivy-minibuffer-shrink)
      ("w" ivy-prev-action)
      ("s" ivy-next-action)
      ("a" ivy-read-action)
      ("t" (setq truncate-lines (not truncate-lines)))
      ("C" ivy-toggle-case-fold)
      ("o" ivy-occur :exit t))))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :defer t
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h M-f" . counsel-describe-face)
         ("C-x r b" . counsel-bookmark)
         ("C-c C-j" . counsel-semantic-or-imenu)
         ("C-x M-t" . counsel-load-theme))
  :init
  (counsel-mode))

;; Be helpful (Override counsel key bindings where possible)
(use-package helpful
  :ensure t
  :after counsel
  :bind (("C-h f" . helpful-callable)
         ("C-h F" . helpful-function)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-c C" . helpful-command)))

(use-package elisp-mode
  :bind ("C-c C-k" . eval-buffer))

(use-package elisp-def
  :ensure t
  :bind (:map emacs-lisp-mode-map
         ("M-." . elisp-def))
  :hook ((emacs-lisp-mode-hook . elisp-def-mode)
         (ielm-mode-hook . elisp-def-mode)))

(use-package swiper
  :ensure t
  :defer t
  :bind (("C-s" . swiper))
  :after ivy
  :config
  (defun swiper-at-point ()
    (interactive)
    (swiper (thing-at-point 'symbol))))

(use-package ivy-lobsters
  :ensure t)

(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode)
  :bind (([remap restclient-http-send-current] . restclient-http-send-current-stay-in-window)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-S-c C-S-a" . mc/mark-all-like-this)))

;; Project management at its best
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq-default projectile-completion-system 'ivy
                projectile-keymap-prefix (kbd "C-c p")
                projectile-project-compilation-cmd ""
                projectile-project-run-cmd ""
                projectile-project-test-cmd "")
  :config
  (projectile-mode))

;; When Counsel meets Projectile (NOTE: Requires `rg')
(use-package counsel-projectile
  :ensure t
  :defer t
  :bind (:map projectile-mode-map
         ("C-c p s s" . counsel-projectile-rg))
  :init
  ;; NOTE: Only counsel-projectile-rg command is needed from this package, do not override projectile key bindings
  (setq-default counsel-projectile-mode nil))

(use-package zeal-at-point
  :ensure t
  :init
  (global-set-key (kbd "C-h M-d") #'zeal-at-point))

(use-package counsel-dash
  :disabled
  :ensure t)

(use-package xkcd
  :ensure t
  :bind (:map xkcd-mode-map
         ("R" . xkcd-rand)))

;; Zone out!
(use-package zone-nyan
  :ensure t
  :after zone
  :init
  (setq-default zone-programs '(zone-nyan))
  :config
  (zone-when-idle (* 60 10)))

;;; init.el ends here
