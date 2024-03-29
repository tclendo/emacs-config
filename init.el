(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(use-package straight
  :custom (straight-use-package-by-default t))

;; Set Emacs screen bounds when not on terminal
(if (display-graphic-p)
    (progn
      (set-face-attribute 'default nil :height 160)
      (setq initial-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 90) ; chars
              (height . 40) ; lines
              (fullscreen . maximized)
	      ))
      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 90)
              (height . 40)
              (fullscreen . maximized)
	      )))
  (progn
    (setq initial-frame-alist '( (tool-bar-lines . 0)))
    (setq default-frame-alist '( (tool-bar-lines . 0)))))

;; Install fonts
(use-package all-the-icons
  :init ;; (all-the-icons-install-fonts)
  :if (display-graphic-p))

;; Set the theme
(use-package doom-themes
  :config
  (load-theme 'doom-solarized-dark-high-contrast t)
  (doom-themes-org-config))

(use-package solaire-mode
  :init (solaire-global-mode +1))

(use-package dimmer
  :init (dimmer-mode t)
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-configure-org))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package dashboard
  :init (dashboard-setup-startup-hook)
  :bind (:map dashboard-mode-map
			  ("n" . dashboard-next-line)
			  ("p" . dashboard-previous-line))
  :config
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Hi Troy, happy coding!")
  (setq dashboard-startup-banner "/Users/troyclendenen/Downloads/dai-logo-long-light.png")
  (setq dashboard-set-navigator t)
  (setq dashboard-set-footer nil)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 5))))

(use-package popper
  :init
  (setq popper-reference-buffers
	'("\\*vterm\\*$" vterm-mode ; vterm
	  "^\\*cider-repl.*" ; cider repl
	  "\\*.* Launch.*" ; DAP debug output
	  help-mode))
  (popper-mode +1)
  :bind (("C-'"   . popper-toggle-latest)
         ("C-\""  . popper-cycle)
         ("C-M-'" . popper-toggle-type))
  :config
  (setq popper-mode-line nil))

;; Editing enhancements

;; vertical buffer and completion layouts
(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init (vertico-mode 1))
(use-package vertico-directory
  :after vertico
  :straight nil
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word)))

;; previews and snappiness for buffers and searches
(use-package consult
  :bind (("C-x b" . consult-buffer)
		 ("M-g g" . consult-goto-line))
  :hook (completion-list-mode . consult-preview-at-point-mode))

;; C-x b and C-x C-f coolness
(use-package marginalia
  :init (marginalia-mode 1))

;; better completions, oh my!
(use-package orderless
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles basic partial-completion)))))

;; which key
(use-package which-key
  :config
  (add-hook 'after-init-hook 'which-key-mode)
  (setq which-key-popup-type 'minibuffer))

;; org mode
(use-package org
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (setq org-startup-truncated nil))

;; git coolness
(use-package magit
  :config
  (global-set-key (kbd "C-c g") 'magit-file-dispatch)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
(use-package git-gutter-fringe
  :init (global-git-gutter-mode +1)
  :config (setq git-gutter-fr:side 'right-fringe))

;; TODO: remove?
(use-package projectile
  :init (projectile-mode)
  :bind (:map projectile-mode-map
			  ("C-c p s" . consult-ripgrep))
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-indexing-method 'alien))

;; I don't use this but it's fun to screenshot
(use-package treemacs
  :bind (:map global-map ("C-c t t" . treemacs))
  :config (setq treemacs-is-never-other-window t))

;; sometime C- is too much
(use-package god-mode
  :config
  (global-set-key (kbd "<escape>") #'god-local-mode)
  (defun my-god-mode-update-cursor-type ()
	(setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))
  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type))

;; Shhh, this is how I check if a buffer is saved lol
(use-package centaur-tabs
  :init (centaur-tabs-mode t)
  :bind
  ("C-<tab>" . centaur-tabs-forward)
  ("C-S-<tab>" . centaur-tabs-backward)
  :config
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-bar 'under)
  (setq centaur-tabs-excluded-prefixes (list "*")))

;; See treemacs
(use-package minimap
  :bind (:map global-map ("C-c t m" . minimap-mode))
  :config
  (setq minimap-update-delay 0)
  (setq minimap-highlight-line nil)
  (setq minimap-window-location 'right)
  (setq minimap-width-fraction 0.10))

;; The one company I'm a shill for
(use-package company
  :hook ((emacs-lisp-mode . company-mode)
		 (prog-mode . company-mode)
		 (text-mode . company-mode)
		 (cider-repl-mode . company-mode))
  :bind (:map company-active-map
			  ("C-<return>" . company-complete-selection))
  :config
  (setq company-dabbrev-downcase t)
  (setq company-idle-delay 0.3)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<return>") nil)
  (with-eval-after-load "company"
    (add-hook 'company-mode-hook 'electric-pair-mode)
    (add-hook 'company-mode-hook 'display-line-numbers-mode)))

;; I finally got this to work
(use-package company-quickhelp
  :init (company-quickhelp-mode)
  :config
  (setq company-quickhelp-delay 2)
  (eval-after-load 'company
    '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)))

;; sanitizing code so you don't have to
(use-package flycheck)
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;; Language Servers
(use-package lsp-mode
  :hook ((c-mode . lsp)
		 (c++-mode . lsp)
		 (rust-mode . lsp)
		 (go-mode . lsp)
		 (clojure-mode . lsp)
		 (cider-repl-mode . lsp)
		 (lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
			  ("C-c l t" . lsp-semantic-tokens-mode))
  :config
  (setq lsp-insert-final-newline nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-on-type-formatting nil)
  :commands lsp)
(setq lsp-keymap-prefix "C-c l")

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-ui-sideline-show-diagnostics t)
  ;; (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-doc-show-with-mouse nil))

;; Faster syntax highlighting
(use-package tree-sitter
  :init
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs)

;; FIXME does this do what it's supposed to?
(use-package dtrt-indent
  :init (dtrt-indent-mode 1))

;; TODO FIXME DEBUG GOTCHA STUB
(use-package hl-todo
  :init (global-hl-todo-mode 1)
  :config (setq hl-todo-keyword-faces
				'(("TODO"   . "#FFFF00")
				  ("FIXME"  . "#FF0000")
				  ("DEBUG"  . "#A020F0")
				  ("GOTCHA" . "#FF4500")
				  ("STUB"   . "#1E90FF"))))

;; Fine Eric, I'll set up a debugger
(use-package dap-mode
  :bind (:map dap-mode-map
			  ("C-c d n" . dap-next)
			  ("C-c d i" . dap-step-in)
			  ("C-c d o" . dap-step-out)
			  ("C-c d c" . dap-continue)
			  ("C-c d r" . dap-debug-restart)
			  ("C-c d s" . dap-debug)
			  ("C-c d d t" . dap-debug-edit-template)
			  ("C-c d d r" . dap-debug-recent)
			  ("C-c d d l" . dap-debug-last)
			  ("C-c d e e" . dap-eval)
			  ("C-c d e r" . dap-eval-region)
			  ("C-c d e s" . dap-eval-thing-at-point)
			  ("C-c d e a" . dap-ui-expressions-add)
			  ("C-c d e d" . dap-ui-expressions-remove)
			  ("C-c d b t" . dap-breakpoint-toggle)
			  ("C-c d b c" . dap-breakpoint-condition)
			  ("C-c d b h" . dap-breakpoint-hit-condition)
			  ("C-c d b l" . dap-breakpoint-log-message)
			  ("C-c d f d" . dap-down-stack-frame)
			  ("C-c d f u" . dap-up-stack-frame)
			  ("C-c d f s" . dap-switch-stack-frame)
			  ("C-c d k a" . dap-delete-all-sessions)
			  ("C-c d k s" . dap-delete-session)))
(require 'dap-lldb)

;; I'm supposed to use-package this, but I never use it
(use-package yasnippet
  :config (yas-reload-all)
  :hook (company-mode . yas-minor-mode))

;; Pretty elisp popups
(use-package eros
  :init (eros-mode 1))

;; C/C++ Settings
(setq-default c-basic-offset 4
			  tab-width 4
			  indent-tabs-mode t)
(setq c-hungry-delete-key t)

;; LLVM IR highlighting
(straight-use-package
 '(llvm-mode :type git :host github :repo "nverno/llvm-mode"))

;; CMake highlighting
(use-package cmake-mode)

;; Rust support
(use-package rust-mode)

;; Go support
(use-package go-mode)

;; Structural editing is pretty cool tbh
(use-package paredit
  :hook ((clojure-mode . paredit-mode)
		 (cider-repl-mode . paredit-mode)
		 (emacs-lisp-mode . paredit-mode)))

;; Clojure support
(use-package clojure-mode)
(use-package cider
  :hook (cider-repl-mode . company-mode))

;; Restart emacs
(use-package restart-emacs
  :bind (:map global-map ("C-c q r" . restart-emacs)))

;; vterm
(use-package vterm
  :bind (:map global-map ("C-c v v" . vterm)))

;; ripgrep
(use-package ripgrep)

;; Default emacs settings
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 1)
(delete-selection-mode 1)
(size-indication-mode 1)
(column-number-mode t)
(add-hook 'prog-mode-hook 'hl-line-mode)
(set-face-background 'line-number nil)
(setq scroll-conservatively 101)
(setq inhibit-startup-message t)
(setq mode-require-final-newline nil)
;; macos stuff
(setq mac-command-modifier      'super
      ns-command-modifier       'super
      mac-option-modifier       'meta
      ns-option-modifier        'meta
      mac-right-option-modifier 'none
      ns-right-option-modifier  'none)
(use-package ns-auto-titlebar
  :init (when (eq system-type 'darwin) (ns-auto-titlebar-mode)))

;; project.el configurations:
(setq project-switch-commands
	  '((project-find-file "Find file")
		(consult-ripgrep "Find regexp")
		(project-find-dir "Find directory")
		(magit-status "Magit status")))
(define-key global-map (kbd "C-x p g") 'consult-ripgrep)
(define-key global-map (kbd "C-x p v") 'magit-status)

;; hide show all!
(add-hook 'prog-mode-hook 'hs-minor-mode)
(progn
  (setq hs-minor-mode-map (make-sparse-keymap))
  (define-key hs-minor-mode-map (kbd "C-c h s a") 'hs-show-all)
  (define-key hs-minor-mode-map (kbd "C-c h h a") 'hs-hide-all)
  (define-key hs-minor-mode-map (kbd "C-c h s b") 'hs-show-block)
  (define-key hs-minor-mode-map (kbd "C-c h h b") 'hs-hide-block)
  (define-key hs-minor-mode-map (kbd "C-c h h l") 'hs-hide-level))

;; Keychords
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key (kbd "C-<wheel-up>"))
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)

;; Backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)

;; Guardspec syntax highlighting (but no linting)
(add-to-list 'auto-mode-alist '("\\.gsml\\'" . (lambda ()
                                                 (html-mode)
                                                 (flycheck-mode -1))))

;; Don't ask why I don't just use doom-modeline
(use-package diminish
  :config
  (diminish 'which-key-mode)
  (diminish 'eldoc-mode)
  (diminish 'abbrev-mode)
  (diminish 'hs-minor-mode)
  (diminish 'auto-revert-mode)
  (diminish 'lsp-mode "lsp")
  (diminish 'company-mode)
  (add-hook 'ws-butler-mode-hook
			(lambda () (diminish 'ws-butler-mode)))
  (add-hook 'yas-minor-mode-hook
			(lambda () (diminish 'yas-minor-mode)))
  (add-hook 'lsp-lens-mode-hook
			(lambda () (diminish 'lsp-lens-mode)))
  (add-hook 'paredit-mode-hook
			(lambda () (diminish 'paredit-mode "par")))
  (add-hook 'hs-minor-mode-hook
			(lambda () (diminish 'hs-minor-mode)))
  (diminish 'projectile-mode)
  (diminish 'tree-sitter-mode)
  (diminish 'flycheck-mode)
  (diminish 'git-gutter-mode))
