;; Define the init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Define and initialize package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; use-package to simplify the config file even further
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure 't)

;; ----------------
;; QOL stuff
;; ----------------

;; evil mode
(use-package evil
  :init
  (evil-mode)
  :config
  (evil-set-undo-system 'undo-tree))

;; Undo tree
(use-package undo-tree
  :init
  :config
  (global-undo-tree-mode t))

;; which-key
(use-package which-key
  :config
  (add-hook 'after-init-hook 'which-key-mode)
  )

;; ivy
(use-package ivy
  :init
  ;; (add-hook 'minibuffer-setup-hook 'ivy-mode)
  (ivy-mode t)
  (setq ivy-initial-inputs-alist nil)
  )

(use-package magit
  :bind ("C-x g" . magit-status)
  )

;; ----------------
;; Language support
;; ----------------

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
	 (c-mode . lsp)
	 (c++-mode . lsp)
	 (python-mode . lsp)
	 (rust-mode . lsp)
	 ;; which-key integration
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  )

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-delay 0.1)
  (setq lsp-ui-sideline-show-code-actions t)
  :commands lsp-ui-mode
  )

;; (use-package eglot

;;   :config
;;   (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
;;   (add-hook 'c++-mode-hook 'eglot-ensure)
;;   (add-hook 'c-mode-hook 'eglot-ensure)
;;   (add-hook 'python-mode-hook 'eglot-ensure)
;;   (add-hook 'rust-mode-hook 'eglot-ensure)
;;   )

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (define-key company-active-map (kbd "<tab>") 'company-select-next)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  )

;; (use-package flycheck
;;   :init
;;   (global-flycheck-mode)
;;   )

(use-package sml-mode
  :config
  )

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook
	    (lambda () (setq indent-tabs-mode nil)))
  (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
  )

;; Appearance
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(use-package powerline
  :config
  (powerline-center-theme)
  )

(use-package gruvbox-theme
  :init
  (load-theme 'gruvbox-dark-hard t)
  )

(show-paren-mode t)
(setq show-paren-delay 0)

;; Column number mode
(column-number-mode t)

;; Show line numbers on the side (ver 26+)
(global-display-line-numbers-mode)

;; Smooth scrolling
(setq scroll-conservatively 101)

;; Put emacs temp files in friendly places
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5
      )

;; Keychord Edits
(global-set-key (kbd "C-c r") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

(global-set-key (kbd "C-x w") 'previous-multiframe-window)
(global-set-key (kbd "C-c c") 'shell-command)

(global-unset-key (kbd "C-z"))
