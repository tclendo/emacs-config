
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
;; (if (display-graphic-p)
;;     (progn
;;       (set-face-attribute 'default nil :height 160)
;;       (setq initial-frame-alist
;;             '(
;;               (tool-bar-lines . 0)
;;               (width . 90) ; chars
;;               (height . 40) ; lines
;;               (fullscreen . fullscreen)
;; 	      ))
;;       (setq default-frame-alist
;;             '(
;;               (tool-bar-lines . 0)
;;               (width . 90)
;;               (height . 40)
;;               (fullscreen . fullscreen)
;; 	      )))
;;   (progn
;;     (setq initial-frame-alist '( (tool-bar-lines . 0)))
;;     (setq default-frame-alist '( (tool-bar-lines . 0)))))

;; Install fonts
(use-package all-the-icons
  :init
  ;; (all-the-icons-install-fonts)
  :if (display-graphic-p))

;; Set the theme
(use-package doom-themes
  :config (setq doom-dark+-blue-modeline t)
  (load-theme 'doom-dark+ t))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package solaire-mode
  :init (solaire-global-mode +1)
  )

(use-package dimmer
  :init
  (dimmer-mode t)
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-configure-org))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package dashboard
  :init
  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Hi Troy, happy coding!")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-footer nil)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 5)
			  (projects . 5)))
  )

;; Editing enhancements
(use-package vertico
  :init (vertico-mode 1))

;; (use-package ivy
;;   :init (ivy-mode t)
;;   :config
;;   (setq ivy-initial-inputs-alist nil)
;;   (add-hook 'minibuffer-setup-hook 'ivy-mode))

(use-package which-key
  :config
  (add-hook 'after-init-hook 'which-key-mode)
  (setq which-key-popup-type 'minibuffer))

(use-package org
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (setq org-startup-truncated nil))

(use-package magit
  :config
  (global-set-key (kbd "C-c g") 'magit-file-dispatch))

(use-package projectile
  :init
  (projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-indexing-method 'native))

(use-package treemacs
  :bind (:map global-map
	      ("C-c o p" . treemacs))
  :config
  (setq treemacs-is-never-other-window t)
  (setq treemacs-show-cursor nil))

(use-package god-mode
  :config
  (global-set-key (kbd "<escape>") #'god-local-mode)
  (defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))
  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type))

(use-package mini-frame
  :init (mini-frame-mode t))

(use-package minimap
  :bind (:map global-map ("C-c t m" . minimap-mode))
  :config
  (setq minimap-update-delay 0)
  (setq minimap-highlight-line nil)
  (setq minimap-window-location 'right)
  (setq minimap-dedicated-window nil)
  (setq minimap-width-fraction 0.10))

(use-package company
  :hook ((emacs-lisp-mode . company-mode)
	 (prog-mode . company-mode)
	 (text-mode . company-mode))
  :config
  (setq company-dabbrev-downcase t)
  (setq company-idle-delay 0.3)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (with-eval-after-load "company"
    (add-hook 'company-mode-hook 'electric-pair-mode)
    (add-hook 'company-mode-hook 'display-line-numbers-mode)))

(use-package company-quickhelp
  :init (company-quickhelp-mode)
  :config
  (setq company-quickhelp-delay 3)
  (eval-after-load 'company
    '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)))

;; Language Servers
(use-package lsp-mode
  :hook ((c-mode . lsp)
	 (c++-mode . lsp)
	 (rust-mode . lsp)
	 (clojure-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(setq lsp-keymap-prefix "C-c l")

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-diagnostics-provider :auto)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-code-actions t))

(use-package yasnippet
  :config
  (yas-reload-all)
  :hook (company-mode . yas-minor-mode))

;; LLVM IR highlighting
(straight-use-package
 '(llvm-mode :type git :host github :repo "nverno/llvm-mode"))

;; CMake highlighting
(use-package cmake-mode)

;; Rust support
(use-package rust-mode)

(use-package paredit
  :hook ((clojure-mode . paredit-mode)
	 (emacs-lisp-mode . paredit-mode)))

;; Clojure support
(use-package clojure-mode)
(use-package cider)

;; Restart emacs
(use-package restart-emacs
  :bind (:map global-map ("C-c q r" . restart-emacs)))

;; vterm
(use-package vterm)

;; ripgrep
(use-package ripgrep)

;; Default emacs settings
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 1)
(delete-selection-mode 1)
(column-number-mode t)
(set-face-background 'line-number nil)
(setq scroll-conservatively 101)
(setq inhibit-startup-message t)
(setq mode-require-final-newline nil)

;; Keychords
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key (kbd "C-<wheel-up>"))

;; Backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)