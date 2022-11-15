;; Install straight.el
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
              (fullscreen . fullscreen)
	      ))
      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 90)
              (height . 40)
              (fullscreen . fullscreen)
	      )))
  (progn
    (setq initial-frame-alist '( (tool-bar-lines . 0)))
    (setq default-frame-alist '( (tool-bar-lines . 0)))))

(use-package doom-themes
  :config
  (load-theme 'doom-molokai t)
  )

;; ----------------
;; Editing enhancements
;; ----------------

(use-package undo-tree
  :config
  (global-undo-tree-mode t)
  (setq undo-tree-auto-save-history nil)
  )

(use-package which-key
  :config
  (add-hook 'after-init-hook 'which-key-mode)
  (setq which-key-popup-type 'minibuffer)
  )

(use-package ivy
  :init
  (ivy-mode t)
  :config
  (setq ivy-initial-inputs-alist nil)
  (add-hook 'minibuffer-setup-hook 'ivy-mode)
  )

(use-package org
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (setq org-startup-truncated nil)
  )

(use-package projectile
  :init
  (projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-indexing-method 'native)
  )

(use-package treemacs
  :bind
  (:map global-map
        ("C-x t t" . treemacs)
		("C-x t d" . treemacs-select-directory))
  :config
  (setq treemacs-is-never-other-window t)
  (setq treemacs-user-mode-line-format 'none)
  (setq treemacs-show-cursoe nil)
  )

(use-package ripgrep
  :after projectile)

(use-package magit
  :config
  (global-set-key (kbd "C-c g") 'magit-file-dispatch)
  )

(use-package centaur-tabs
  :init
  (centaur-tabs-mode)
  :config
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-headline-match)
  :bind
  (:map global-map
        ("C-<tab>" . centaur-tabs-forward)
        ("C-S-<tab>" . centaur-tabs-backward))
  )

(use-package god-mode
  :config
  (global-set-key (kbd "<escape>") #'god-local-mode)
  (defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))
  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type)
  )

(use-package org-jira
  :config
  (setq jiralib-url "https://arxantech.atlassian.net")
  :bind (:prefix-map org-jira-map
                     :prefix "C-c o"
                     ("p g" . org-jira-get-projects)
                     ("i b" . org-jira-browse-issue)
                     ("i g" . org-jira-get-issues)
                     ("i s" . org-jira-get-issues-by-sprint)
                     ("i p" . org-jira-get-issues-by-board)
                     ("i u" . org-jira-update-issue)
                     ("i w" . org-jira-progress-issue)
                     ("i n" . org-jira-progress-issue-next)
                     ("i a" . org-jira-assign-issue)
                     ("i r" . org-jira-refresh-issue)
                     ("i R" . org-jira-refresh-issues-in-buffer)
                     ("i c" . org-jira-create-issue)
                     ("i k" . org-jira-copy-current-issue-key)
                     ("s c" . org-jira-create-subtask)
                     ("s g" . org-jira-get-subtasks)
                     ("c c" . org-jira-add-comment)
                     ("c u" . org-jira-update-comment)
                     ("w u" . org-jira-update-worklogs-from-org-clocks)
                     ("t j" . org-jira-todo-to-jira)
                     ("i f" . org-jira-get-issues-by-fixversion)
					 )
  )

(use-package visual-regexp
  :config
  (define-key global-map (kbd "C-c r") 'vr/replace)
  (define-key global-map (kbd "C-c q") 'vr/query-replace)
  )

(use-package goto-line-preview
  :config
  (global-set-key (kbd "M-g g") 'goto-line-preview))

;;
;; Language Servers
;;

;; C++ Language settings
(setq c-default-style
      '((c-mode . "stroustrup")
        (c++-mode . "stroustrup")
        ))

;; llvm ir highlighting
(straight-use-package
 '(llvm-mode :type git :host github :repo "nverno/llvm-mode"))

;; CMake support
(use-package cmake-mode)

;; (use-package flycheck)

(use-package lsp-mode
  :init
  :hook (
	 (c-mode . lsp)
	 (c++-mode . lsp)
	 (python-mode . lsp)
	 ;; which-key integration
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-keymap-prefix "C-c l")
  )

(use-package lsp-ui
  :config
  (setq lsp-diagnostics-provider :auto)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-sideline-delay 1)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-symbol t)
  :commands lsp-ui-mode)

(use-package company
  :hook (
         (emacs-lisp-mode . company-mode)
         (prog-mode . company-mode)
         (text-mode . company-mode)
         )
  :config
  (setq company-dabbrev-downcase t)
  (setq company-idle-delay 0.3)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (with-eval-after-load "company" (add-hook 'company-mode-hook 'electric-pair-mode))
  )

;; Debugger support
(use-package realgud
  :config
  :bind (:prefix-map debug-prefix-map
                     :prefix "C-x d"
                     ("a c" . realgud:attach-cmd-buffer)
                     ("a s" . realgud:attach-source-buffer)
                     ("d l" . realgud--lldb)
                     ("i" . realgud:cmdbuf-info-describe))
  :bind (:map realgud:shortkey-mode-map
              ("x" . realgud:cmd-disable)
              ("z" . realgud:cmd-enable)
              )
  )

(use-package realgud-lldb
  :after realgud)

(use-package yasnippet
  :config
  (yas-reload-all)
  :hook (company-mode . yas-minor-mode)
  )

(use-package yasnippet-snippets
  :after yasnippet)

;;
;; Appearance
;;

(use-package minimap
  :init
  (minimap-mode 1)
  :config
  (setq minimap-update-delay nil)
  (setq minimap-window-location "Right")
  )

(use-package all-the-icons
  :init
  ;; (all-the-icons-install-fonts)
  :if (display-graphic-p)
  )

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  )

(use-package solaire-mode
  :init
  (solaire-global-mode +1)
  )

(use-package dimmer
  :init
  (dimmer-mode t)
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-configure-org)
  )

(use-package dashboard
  :init
  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Hi Troy, happy coding!")
  (setq dashboard-startup-banner "~/Downloads/dai-logo-long-light.png")
  (setq dashboard-set-navigator t)
  (setq dashboard-set-footer nil)
  (setq dashboard-items '((recents  . 6)
                          (projects . 6)
                          (bookmarks . 6)
                          ))
  (setq bookmark-set-fringe-mark nil)
  )

(use-package beacon
  :config
  (beacon-mode 1)
  (setq beacon-blink-duration 0.2)
  (setq beacon-blink-delay 0.2)
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

;;
;; Miscellaneous
;;

(use-package vterm
  :config
  (global-set-key (kbd "C-c t") 'vterm)
  )

(use-package restart-emacs
  :bind
  (:map global-map
        ("C-x r e" . restart-emacs)
        )
  )

;; Good Default Emacs Defs
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq-default cursor-type 'bar)
(blink-cursor-mode 1)

;; Hideshow mode for hiding/showing code blocks
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Highlight text then delete on type
(delete-selection-mode 1)

;; Column number mode
(column-number-mode t)

;; Show line numbers on the side (ver 26+)
(add-hook 'company-mode-hook 'display-line-numbers-mode)
(set-face-background 'line-number nil)

;; Smooth scrolling
(setq scroll-conservatively 101)

;; Truncate lines
;; (add-hook 'company-mode-hook 'toggle-truncate-lines)

;; Newline at end of file
(setq mode-require-final-newline nil)

;; Put emacs temp files in friendly places
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)

;; Indentation Fixes
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)

;; Keychord Edits
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key (kbd "C-<wheel-up>"))

(find-file "~/todo.org")
