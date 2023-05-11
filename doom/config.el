;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Troy Clendenen"
      user-mail-address "troy.clendenen@digital.ai")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; (setq doom-font (font-spec :family "Ubuntu Mono" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "Ubuntu Mono" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)
;; (setq doom-theme 'doom-one-light)
(setq doom-themes-treemacs-enable-variable-pitch nil)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(toggle-frame-maximized)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Enable minibuffer-frame
;; (mini-frame-mode)

(after! lsp-mode
  (lsp-ui-mode t)
  (setq company-minimum-prefix-length 1)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-lens-enable t)
  (setq lsp-ui-peek-enable t)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-insert-final-newline nil)
  (setq require-final-newline nil))

(after! dap-mode
  (map! :map dap-mode-map
        :leader
        :prefix ("d" . "dap")
        ;; basics
        :desc "dap next"          "n" #'dap-next
        :desc "dap step in"       "i" #'dap-step-in
        :desc "dap step out"      "o" #'dap-step-out
        :desc "dap continue"      "c" #'dap-continue
        :desc "dap hydra"         "h" #'dap-hydra
        :desc "dap debug restart" "r" #'dap-debug-restart
        :desc "dap debug"         "s" #'dap-debug

        ;; debug
        :prefix ("dd" . "Debug")
        :desc "dap debug edit template" "t" #'dap-debug-edit-template
        :desc "dap debug recent"  "r" #'dap-debug-recent
        :desc "dap debug last"    "l" #'dap-debug-last

        ;; eval
        :prefix ("de" . "Eval")
        :desc "eval"                "e" #'dap-eval
        :desc "eval region"         "r" #'dap-eval-region
        :desc "eval thing at point" "s" #'dap-eval-thing-at-point
        :desc "add expression"      "a" #'dap-ui-expressions-add
        :desc "remove expression"   "d" #'dap-ui-expressions-remove

        ;; breakpoint
        :prefix ("db" . "Breakpoint")
        :desc "dap breakpoint toggle"      "b" #'dap-breakpoint-toggle
        :desc "dap breakpoint condition"   "c" #'dap-breakpoint-condition
        :desc "dap breakpoint hit count"   "h" #'dap-breakpoint-hit-condition
        :desc "dap breakpoint log message" "l" #'dap-breakpoint-log-message

        ;; stack
        :prefix ("df" . "Stack")
        :desc "dap down stack frame"   "d" #'dap-down-stack-frame
        :desc "dap up stack frame"     "u" #'dap-up-stack-frame
        :desc "dap switch stack frame" "s" #'dap-switch-stack-frame

        ;; delete
        :prefix ("dk" . "Kill")
        :desc "dap delete all sessions" "a" #'dap-delete-all-sessions
        :desc "dap delete session"      "s" #'dap-delete-session)
  )

(global-tree-sitter-mode 1)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(after! centaur-tabs
  (setq centaur-tabs-set-bar 'over)
  (setq x-underline-at-descent-line t)
  (setq centaur-tabs-excluded-prefixes (list "*")))

(after! god-mode
  (global-set-key (kbd "<escape>") #'god-local-mode)
  (defun my-god-mode-update-cursor-type ()
    (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))
  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type)
  (setq god-global-mode nil))

(add-hook! emacs-lisp-mode
  (paredit-mode 1))
(add-hook! clojure-mode
  (paredit-mode 1))
(add-hook! cider-mode
  (paredit-mode 1))

(after! popper
  (map! "C-c C-'" #'popper-toggle-latest
        "C-c C-\"" #'popper-cycle)
  (setq popper-reference-buffers
        '("\\*vterm\\*$" vterm-mode
          "^\\*cider-repl.*"
          "\\*helpful-command.*"
          help-mode)))
(popper-mode +1)

;; Guardspec syntax highlighting (but no linting)
(add-to-list 'auto-mode-alist '("\\.gsml\\'" . (lambda ()
                                                 (html-mode)
                                                 (flycheck-mode -1))))
;; hideshow keybindings
(add-hook 'prog-mode-hook 'hs-minor-mode)
(progn
  (setq hs-minor-mode-map (make-sparse-keymap))
  (define-key hs-minor-mode-map (kbd "C-c h s a") 'hs-show-all)
  (define-key hs-minor-mode-map (kbd "C-c h h a") 'hs-hide-all)
  (define-key hs-minor-mode-map (kbd "C-c h s b") 'hs-show-block)
  (define-key hs-minor-mode-map (kbd "C-c h h b") 'hs-hide-block)
  (define-key hs-minor-mode-map (kbd "C-c h h l") 'hs-hide-level))

(setq fancy-splash-image "~/Downloads/dai-logo-long-light.png")

(add-hook! company-mode
  (company-quickhelp-mode 1))

(dimmer-mode t)
