;;; init-builtins --- settings for builtins  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; os & coding settings
(with-eval-after-load 'w32-win
  (when cabins-os-win
    (setq w32-get-true-file-attributes nil
          w32-pipe-read-delay 0
          w32-pipe-buffer-size (* 64 1024))))

(when cabins-os-mac
  (setq mac-command-modifier 'meta
	mac-option-modifier 'super
	ns-use-native-fullscreen t))

;; solve the Chinese paste issue
;; let Emacs auto-guess the selection coding according to the Windows/system settings
(prefer-coding-system 'utf-8)
(unless cabins-os-win
  (set-selection-coding-system 'utf-8))

;; core package settings
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; the basic usage for `use-package'
;; (use-package package-name
;;   :ensure t       ; package will be installed automatically, if the package is not installed
;;   :init (code)    ; execute before package loading
;;   :config (code)  ; execute after package loading
;;   :bind (keybindings)
;;   :hook (hooks)
;;   :defer t        ; defer loading, until needed (speep up)
;; )

;; programming language hooks
(defun my/prog-mode-common-setup ()
  "通用编程模式设置，包括显示列号、行号和启用次模式等."
  (setq-local column-number-mode t)
  (display-line-numbers-mode 1)
  (electric-pair-mode 1)
  (hl-line-mode 1)
  (hs-minor-mode 1)
  (visual-line-mode 1)
  (which-function-mode 1))

;; make use-package default behavior better
;; with `use-package-always-ensure' you won't need ":ensure t" all the time
;; with `use-package-always-defer' you won't need ":defer t" all the time
(setq use-package-enable-imenu-support t
      use-package-expand-minimally t)

;; Emacs builtin packages
(setq-default auto-window-vscroll nil
	      default-directory "~"
	      default-text-properties '(line-spacing 0.2 line-height 1.2) ;default line height
	      frame-title-format "%b"
	      help-window-select t
	      kill-whole-line t
	      mode-line-compact t
	      make-backup-files nil	; disable backup file
	      read-process-output-max (* 4 1024 1024)
	      require-final-newline t
	      scroll-conservatively 1000
	      show-trailing-whitespace t
	      system-time-locale "C"
	      use-short-answers t)

;; auto revert
;; `global-auto-revert-mode' is provided by autorevert.el (builtin)
(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

;; Delete Behavior
;; `delete-selection-mode' is provided by delsel.el (builtin)
(use-package delsel
  :hook (after-init . delete-selection-mode))

;; Language Server (eglot - builtin since v29)
(use-package eglot
  :bind (:map eglot-mode-map ("C-c e f" . eglot-format-buffer))
  :custom
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.1)
  (eglot-documentation-renderer 'markdown-ts-view-mode)
  (eglot-code-action-indications nil)
  :hook (prog-mode . eglot-ensure))

;; Flymake
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (("M-n" . #'flymake-goto-next-error)
	 ("M-p" . #'flymake-goto-prev-error)))

;; auto save to the visited file (provided by `files.el')
(use-package files
  :hook (after-init . auto-save-visited-mode))

;; ibuffer
(defalias 'list-buffers 'ibuffer)

;; fido-mode
;; `fido-mode' is provided by icomplete.el
(use-package icomplete
  :hook (after-init . fido-mode)
  :config (setq completions-detailed t
		icomplete-vertical-in-buffer-adjust-list t
		icomplete-vertical-render-prefix-indicator t))

;; isearch
(use-package isearch
  :config
  (setq-default isearch-allow-motion t
		isearch-lazy-count t))

;; markdown-ts-mode
(use-package markdown-ts-mode
  :ensure nil
  :defer t)

;; minibuffer
(use-package minibuffer
  :config
  (setq completion-eager-update t
	completion-eader-display 'auto
	minibuffer-visible-completions 'up-down))

;; Org Mode
(use-package org
  :config
  (setq org-startup-indented t
	org-modules-loaded t))

;; Show Paren Mode
(use-package paren
  :config
  (setq show-paren-when-point-in-periphery t
	show-paren-when-point-inside-paren t
	show-paren-style 'mixed))

;; pixel-scroll-precise-mode
(use-package pixel-scroll
  :hook (after-init . pixel-scroll-precision-mode))

(use-package prog-mode
  :hook ((prog-mode . my/prog-mode-common-setup)))

;; Recentf
(use-package recentf
  :hook (after-init . recentf-mode)
  ;; recentf-open since v29.1, recentf-open-files since v22
  :bind (("C-c r" . #'recentf-open)))

;; tree-sitter
(use-package treesit
  :ensure nil
  :config
  (setq treesit-auto-install-grammar 'always
	treesit-enabled-modes t))

;; Pulse the cursor line
(dolist (cmd '(recenter-top-bottom other-window))
  (advice-add cmd :after (lambda (&rest _) (pulse-momentary-highlight-one-line))))

;; windmove.el, use  <SHIFT - arrow key> to switch buffers
(use-package windmove
  :config (windmove-default-keybindings))

;; Settings for which-key - suggest next key, Builtin package now
(use-package which-key
  :hook (after-init . which-key-mode))

(provide 'init-builtins)

;;; init-builtins.el ends here
