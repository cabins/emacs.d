;;; init.el --- Emacs init file, builtin-only config -*- lexical-binding: t -*-
;; Author: Cabins
;; Github: https://github.com/cabins-emacs.d
;;; Commentary:
;; Built-in only configuration, requires Emacs 31+. No third-party packages installed.
;;; Code:

;;; Fonts Configuration

;; Global unified font family and default size (130 = 13pt).
(defvar this/font-name "Maple Mono NF CN"
  "The unified font family for all Emacs UI elements.")

(defvar this/font-size 130
  "The default font size in 1/10 pt.")

(defun this/setup-fonts (&optional frame)
  "Apply Maple Mono NF CN across all faces, character sets, and FRAME."
  (with-selected-frame (or frame (selected-frame))
    (when (and (display-graphic-p)
               (find-font (font-spec :family this/font-name)))
      ;; Set default face font and height
      (set-face-attribute 'default nil :family this/font-name :height this/font-size)
      ;; Force all character sets (including CJK and symbols) to use Maple Mono NF CN
      (set-fontset-font t nil (font-spec :family this/font-name)))))

;; Apply font configuration on startup and for future frames (e.g. emacsclient/daemon)
(if (daemonp)
    (add-hook 'after-make-frame-functions #'this/setup-fonts)
  (this/setup-fonts))

;; Ensure new graphic frames inherit the font settings directly
(add-to-list 'default-frame-alist `(font . ,(format "%s-%d" this/font-name (/ this/font-size 10))))

;;; Encoding & Environment

;; Force UTF-8 as the primary coding system across all operations.
(prefer-coding-system 'utf-8)

;;; File Associations

;; Associate .vue files with built-in html-mode.
(add-to-list 'auto-mode-alist '("\\.vue\\'" . html-mode))

;;; Utility Functions & Global Hooks

;; Display total startup time and garbage collection statistics in the minibuffer.
(defun this/display-startup-time ()
  "Display Emacs startup time and the number of garbage collections."
  (message "Emacs started in %.2f seconds with %d GCs."
           (float-time (time-since before-init-time))
           gcs-done))

(add-hook 'emacs-startup-hook #'this/display-startup-time)

;; Enable lightweight global minor modes.
(which-function-mode 1)

;;; Global Defaults Settings

;; UI and Display preferences
(setq-default default-text-properties '(line-spacing 0.2 line-height 1.2)
              frame-title-format "%b"
              mode-line-compact t
              show-trailing-whitespace t)

;; Editing behavior and interaction defaults
(setq-default help-window-select t
              initial-major-mode 'text-mode
              kill-whole-line t
              use-short-answers t)

;; Scrolling behavior and I/O buffer performance
(setq-default auto-window-vscroll nil
              read-process-output-max 4194304
              scroll-conservatively 1000)

;; Files, backup, and environment locale settings
(setq-default default-directory "~"
              make-backup-files nil
              require-final-newline t
              system-time-locale "C")

;; Configure minibuffer and tab completion behaviors.
(setq tab-always-indent 'complete
      completion-auto-select 'second-tab)

;; Configure use-package execution macro defaults globally.
(setq use-package-enable-imenu-support t
      use-package-always-defer t
      use-package-always-ensure nil
      use-package-expand-minimally t)

;;; Built-in Package Configurations

;; Automatically revert buffers when underlying files change on disk.
(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

;; Replace selected text upon typing, matching modern editor behavior.
(use-package delsel
  :hook (after-init . delete-selection-mode))

;; Language Server Protocol (LSP) integration via built-in Eglot.
(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c e f" . eglot-format-buffer)
              ("C-c e r" . eglot-rename)
              ("C-c e a" . eglot-code-actions))
  ;; 在编程模式（含各类 ts-mode）和 HTML 模式（含 .vue 文件）下自动尝试启动 Eglot
  :hook ((prog-mode . eglot-ensure)
         (html-mode . eglot-ensure))
  :custom
  ;; Disable event logging to eliminate memory leaks and overhead.
  (eglot-events-buffer-size 0)
  ;; Faster response time for completion and diagnostics.
  (eglot-send-changes-idle-time 0.15)
  ;; Automatically shutdown LSP server when all project buffers are closed.
  (eglot-autoshutdown t)
  :config
  ;; 指定 Python 使用 ty server (python-base-mode 同时覆盖 python-mode 与 python-ts-mode)
  (add-to-list 'eglot-server-programs '(python-base-mode "ty" "server"))
  ;; 配置 Vue 与 TS/JS 使用 vtsls（优先选择 vtsls，若未安装则降级回 vue-language-server）
  (add-to-list 'eglot-server-programs
               `((html-mode typescript-mode typescript-ts-mode js-mode js-ts-mode)
                 . ,(eglot-alternatives
                     '(("vtsls" "--stdio")
                       ("vue-language-server" "--stdio"))))))

;; Automatically insert matching pairs of parentheses and brackets.
(use-package elec-pair
  :hook (after-init . electric-pair-mode))

;; Enable automatic saving of visited files.
(use-package files
  :hook (after-init . auto-save-visited-mode))

;; On-the-fly syntax checking framework.
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

;; Flexible minibuffer completion visual interface.
(use-package icomplete
  :hook (after-init . fido-mode)
  :custom
  (completion-auto-select t))

;; Incremental search configurations.
(use-package isearch
  :custom
  (isearch-allow-motion t)
  (isearch-lazy-count t))

;; Native minibuffer completion framework settings.
(use-package minibuffer
  :custom
  (completion-styles '(substring flex basic))
  (completions-detailed t)
  (completion-auto-help 'always)
  (completion-eager-update t)
  (completion-header-display 'auto)
  (minibuffer-visible-completions 'up-down))

;; Org-mode settings for documentation and note-taking.
(use-package org
  :custom
  (org-startup-indented t))

;; Automatic parenthesis highlighting setup.
(use-package paren
  :custom
  (show-paren-when-point-in-periphery t)
  (show-paren-when-point-inside-paren t)
  (show-paren-style 'mixed))

;; Smooth pixel-based precision scrolling.
(use-package pixel-scroll
  :hook (after-init . pixel-scroll-precision-mode))

;; General programming modes setup including line highlight and code folding.
(use-package prog-mode
  :hook ((prog-mode . completion-preview-mode)
         (text-mode . completion-preview-mode))
  :config
  (add-hook 'prog-mode-hook (lambda () (setq-local column-number-mode t)))
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'prog-mode-hook #'hs-minor-mode))

;; Track recently opened files for quick access.
(use-package recentf
  :hook (after-init . recentf-mode)
  :bind ("C-c r" . recentf-open))

;; Tree-sitter incremental parsing framework for enhanced syntax highlighting.
(use-package treesit
  :custom
  (major-mode-remap-alist
   '((bash-mode   . bash-ts-mode)
     (c-mode      . c-ts-mode)
     (c++-mode    . c++-ts-mode)
     (json-mode   . json-ts-mode)
     (python-mode . python-ts-mode))))

;; Popup keybinding helper displaying available key combinations.
(use-package which-key
  :hook (after-init . which-key-mode))

;; Window navigation shortcuts using modifier keys.
(use-package windmove
  :config
  (windmove-default-keybindings))

;;; UI Enhancements & Advices

;; Replace standard buffer listing with ibuffer interface.
(defalias 'list-buffers 'ibuffer)

;; Briefly highlight current line on window focus shift or recenter.
(dolist (cmd '(recenter-top-bottom other-window))
  (advice-add cmd :after #'pulse-momentary-highlight-one-line))

;;; Custom Storage Serialization

;; Isolate custom-set variables into a separate dedicated file.
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil))

(provide 'init)

;;; init.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not unresolved obsolete)
;; End:
