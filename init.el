;;; init.el --- Emacs init file, builtin-only config -*- lexical-binding: t -*-
;; Author: Cabins
;; Github: https://github.com/cabins-emacs.d
;;; Commentary:
;; Built-in only configuration, requires Emacs 31+. No third-party packages installed.
;;; Code:

;;; Fonts Configuration

;; Define and set the default font family for graphic frames.
(defvar this/font-name "Maple Mono NF CN"
  "The default font family for the Emacs frame.")

(when (and (display-graphic-p)
           (find-font (font-spec :family this/font-name)))
  (set-face-attribute 'default t :family this/font-name)
  (set-frame-font this/font-name t))

;;; Encoding & Environment

;; Force UTF-8 as the primary coding system across all operations.
(prefer-coding-system 'utf-8)

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

;; Configure general editor behaviors and defaults.
(setq-default auto-window-vscroll nil
              default-directory "~"
              default-text-properties '(line-spacing 0.2 line-height 1.2)
              frame-title-format "%b"
              help-window-select t
              initial-major-mode 'text-mode
              kill-whole-line t
              mode-line-compact t
              make-backup-files nil
              read-process-output-max 4194304
              require-final-newline t
              scroll-conservatively 1000
              show-trailing-whitespace t
              system-time-locale "C"
              use-short-answers t)

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
  ;; 在编程模式（含各类 ts-mode）和 HTML 模式下自动尝试启动 Eglot
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
  (add-to-list 'eglot-server-programs '(python-base-mode "ty" "server")))

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
