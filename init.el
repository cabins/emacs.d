;;; init.el --- Emacs init file, builtin-only config -*- lexical-binding: t -*-
;; Author: Cabins
;; Github: https://github.com/cabins-emacs.d
;;; Commentary:
;; Built-in only configuration, requires Emacs 31+. No third-party packages installed.
;;; Code:

;;;===========================================================================
;;; Fonts
;;;===========================================================================

(defvar this/font-name "Maple Mono NF CN"
  "The default font family for the Emacs frame.")

(when (find-font (font-spec :family this/font-name))
  (set-face-attribute 'default t :family this/font-name)
  (set-frame-font this/font-name t)
)

;;;===========================================================================
;;; Environment & Encoding
;;;===========================================================================

;; Use UTF-8 as the default coding system to prevent encoding issues
(prefer-coding-system 'utf-8)

;; selection coding system adjustments for non-Windows platforms
(unless (memq system-type '(ms-dos windows-nt cygwin))
  (set-selection-coding-system 'utf-8))

;; Windows pipe and I/O optimizations for LSP/Eglot performance
(when (memq system-type '(ms-dos windows-nt cygwin))
  (with-suppressed-warnings ((free-vars w32-get-true-file-attributes
                                        w32-pipe-read-delay
                                        w32-pipe-buffer-size))
    (setq w32-get-true-file-attributes nil
          w32-pipe-read-delay 0
          w32-pipe-buffer-size 65536
          process-adaptive-read-buffering nil)))

;; macOS modifier keys and native fullscreen setup
(when (eq system-type 'darwin)
  (with-suppressed-warnings ((free-vars mac-command-modifier
                                        mac-option-modifier
                                        ns-use-native-fullscreen))
    (setq mac-command-modifier 'meta
          mac-option-modifier 'super
          ns-use-native-fullscreen t)))

;;;===========================================================================
;;; Utility Functions & Global Hooks
;;;===========================================================================

(defun this/display-startup-time ()
  "Display Emacs startup time and the number of garbage collections."
  (message "Emacs started in %.2f seconds with %d GCs."
           (float-time (time-since before-init-time))
           gcs-done))
(add-hook 'emacs-startup-hook #'this/display-startup-time)

(defun this/prog-mode-common-setup ()
  "Local configurations applied to all programming modes."
  (setq-local column-number-mode t)
  (hl-line-mode 1)
  (hs-minor-mode 1))

(defun this/open-init-file ()
  "Open the user initialization file (init.el) interactively."
  (interactive)
  (find-file user-init-file))

(defun this/open-custom-file ()
  "Open the customization storage file (custom.el) interactively."
  (interactive)
  (find-file custom-file))

;; Bind init file opening shortcut
(global-set-key (kbd "C-,") #'this/open-init-file)

;; Enable global minor modes
(electric-pair-mode 1)
(which-function-mode 1)

;;;===========================================================================
;;; Global Defaults
;;;===========================================================================

(setq-default auto-window-vscroll nil
              default-directory "~"
              default-text-properties '(line-spacing 0.2 line-height 1.2)
              frame-title-format "%b"
              help-window-select t
              initial-major-mode 'text-mode
              kill-whole-line t
              mode-line-compact t
              make-backup-files nil
              read-process-output-max 4194304 ; Pre-calculated (* 4 1024 1024) for optimal LSP data transfer
              require-final-newline t
              scroll-conservatively 1000
              show-trailing-whitespace t
              system-time-locale "C"
              treesit-enabled-modes t
              use-short-answers t)

;; Configure use-package default behaviors
(setq package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)
(require 'use-package)
(setq use-package-enable-imenu-support t
      use-package-always-defer t
      use-package-always-ensure nil
      use-package-expand-minimally t)

;;;===========================================================================
;;; Built-in Package Configurations (via use-package)
;;;===========================================================================

(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

(use-package company
  :ensure t
  :hook (prog-mode . company-mode))

(use-package delsel
  :hook (after-init . delete-selection-mode))

(use-package eglot
  :ensure nil ; Eglot is natively built-in since Emacs 29+

  :bind (:map eglot-mode-map
              ("C-c e f" . eglot-format-buffer)   ; Format code via LSP
              ("C-c e r" . eglot-rename)          ; Rename symbols project-wide
              ("C-c e a" . eglot-code-actions))   ; Trigger quick-fixes/actions
  :config
  (add-to-list 'eglot-server-programs
	       `(python-base-mode . ("ty" "server")))
  :hook
  ;; Target ONLY your requested 5 core languages (Supports both standard & modern Tree-sitter modes)
  ((c-mode-common      . eglot-ensure) ; C/C++
   (c-ts-mode          . eglot-ensure)
   (c++-ts-mode        . eglot-ensure)
   (go-mode            . eglot-ensure) ; Go
   (go-ts-mode         . eglot-ensure)
   (python-base-mode        . eglot-ensure) ; Python
   (rust-mode          . eglot-ensure) ; Rust
   (rust-ts-mode       . eglot-ensure)
   (js-base-mode            . eglot-ensure) ; JavaScript / TypeScript
   (typescript-ts-base-mode    . eglot-ensure))
  :custom
  ;; --- Performance Adjustments ---
  (eglot-events-buffer-size 0)             ; Disable event logs completely to avoid massive memory leaks
  (eglot-send-changes-idle-time 0.15)      ; Optimize buffer sync intervals for snappy completion (default 0.5)
  (eglot-connect-timeout 15)               ; Fast fallback timeout for lagging or crashed servers

  ;; --- User Experience & UI Cleanliness ---
  (eglot-sync-connect nil)                 ; CONNECT ASYNCHRONOUSLY. Never block Emacs UI on project load!
  (eglot-autoshutdown t)                   ; Automatically kill LSP processes when the last project buffer closes
  (eglot-code-action-indications nil)     ; Suppress distracting visual noises/margins for code actions
  (eglot-report-progress nil)              ; Kill mode-line flickering spinners during long background compilations
  (eglot-documentation-renderer 'markdown-ts-view-mode) ; Render rich markdown docs cleanly using tree-sitter
  )


(use-package files
  :hook (after-init . auto-save-visited-mode))

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (("M-n" . #'flymake-goto-next-error)
         ("M-p" . #'flymake-goto-prev-error)))

(use-package icomplete
  :hook (after-init . fido-mode)
  :custom
  (completion-auto-select t)
  (completion-auto-help 'always)
  (completions-detailed t))

(use-package isearch
  :custom
  (isearch-allow-motion t)
  (isearch-lazy-count t))

(use-package minibuffer
  :custom
  (completion-eager-update t)
  (completion-header-display 'auto)
  (minibuffer-visible-completions 'up-down))

(use-package org
  :custom
  (org-startup-indented t)
  (org-modules-loaded t))

(use-package paren
  :custom
  (show-paren-when-point-in-periphery t)
  (show-paren-when-point-inside-paren t)
  (show-paren-style 'mixed))

(use-package pixel-scroll
  :hook (after-init . pixel-scroll-precision-mode))

(use-package prog-mode
  :hook (prog-mode . this/prog-mode-common-setup))

(use-package recentf
  :hook (after-init . recentf-mode)
  :bind (("C-c r" . #'recentf-open)))

(use-package windmove
  :config (windmove-default-keybindings))

(use-package which-key
  :hook (after-init . which-key-mode))

;;;===========================================================================
;;; UI Enhancements & Advices
;;;===========================================================================

;; Substitute default buffer list with ibuffer
(defalias 'list-buffers 'ibuffer)

;; Highlight the current line temporarily when recentering or switching windows
(dolist (cmd '(recenter-top-bottom other-window))
  (advice-add cmd :after #'pulse-momentary-highlight-one-line))

;;;===========================================================================
;;; Custom Storage Serialization
;;;===========================================================================

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil))

(provide 'init)

;;; init.el ends here
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not unresolved obsolete)
;; End:
