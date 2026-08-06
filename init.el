;;; init.el --- Emacs init file, builtin-only config -*- lexical-binding: t -*-
;; Author: Cabins
;; Github: https://github.com/cabins-emacs.d
;;; Commentary:
;; 纯内置配置，依赖 Emacs 31+，不安装任何第三方包。
;;; Code:

;;;===========================================================================
;;; 平台检测
;;;===========================================================================

(defvar cabins-os-win (memq system-type '(ms-dos windows-nt cygwin))
  "非 nil 表示当前运行在 Windows。")

(defvar cabins-os-mac (eq system-type 'darwin)
  "非 nil 表示当前运行在 macOS。")

;;;===========================================================================
;;; 字体
;;;===========================================================================

(defvar my/font-name "Maple Mono NF CN"
  "默认字体族。")

;; 启动帧在读取 init.el 之前就已创建并带有显式 font 参数，
;; 因此除 `set-face-attribute'（作用于后续帧）外，
;; 还需 `set-frame-font' 刷新所有已存在的帧。
(when (find-font (font-spec :family my/font-name))
  (set-face-attribute 'default t :family my/font-name)
  (set-frame-font my/font-name nil t))

;;;===========================================================================
;;; 编码与环境
;;;===========================================================================

;; 统一 UTF-8，避免 Windows 中文粘贴乱码
(prefer-coding-system 'utf-8)
(unless cabins-os-win
  (set-selection-coding-system 'utf-8))

;; Windows 管道优化
(with-eval-after-load 'w32-win
  (when cabins-os-win
    (setq w32-get-true-file-attributes nil
          w32-pipe-read-delay 0
          w32-pipe-buffer-size (* 64 1024))))

;; macOS 修饰键映射
(with-suppressed-warnings ((free-vars mac-command-modifier
                                      mac-option-modifier
                                      ns-use-native-fullscreen))
  (when cabins-os-mac
    (setq mac-command-modifier 'meta
          mac-option-modifier 'super
          ns-use-native-fullscreen t)))

;;;===========================================================================
;;; 工具函数
;;;===========================================================================

(defun my/display-startup-time ()
  "回显启动耗时与垃圾回收次数。"
  (message "Emacs 启动耗时 %.2f 秒（%d 次 GC）"
           (float-time (time-since before-init-time))
           gcs-done))
(add-hook 'emacs-startup-hook #'my/display-startup-time)

(defun my/prog-mode-common-setup ()
  "编程模式通用设置：行号、列号与常用次模式。"
  (setq-local column-number-mode t)
  (display-line-numbers-mode 1)
  (electric-pair-mode 1)
  (hl-line-mode 1)
  (hs-minor-mode 1)
  (visual-line-mode 1)
  (which-function-mode 1))

(defun my/open-init-file ()
  "打开配置文件 init.el。"
  (interactive)
  (find-file user-init-file))

(defun my/open-custom-file ()
  "打开定制文件 custom.el。"
  (interactive)
  (find-file custom-file))

(global-set-key (kbd "C-,") #'my/open-init-file)

;;;===========================================================================
;;; 全局默认值
;;;===========================================================================

(setq-default auto-window-vscroll nil
              default-directory "~"
              default-text-properties '(line-spacing 0.2 line-height 1.2)
              frame-title-format "%b"
              help-window-select t
	      initial-major-mode 'text-mode
              kill-whole-line t
              mode-line-compact t
              make-backup-files nil              ; 不生成备份文件
              read-process-output-max (* 4 1024 1024)
              require-final-newline t
              scroll-conservatively 1000
              show-trailing-whitespace t
              system-time-locale "C"
	      treesit-enabled-modes t
              use-short-answers t)

;; use-package 默认行为更友好
(require 'use-package)
(setq use-package-enable-imenu-support t
      use-package-expand-minimally t)

;;;===========================================================================
;;; 内置包配置
;;;===========================================================================

(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

(use-package delsel
  :hook (after-init . delete-selection-mode))

;; 语言服务（Emacs 29+ 内置），依赖 markdown-ts-mode 渲染文档
(use-package eglot
  :bind (:map eglot-mode-map ("C-c e f" . eglot-format-buffer))
  :preface
  (defvar my/eglot-ignored-modes
    '(emacs-lisp-mode        ; Elisp 编程模式
      lisp-interaction-mode  ; Elisp 交互模式（如 *scratch* 缓冲）
      )
    "不自动启用 Eglot 的主模式列表。")

  (defun my/eglot-ensure-unless-ignored ()
    "如果当前模式不在黑名单中，则自动启用 Eglot。"
    (unless (or (minibufferp)
                (member major-mode my/eglot-ignored-modes))
      (eglot-ensure)))

  :hook
  (prog-mode . my/eglot-ensure-unless-ignored)
  :config
  (setq eglot-events-buffer-size 0) ; 禁用事件日志以提升性能（可选）
  :custom
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.1)
  (eglot-documentation-renderer 'markdown-ts-view-mode)
  (eglot-code-action-indications nil)
  )

(use-package files
  :hook (after-init . auto-save-visited-mode))

;; 编译/语法错误导航
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (("M-n" . #'flymake-goto-next-error)
         ("M-p" . #'flymake-goto-prev-error)))

;; 补全 UI（fido 垂直菜单）
(use-package icomplete
  :hook (after-init . fido-vertical-mode)
  :custom
  (completion-auto-select t)
  (completion-auto-help 'always)
  (completions-detailed t)
  (icomplete-vertical-in-buffer-adjust-list t)
  (icomplete-vertical-render-prefix-indicator t))

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

;; 编程模式通用设置
(use-package prog-mode
  :hook (prog-mode . my/prog-mode-common-setup))

;; 最近打开文件
(use-package recentf
  :hook (after-init . recentf-mode)
  :bind (("C-c r" . #'recentf-open)))

;; 窗口切换（SHIFT + 方向键）
(use-package windmove
  :config (windmove-default-keybindings))

;; 按键提示
(use-package which-key
  :hook (after-init . which-key-mode))

;;;===========================================================================
;;; 其它设置
;;;===========================================================================

;; 用 ibuffer 替代默认 buffer 列表
(defalias 'list-buffers 'ibuffer)

;; 重绘与切换窗口时高亮当前行
(dolist (cmd '(recenter-top-bottom other-window))
  (advice-add cmd :after #'pulse-momentary-highlight-one-line))

;;;===========================================================================
;;; custom 文件
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
