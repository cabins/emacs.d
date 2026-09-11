;;; -*- lexical-binding: t; -*-

;; =============================================================================
;; 1. Emacs 全局基础 & UI 架构
;; =============================================================================

(use-package emacs
  :init
  ;; 基础编码
  (prefer-coding-system 'utf-8)

  ;; 基础变量与选项设置 (使用 setq / setq-default)
  (setq use-package-always-ensure nil
        use-package-always-defer t
        use-short-answers t                  ; 将 yes/no 简化为 y/n
        mode-line-compact t
        make-backup-files nil)

  (setq-default indent-tabs-mode nil
                tab-width 4)

  ;; 过滤及隔离自定义配置
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file nil t))

  :config
  ;; 主题加载
  (load-theme 'modus-operandi-tinted t)

  ;; 全局轻量级 Mode 开启 (这些是函数，直接调用)
  (global-auto-revert-mode 1)
  (auto-save-visited-mode 1)
  (global-hl-line-mode 1)
  (electric-pair-mode 1)

  ;; 终端鼠标支持
  (add-hook 'tty-setup-hook #'xterm-mouse-mode))

;; =============================================================================
;; 2. 内置工具包配置
;; =============================================================================

;; 按键提示
(use-package which-key
  :init
  (which-key-mode 1))

;; 文件管理 (Dired)
(use-package dired
  :custom
  (dired-listing-switches "-lahv --group-directories-first")
  (dired-dwim-target t)
  :hook
  (dired-mode . dired-hide-details-mode))

;; Buffer 管理 (iBuffer 替代 list-buffers)
(use-package ibuffer
  :bind
  ([remap list-buffers] . ibuffer))

;; 最近打开文件 (Recentf)
(use-package recentf
  :init
  (recentf-mode 1)
  :bind
  ("C-c r" . recentf-open-files))

;; 大纲折叠
(use-package outline
  :hook (prog-mode . outline-minor-mode)
  :bind (:map outline-minor-mode-map
              ("C-c TAB" . outline-cycle)))

;; =============================================================================
;; 3. 补全系统 (Completion & Preview)
;; =============================================================================

(use-package icomplete
  :init
  (fido-mode 1)
  :custom
  (icomplete-in-buffer t)
  (completion-styles '(basic substring initials flex))
  (tab-always-indent 'complete))

(use-package completion-preview
  :init
  (global-completion-preview-mode 1)
  :custom
  (completion-auto-help t)
  (completions-group t)
  (completion-show-inline-help t)
  (completions-detailed t)
  (completion-preview-minimum-symbol-length 1))

;; =============================================================================
;; 4. 编程语言 & Eglot (LSP) & Tree-sitter & project
;; =============================================================================

(use-package treesit
  :custom
  (treesit-enabled-modes t)
  (treesit-auto-install-grammar t))

(use-package eldoc
  :custom
  (eldoc-help-at-pt t))

;; 配置vue-mode
(use-package sgml-mode
  :init
  ;; 利用内置 html-mode 派生出名为 vue-mode 的虚拟模式，供 Eglot 识别
  (define-derived-mode vue-mode html-mode "Vue")
  ;; 将 .vue 文件关联到新创建的 vue-mode
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode)))

(use-package eglot
  :hook
  (prog-mode . eglot-ensure)
  :custom
  (eglot-events-buffer-size 0)
  (eglot-autoshutdown t)
  :config
  ;; 使用rass(需要手动安装uv tool install rassumfrassum)配置ty+ruff为python的语言服务器
  (add-to-list 'eglot-server-programs '(python-base-mode . ("rass" "--" "ty" "server" "--" "ruff" "server")))
  ;; 保存时自动格式化 (限制仅在 Eglot 管理的 Buffer 生效)
  (add-hook 'before-save-hook
            (lambda ()
              (when (eglot-managed-p)
                (eglot-format-buffer)))))

(use-package project
  :custom
  ;;将带有.project的目录自动识别为项目根目录
  (project-vc-extra-root-markers '(".project")))

;; =============================================================================
;; 5. 自定义函数 (Custom Functions)
;; =============================================================================

(defun format-with-oxfmt ()
  "Format current buffer with oxfmt."
  (interactive)
  (if (executable-find "oxfmt")
      (call-process-region (point-min) (point-max)
                           "oxfmt" t t t
                           "--stdin-filepath" (buffer-file-name))
    (user-error "未找到 oxfmt 可执行文件，请检查 PATH 设置")))
(put 'narrow-to-region 'disabled nil)
