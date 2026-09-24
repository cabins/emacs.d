;;; init.el --- the main configurations -*- lexical-binding: t; -*-
;;; Commentary:
;;;     - for Emacs 31+ only
;;; Code:

;; =============================================================================
;; 1. Emacs 全局基础 & UI 架构
;; =============================================================================

;; use-package emacs：集中配置 Emacs 自身的内置基础行为
(use-package emacs
  :init
  ;; 基础编码
  (prefer-coding-system 'utf-8)

  ;; 基础变量与选项设置 (使用 setq / setq-default)
  ;; 仅使用内置包，禁止从 ELPA 自动安装
  (setq use-package-always-ensure nil
        ;; 所有 use-package 声明默认延迟加载
        use-package-always-defer t
        ;; 将 yes/no 简化为 y/n
        use-short-answers t
        ;; 压缩模式行的显示
        mode-line-compact t
        ;; 关闭自动备份文件
        make-backup-files nil)

  ;; 缩进一律使用空格，Tab 宽度为 4
  (setq-default indent-tabs-mode nil
                tab-width 4)

  ;; 过滤及隔离自定义配置
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file nil t))

  :config
  ;; 主题加载
  ;; (load-theme 'modus-operandi-tinted t)
  ;; ModeLine背景透明
  (set-face-background 'mode-line 'unspecified)
  (set-face-background 'mode-line-active 'unspecified)
  (set-face-background 'mode-line-inactive 'unspecified)

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

;; 保存 minibuffer 历史 (M-x、搜索等)
(use-package savehist
  :init
  (savehist-mode 1))

;; 记录并恢复每个文件上次的光标位置
(use-package save-place
  :init
  ;; 复用已有的 places.eld 文件
  (setq save-place-file (locate-user-emacs-file "places.eld"))
  (save-place-mode 1))

;; 大纲折叠
(use-package outline
  :hook (prog-mode . outline-minor-mode)
  :bind (:map outline-minor-mode-map
              ("C-c TAB" . outline-cycle)))

;; =============================================================================
;; 3. 补全系统 (Completion & Preview)
;; =============================================================================

;; Icomplete / Fido 模式：轻量级 minibuffer 补全
(use-package icomplete
  :init
  (fido-mode 1)
  :custom
  ;; 补全候选直接内联显示在输入框中
  (icomplete-in-buffer t)
  ;; 补全匹配风格：基础 + 子串 + 首字母缩写 + 弹性
  (completion-styles '(basic substring initials flex))
  ;; Tab 键用于补全而非缩进
  (tab-always-indent 'complete))

;; Completion Preview：光标处内联预览补全 (Emacs 30+ 内置)
(use-package completion-preview
  :init
  (global-completion-preview-mode 1)
  :custom
  ;; 光标进入预览区域时自动弹出帮助
  (completion-auto-help t)
  ;; 补全面板按组分类展示
  (completions-group t)
  ;; 在补全面板内联显示文档
  (completion-show-inline-help t)
  ;; 补全候选显示详细描述
  (completions-detailed t)
  ;; 输入 1 个字符即触发补全预览
  (completion-preview-minimum-symbol-length 1))

;; =============================================================================
;; 4. 编程语言 & Eglot (LSP) & Tree-sitter & project
;; =============================================================================

;; Tree-sitter：高性能增量语法解析 (供字体锁定/缩进等使用)
(use-package treesit
  :demand t
  :custom
  ;; 对所有支持 Tree-sitter 的主要模式启用
  (treesit-enabled-modes t)
  ;; 缺少 grammar 时自动安装
  (treesit-auto-install-grammar t))

;; Eldoc：在回显区显示函数参数与文档
(use-package eldoc
  :demand t
  :custom
  ;; 在帮助缓冲区显示按需文档
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
  ;; vue-mode 派生自 html-mode，并非 prog-mode 派生类，需单独挂载
  (vue-mode . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-documentation-renderer 'markdown-ts-view-mode)
  ;; 关闭 eglot 的 JSON-RPC 日志记录
  (eglot-events-buffer-config '(:size 0 :v-ssl nil))
  ;; 同步建立连接，打开文件时 LSP 立即可用
  (eglot-sync-connect 1)
  :config
  ;; 1. 当 Eglot 接管 Buffer 时的统一初始化逻辑
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; 启用内联类型提示
              (eglot-inlay-hints-mode 1)

              ;; 针对 Vue 模式，局部禁用格式化能力，彻底规避 vue-language-server 卡死
              (when (derived-mode-p 'vue-mode)
                (setq-local eglot-ignored-server-capabilities
                            (append '(:documentFormattingProvider :documentRangeFormattingProvider)
                                    eglot-ignored-server-capabilities)))))
  ;; 每次 Eglot 接管 buffer 时启用内联类型提示
  (add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)
  ;; 使用rass(需要手动安装uv tool install rassumfrassum)配置ty+ruff为python的语言服务器
  (add-to-list 'eglot-server-programs '(python-base-mode . ("rass" "python")))
  ;; 使用rass配置vue的语言服务器
  (add-to-list 'eglot-server-programs '(vue-mode . ("rass" "vue")))
  ;; 保存时自动格式化 (限制仅在 Eglot 管理的 Buffer 生效)
  (add-hook 'before-save-hook
            (lambda ()
              (when (and (eglot-managed-p)
                         (not (member :documentFormattingProvider eglot-ignored-server-capabilities))
                         (eglot-server-capable :documentFormattingProvider))
                (ignore-errors (eglot-format-buffer))))))

(use-package project
  :demand t
  :custom
  ;;将带有.project的目录自动识别为项目根目录
  (project-vc-extra-root-markers '(".project")))

;; Flymake：内置即时语法检查
(use-package flymake
  :hook
  (prog-mode . flymake-mode)
  :bind
  (:map flymake-mode-map
        ;; 跳转到下一个错误
        ("M-n" . flymake-goto-next-error)
        ;; 跳转到上一个错误
        ("M-p" . flymake-goto-prev-error)))

;; =============================================================================
;; 5. 自定义函数 (Custom Functions)
;; =============================================================================

(defun format-with-oxfmt ()
  "Format current buffer with oxfmt."
  (interactive)
  (unless (executable-find "oxfmt")
    (user-error "未找到 oxfmt 可执行文件，请检查 PATH 设置"))
  ;; 只有当buffer-file-name存在（即保存过成文件才执行格式化）
  (when-let ((file (buffer-file-name)))
    (let ((pt (point)))
      (call-process-region (point-min) (point-max)
                           "oxfmt" t t nil
                           "--stdin-filepath" file)
      (goto-char (min pt (point-max))))))
(defun format-with-oxfmt ()
  "Format current buffer with oxfmt."
  (interactive)
  (unless (executable-find "oxfmt")
    (user-error "未找到 oxfmt 可执行文件，请检查 PATH 设置"))
  (if-let ((file (buffer-file-name)))
      (let ((pt (point)))
        (call-process-region (point-min) (point-max)
                             "oxfmt" t t nil
                             "--stdin-filepath" file)
        (goto-char (min pt (point-max))))
    ;; 当未保存成文件时：仅在交互式 M-x 手动调用时提示，静默不阻断 Hook 保存
    (when (called-interactively-p 'interactive)
      (message "[oxfmt] 当前 Buffer 未保存为文件，请先保存 (C-x C-s) 后再格式化"))))

(defun oxfmt-before-save-hook ()
  "仅在前端相关模式且 oxfmt 存在时，于保存前自动格式化。"
  (when (and (derived-mode-p 'js-base-mode
                             'typescript-ts-base-mode
                             'css-base-mode
                             'json-ts-mode
                             'markdown-ts-mode
                             'toml-ts-mode
                             'yaml-ts-mode)
             (executable-find "oxfmt"))
    (ignore-errors (format-with-oxfmt))))

(add-hook 'before-save-hook #'oxfmt-before-save-hook)

(provide 'init)

;;; init.el ends here
