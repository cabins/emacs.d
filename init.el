;;; init.el --- Emacs 31+ 纯净内置环境配置文件 -*- lexical-binding: t; -*-

;; Author: Cabins
;; Keywords: convenience, tools, emacs
;; URL: https://github.com/cabins/emacs.d
;; Package-Requires: ((emacs "31.1"))

;;; Commentary:

;; 本配置文件专为 Emacs 31 及以上版本设计。
;; 严格遵循 Vanilla Emacs 原生配置范式，完全依赖内置库与工具包，
;; 拒绝任何第三方 ELPA/MELPA 扩展包。
;;
;; 架构主要包含以下五大模块：
;;  1. Emacs 全局基础 & UI 架构
;;  2. 内置实用工具包 (Dired, iBuffer, Recentf, Savehist 等)
;;  3. 内置补全系统 (Fido-mode & Completion Preview)
;;  4. 编程语言与 LSP 架构 (Eglot, Tree-sitter, Flymake, Project)
;;  5. 自定义函数与保存 Hook 集成 (Oxfmt 集成)

;;; Code:

;; =============================================================================
;; 1. Emacs 全局基础 & UI 架构
;; =============================================================================

;; Emacs 核心引擎配置
;; 集中管理 Emacs 自身的全局编码、缩进规则、UI 视觉以及基础编辑行为
(use-package emacs
  :init
  ;; 编码系统配置：默认全局采用 UTF-8 编码
  (prefer-coding-system 'utf-8)

  ;; 基础变量与选项设置
  ;; 仅使用内置包，禁止自动从 ELPA 安装
  (setq use-package-always-ensure nil
        ;; 所有 use-package 声明默认启用延迟加载
        use-package-always-defer t
        ;; 将交互提示中的 yes/no 简化为 y/n
        use-short-answers t
        ;; 启用模式行紧凑显示，自动清理多余空格
        mode-line-compact t
        ;; 关闭自动生成备份文件 (file~)
        make-backup-files nil)

  ;; 全局文本缩进规则：一律替换为空格，Tab 缩进宽度固定为 4 列
  (setq-default indent-tabs-mode nil
                tab-width 4)

  ;; 隔离自定义配置：将 M-x customize 生成的代码重定向至独立文件
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file nil t))

  :config
  ;; 主题与 Mode-line 面板优化：将模式行背景设为透明以契合终端/系统主题
  (set-face-attribute 'mode-line nil :underline nil :box nil)
  (set-face-attribute 'mode-line-active nil :underline nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :underline nil :box nil)

  ;; 开启全局轻量级基础 Mode
  ;; 磁盘文件变更时自动刷新 Buffer
  (global-auto-revert-mode 1)
  ;; 闲置时自动保存已修改的 Buffer
  (auto-save-visited-mode 1)
  ;; 高亮显示当前光标所在行
  (global-hl-line-mode 1)
  ;; 括号与引号自动配对闭合
  (electric-pair-mode 1)

  ;; 终端集成：开启 TTY 环境下的鼠标点击与滚动支持
  (add-hook 'tty-setup-hook #'xterm-mouse-mode))

;; =============================================================================
;; 2. 内置工具包配置
;; =============================================================================

;; Which-Key：按键序列提示工具
;; 当输入未完成的快捷键组合时，在底部自动弹出候选快捷键提示面板
(use-package which-key
  :init
  (which-key-mode 1))

;; Dired：内置文件管理器
;; 提供高效的文件/目录浏览、批量重命名及文件操作能力
(use-package dired
  :custom
  ;; 文件列表详细参数：使用人类可读大小、按目录优先排序
  (dired-listing-switches "-lahv --group-directories-first")
  ;; 开启 DWIM (Do What I Mean)：双窗口模式下自动推荐目标复制/移动路径
  (dired-dwim-target t)
  :hook
  ;; 进入 Dired 时自动隐藏文件权限与所有者信息，保持界面简洁
  (dired-mode . dired-hide-details-mode))

;; iBuffer：高级 Buffer 管理面板
;; 替换默认的 list-buffers，提供对打开 Buffer 的分类、筛选与批量操作
(use-package ibuffer
  :bind
  ([remap list-buffers] . ibuffer))

;; Recentf：最近打开文件记录器
;; 自动追踪并记录最近访问过的文件历史，方便快速二次打开
(use-package recentf
  :init
  (recentf-mode 1)
  :bind
  ("C-c r" . recentf-open-files))

;; Savehist：Minibuffer 输入历史持久化
;; 保存命令历史 (M-x)、搜索历史等，在 Emacs 重启后依然可复用
(use-package savehist
  :init
  (savehist-mode 1))

;; Save-Place：光标位置记忆器
;; 自动记录并恢复上次关闭文件时光标所处的精确位置
(use-package save-place
  :init
  (save-place-mode 1))

;; Outline：代码与文本大纲折叠工具
;; 基于层级结构对文本或代码块进行展开与折叠
(use-package outline
  :hook (prog-mode . outline-minor-mode)
  :bind (:map outline-minor-mode-map
              ("C-c TAB" . outline-cycle)))

;; =============================================================================
;; 3. 补全系统 (Completion & Preview)
;; =============================================================================

;; Icomplete / Fido：内置 Minibuffer 垂直与交互补全引擎
;; 提供类似 Vertico/Rxfico 的极简 Minibuffer 过滤体验
(use-package icomplete
  :init
  ;; 启用 Fido 模式 (模拟 Ido 的交互行为)
  (fido-mode 1)
  :custom
  ;; 补全候选直接内联显示在输入框中
  (icomplete-in-buffer t)
  ;; 补全匹配风格：基础 + 子串 + 首字母缩写 + 弹性模糊匹配
  (completion-styles '(basic substring initials flex))
  ;; Tab 键优先触发上下文补全
  (tab-always-indent 'complete))

;; Completion Preview：光标处内联补全预览 (Emacs 30+ 内置)
;; 在代码/文本编辑区提供类似 Copilot/Ghost-text 风格的实时补全预览
(use-package completion-preview
  :init
  (global-completion-preview-mode 1)
  :custom
  ;; 光标进入预览区域时自动弹出帮助
  (completion-auto-help t)
  ;; 补全面板候选按类别分组展示
  (completions-group t)
  ;; 在补全面板中内联显示帮助文档
  (completion-show-inline-help t)
  ;; 显示详细的变量类型或函数签名描述
  (completions-detailed t)
  ;; 输入 1 个字符即触发预览
  (completion-preview-minimum-symbol-length 1))

;; =============================================================================
;; 4. 编程语言 & Eglot (LSP) & Tree-sitter & Project
;; =============================================================================

;; Tree-sitter：高性能增量语法解析引擎
;; 用于实现准确的语法高亮 (Tree-sitter Major Modes) 以及智能代码缩进
(use-package treesit
  :demand t
  :custom
  ;; 全局对所有支持的语言启用 TS 模式
  (treesit-enabled-modes t)
  ;; 缺失语法解析器时自动下载并编译
  (treesit-auto-install-grammar t))

;; Eldoc：实时文档与函数签名显示工具
;; 在 Echo Area (回显区) 或弹窗中自动显示光标所在位置的函数参数及文档
(use-package eldoc
  :demand t
  :custom
  ;; 在帮助缓冲区中按需生成完整文档
  (eldoc-help-at-pt t))

;; SGML-Mode：HTML/XML 编辑模式扩展
;; 此处用于派生出虚拟的 vue-mode，以便 Eglot 与 LSP 能够正确识别与接管 .vue 文件
(use-package sgml-mode
  :init
  ;; 从内置 html-mode 派生 vue-mode
  (define-derived-mode vue-mode html-mode "Vue")
  ;; 建立文件后缀名关联
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode)))

;; Eglot：内置轻量级 LSP (Language Server Protocol) 客户端
;; 提供代码跳转、智能补全、重构、诊断与类型提示等语言服务
(use-package eglot
  :hook
  ;; 编程模式下自动启动/挂载 Eglot
  (prog-mode . eglot-ensure)
  ;; 单独为 Vue 模式挂载 Eglot
  (vue-mode . eglot-ensure)
  :custom
  ;; 关闭最后一个相关 Buffer 时自动关停 LSP 服务器
  (eglot-autoshutdown t)
  ;; 使用 Markdown 渲染文档
  (eglot-documentation-renderer 'markdown-ts-view-mode)
  ;; 关闭 JSON-RPC 日志以提升性能
  (eglot-events-buffer-config '(:size 0 :v-ssl nil))
  ;; 同步建立连接，确保打开文件时 LSP 立即就绪
  (eglot-sync-connect 1)
  :config
  ;; 建立 LSP 服务映射 (通过外部 Rass 工具分发)
  (add-to-list 'eglot-server-programs '(python-base-mode . ("rass" "python")))
  (add-to-list 'eglot-server-programs '(vue-mode . ("rass" "vue")))

  ;; 当 Eglot 成功管理当前 Buffer 时的初始化 Hook
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; 自动启用内联类型提示 (Inlay Hints)
              (eglot-inlay-hints-mode 1)

              ;; Vue 模式局部禁用格式化，避免卡死
              (when (derived-mode-p 'vue-mode)
                (setq-local eglot-ignored-server-capabilities
                            (append '(:documentFormattingProvider :documentRangeFormattingProvider)
                                    eglot-ignored-server-capabilities)))))

  ;; 保存文件前自动执行 LSP 格式化 (仅在服务器支持且未禁用的 Buffer 生效)
  (add-hook 'before-save-hook
            (lambda ()
              (when (and (eglot-managed-p)
                         (not (member :documentFormattingProvider eglot-ignored-server-capabilities))
                         (eglot-server-capable :documentFormattingProvider))
                (ignore-errors (eglot-format-buffer))))))

;; Project：内置项目管理架构
;; 基于版本控制系统或标记文件识别项目边界，提供项目级文件查找与全局搜索
(use-package project
  :demand t
  :custom
  ;; 将包含 .project 文件的目录自动识别为项目根目录
  (project-vc-extra-root-markers '(".project")))

;; Flymake：内置即时代码语法诊断框架
;; 配合 LSP 或外部 Linter，在代码行旁高亮显示 Warning 和 Error
(use-package flymake
  :hook
  (prog-mode . flymake-mode)
  :bind
  (:map flymake-mode-map
        ;; 跳转至下一个语法错误
        ("M-n" . flymake-goto-next-error)
        ;; 跳转至上一个语法错误
        ("M-p" . flymake-goto-prev-error)))

;; Smerge-Mode：Git 冲突解决辅助模式
;; 打开包含 Git 冲突标记 (<<<<<<< / ======= / >>>>>>>) 的文件时自动激活辅助面板
(use-package smerge-mode
  :ensure nil
  :hook (find-file . smerge-start-session))

;; Subword：驼峰命名法光标移动模式
;; 让 M-f 和 M-b 可以在驼峰命名 (CamelCase) 的每个单词内部进行跳转
(use-package subword
  :hook
  (prog-mode . subword-mode))

;; =============================================================================
;; 5. 自定义函数与保存 Hook (Custom Functions)
;; =============================================================================

(defun format-with-oxfmt ()
  "使用外部工具 oxfmt 格式化当前 Buffer 内容."
  (interactive)
  (unless (executable-find "oxfmt")
    (user-error "未找到 oxfmt 可执行文件，请检查 PATH 设置"))
  (if-let* ((file (buffer-file-name)))
      (let ((pt (point)))
        (call-process-region (point-min) (point-max)
                             "oxfmt" t t nil
                             "--stdin-filepath" file)
        (goto-char (min pt (point-max))))
    (when (called-interactively-p 'interactive)
      (message "[oxfmt] 当前 Buffer 未保存为文件，请先保存 (C-x C-s) 后再格式化"))))

(defun oxfmt-before-save-hook ()
  "仅在前端及相关配置文件模式下，于文件保存前自动调用 oxfmt 进行格式化."
  (when (and (derived-mode-p 'js-base-mode
                             'typescript-ts-base-mode
                             'css-base-mode
                             'json-ts-mode
                             'markdown-ts-mode
                             'toml-ts-mode
                             'yaml-ts-mode)
             (executable-find "oxfmt"))
    (ignore-errors (format-with-oxfmt))))

;; 将前端格式化函数挂载至全局保存前 Hook
(add-hook 'before-save-hook #'oxfmt-before-save-hook)

(provide 'init)

;;; init.el ends here
