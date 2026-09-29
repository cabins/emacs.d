;;; early-init.el --- Emacs 31+ 早期初始化配置 -*- lexical-binding: t; -*-

;; Author: Kong Lingcun
;; Keywords: convenience, tools, emacs
;; URL: https://github.com/
;; Package-Requires: ((emacs "31.1"))

;;; Commentary:

;; 本文件为 Emacs 启动早期调优配置。
;; 在图形界面创建及主配置文件 init.el 加载前执行。
;; 主要负责提升启动速度、禁用无关 UI 元素、关闭外置包加载以及优化初始化渲染过程。

;;; Code:

;; =============================================================================
;; 1. 启动性能调优 (GC & 挂钩控制)
;; =============================================================================

;; 临时调高垃圾回收 (GC) 阈值至 100MB，避免启动期间频繁触发 GC 拖慢速度
(setq gc-cons-threshold (* 100 1024 1024))

;; 启动完成后恢复 GC 阈值为合理的 8MB，并开启闲置自动回收
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 8 1024 1024))))

;; 启动阶段禁用文件名称处理挂钩 (File Name Handlers)，提升文件加载效率
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)))

;; =============================================================================
;; 2. 包管理器 (Package.el) 控制
;; =============================================================================

;; 阻止 Emacs 在启动时自动加载或初始化外部 ELPA 包
(setq package-enable-at-startup nil)

;; 禁用 package-quickstart 校验
(setq package-quickstart nil)

;; =============================================================================
;; 3. GUI & 渲染优化 (防止界面闪烁)
;; =============================================================================

;; 禁用不必要的 UI 元素 (在窗口创建前即隐藏，防止启动时闪烁)
;; (push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(scroll-bar-lines . 0) default-frame-alist)

;; 取消图形界面下的 Frame 标题栏/菜单显示 (如果需要保持纯净终端感)
(setq ;; menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; 禁用启动时的欢迎画面 (Inhibit Startup Screen)
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name)

;; 禁止调整 Frame 尺寸时的过渡动画与字体微调重绘
(setq frame-inhibit-implied-resize t)

;; 禁止将系统 X Resources 混入 Emacs 配置
(setq inhibit-x-resources t)

(provide 'early-init)

;;; early-init.el ends here
