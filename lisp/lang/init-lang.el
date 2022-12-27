;;; init-lang.el --- configuration for IDE programming -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Homepage: github.com/cabins
;;; Commentary:
;;; Code:

;; 编程模式下建议开启的一些设置
(defun prog-extra-modes()
  "Extra modes when in programming mode."

  (column-number-mode)
  (display-line-numbers-mode)
  (electric-pair-mode)
  (flymake-mode)
  (hs-minor-mode)
  (prettify-symbols-mode)

  (use-package highlight-parentheses
    :hook (prog-mode . highlight-parentheses-mode)))
(add-hook 'prog-mode-hook 'prog-extra-modes)

;; Flymake
(global-set-key (kbd "M-n") #'flymake-goto-next-error)
(global-set-key (kbd "M-p") #'flymake-goto-prev-error)

;; CC mode
(add-hook 'c-mode-common-hook 'c-toggle-hungry-state)

;; 非内置支持的一些编程语言模式
(use-package emmet-mode
  :hook ((web-mode css-mode) . emmet-mode))
(use-package go-mode)
(use-package kotlin-mode)
(use-package markdown-mode)
(use-package protobuf-mode)
(use-package rust-mode)
(use-package typescript-mode)
(use-package web-mode
  :init
  ;; use web-mode to handle vue/html files
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  :config
  (setq web-mode-enable-current-element-highlight t))
(use-package yaml-mode)

;; 一些感觉比较有用的工具
(use-package quickrun)                  ; quickrun code
(use-package restclient
  :mode (("\\.http\\'" . restclient-mode))) ; restclient support

;; Language Server (eglot - builtin)
;; **************************************************
(use-package eglot
  :hook ((c-mode c++-mode css-mode go-mode java-mode js-mode kotlin-mode python-mode rust-mode ruby-mode web-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(web-mode "vls"))
  (advice-add 'eglot-code-action-organize-imports :before #'eglot-format-buffer)
  (add-hook 'eglot-managed-mode-hook (lambda () (add-hook 'before-save-hook #'eglot-format-buffer))))


(provide 'init-lang)

;;; init-lang.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
