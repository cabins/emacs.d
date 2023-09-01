;;; init-lang.el --- configuration for IDE programming -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Homepage: github.com/cabins
;;; Commentary: I delete some packages, such as `go-mode', `rust-mode', `typescript-mode' etc,
;;              Because I use the relevant '-ts-mode' since Emacs 29.1.
;;              If you want use the -ts-mode, you may install the dll/so files with `treesit-install-language-grammar', if you do NOT want to install any third-part packages.
;;; Code:

;; 编程模式下建议开启的一些设置
(defun prog-extra-modes()
  "Extra modes when in programming mode."

  (column-number-mode)
  (display-line-numbers-mode)
  (electric-pair-mode)
  (flymake-mode)
  (hs-minor-mode)
  (prettify-symbols-mode))
(add-hook 'prog-mode-hook 'prog-extra-modes)

;; Flymake
(global-set-key (kbd "M-n") #'flymake-goto-next-error)
(global-set-key (kbd "M-p") #'flymake-goto-prev-error)

;; 非内置支持的一些编程语言模式
(use-package protobuf-mode)

;; ;; 一些感觉比较有用的工具
(use-package quickrun)
(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)))

;; Language Server (eglot - builtin)
;; **************************************************
(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :bind ("C-c e f" . eglot-format)
  :config (advice-add 'eglot-code-action-organize-imports :before #'eglot-format-buffer))

(if (treesit-available-p)
    (require 'init-treesit))

(provide 'init-lang)

;;; init-lang.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
