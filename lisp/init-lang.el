;;; init-lang.el --- configuration for IDE programming -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Homepage: github.com/cabins
;;; Commentary: I delete some packages, such as `go-mode', `rust-mode', `typescript-mode' etc,
;;              Because I use the relevant '-ts-mode' since Emacs 29.1.
;;              If you want use the -ts-mode, you may install the dll/so files with `treesit-install-language-grammar', if you do NOT want to install any third-part packages.
;;; Code:

(defun gh-repo (short-name)
  "Get the full path of Github url from SHORT-NAME."

  (concat "https://github.com/" short-name))

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

;; CC mode
(add-hook 'c-mode-common-hook 'c-toggle-hungry-state)

;; 非内置支持的一些编程语言模式
(use-package emmet-mode
  :hook ((web-mode css-mode) . emmet-mode))
(use-package markdown-mode)
(use-package protobuf-mode)
(use-package web-mode
  :init
  ;; use web-mode to handle vue/html files
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  :config
  (setq web-mode-enable-current-element-highlight t))

;; 一些感觉比较有用的工具
(use-package quickrun)                  ; quickrun code
(use-package restclient
  :mode (("\\.http\\'" . restclient-mode))) ; restclient support

;; Language Server (eglot - builtin)
;; **************************************************
(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(web-mode "vls"))
  (advice-add 'eglot-code-action-organize-imports :before #'eglot-format-buffer)
  (add-hook 'eglot-managed-mode-hook (lambda () (add-hook 'before-save-hook #'eglot-format-buffer))))

(if (treesit-available-p)
    (use-package treesit
      :ensure nil
      :mode(("\\.go\\'" . go-ts-mode)
            ("/go\\.mod\\'" . go-mod-ts-mode)
            ("\\.rs\\'" . rust-ts-mode)
            ("\\.ts\\'" . typescript-ts-mode)
            ("\\.ya?ml\\'" . yaml-ts-mode))
      :config
      (add-to-list 'treesit-language-source-alist '(gomod . ((gh-repo "camdencheek/tree-sitter-go-mod"))))
      (add-to-list 'treesit-language-source-alist '(kotlin . ((gh-repo "fwcd/tree-sitter-kotlin"))))
      :init
      (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
      (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
      (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
      (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
      (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
      )
  )

(provide 'init-lang)

;;; init-lang.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
