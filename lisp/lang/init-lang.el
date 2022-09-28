;;; init-lang.el --- configuration for IDE programming -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Homepage: github.com/cabins

;;; Commentary:
;;; Code:

;; Common features when programming
;; **************************************************
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode)
            (electric-pair-mode)
            (hs-minor-mode)
            (prettify-symbols-mode)))

;; Flymake
(add-hook 'prog-mode-hook 'flymake-mode)
(global-set-key (kbd "M-n") #'flymake-goto-next-error)
(global-set-key (kbd "M-p") #'flymake-goto-prev-error)

;; CC mode
(add-hook 'c-mode-common-hook 'c-toggle-hungry-state)

;; Highlight Parentheses
(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode))

;; Language Server
;; **************************************************
;; `eglot', a light-weight LSP client
;; (require 'init-eglot)
;; `lsp-mode', a full-feature LSP client
;; (require 'init-lsp)
;; `lsp-bridge', the fastest LSP client
(require 'init-lsp-bridge)

;; Languages
;; **************************************************

;; Golang
(use-package go-mode)

;; Rust
(use-package rust-mode
  :config
  ;; (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run))

;; Web Developemnt
(use-package emmet-mode
  :hook ((web-mode css-mode) . emmet-mode))
(use-package web-mode
  :init
  ;; use web-mode to handle vue/html files
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  :config
  (setq web-mode-enable-current-element-highlight t))
(use-package typescript-mode)

;; Program Useful text/config files
(use-package markdown-mode)
(use-package protobuf-mode)
(use-package yaml-mode)

;; Useful Tools
(use-package quickrun)                  ; quickrun code
(use-package restclient
  :mode (("\\.http\\'" . restclient-mode))) ; restclient support

(provide 'init-lang)

;;; init-lang.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
