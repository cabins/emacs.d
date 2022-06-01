;;; init-lang.el --- configuration for IDE programming -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Homepage: github.com/cabins

;;; Commentary:
;;; Code:

;; Common features when programming
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode)
            (electric-layout-mode)
            (electric-pair-mode)
            (highlight-parentheses-mode)
            (hs-minor-mode)
            (prettify-symbols-mode)))

;; Flymake
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :config
  (global-set-key (kbd "M-n") #'flymake-goto-next-error)
  (global-set-key (kbd "M-p") #'flymake-goto-prev-error))

;; cc-mode
(add-hook 'c-mode-common-hook 'c-toggle-hungry-state)

;; Language Server
;; `eglot', a light-weight lsp client
(require 'init-eglot)
;; `lsp-mode', enable next line if you like lsp-mode and disable the previous eglot line
;; (require 'init-lsp)

;; Specific Languages
(require 'init-lang-go)
(require 'init-lang-python)
(require 'init-lang-rust)
(require 'init-lang-web)

;; Program Useful text/config files
(use-package json-mode)
(use-package markdown-mode)
(use-package protobuf-mode)
(use-package yaml-mode)

;; Useful Tools
(use-package quickrun)                  ; quickrun code
(use-package restclient                 ; restclient support
  :mode (("\\.http\\'" . restclient-mode)))

(provide 'init-lang)

;;; init-lang.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
