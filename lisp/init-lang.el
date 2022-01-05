;;; init-lang.el --- configuration for IDE programming -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Homepage: github.com/cabins

;;; Commentary:
;;; Code:

;; (require 'init-eglot)
(require 'init-lsp)

(require 'init-elisp)			; elisp
(require 'init-go)			; golang
(require 'init-python)			; python
(require 'init-rust)			; rust
(require 'init-web)			; web development
(use-package json-mode)			; json support
(use-package markdown-mode)		; markdown support
(use-package protobuf-mode)		; protobuf support
(use-package restclient			; restclient support
  :mode (("\\.http\\'" . restclient-mode)))
(use-package yaml-mode)			; yaml support

(use-package quickrun)

(provide 'init-lang)

;;; init-lang.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
