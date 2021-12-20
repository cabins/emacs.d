;;; init-lang.el --- configuration for IDE programming -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Homepage: github.com/cabins

;;; Commentary:
;;; Code:

;;; ========== ide features
;; implements the ide features with eglot
;; (another lsp client instead of lsp-mode)
;; Please, DON'T use any lsp client on Windows, for currently.
;;(unless (memq system-type '(windows-nt dos))
;;  (require 'init-eglot))

(require 'init-eglot)

;;; ========== language features
(require 'init-elisp)			; elisp
(require 'init-go)			; golang
(require 'init-python)			; python
(require 'init-rust)			; rust
(require 'init-web)			; web development

;;; ========== tools & serializers
(use-package json-mode)			; json support
(use-package markdown-mode)		; markdown support
(use-package protobuf-mode)		; protobuf support
(use-package restclient			; restclient support
  :mode (("\\.http\\'" . restclient-mode)))
(use-package yaml-mode)			; yaml support

;;; ========== code action features
(use-package quickrun)

(provide 'init-lang)

;;; init-lang.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
