;;; init-lang.el --- configuration for IDE programming -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Homepage: github.com/cabins

;;; Commentary:
;;; Code:

(require 'init-eglot)
(require 'init-elisp)
(require 'init-go)
(require 'init-python)
(require 'init-rust)
(require 'init-vue)
(require 'init-web)

(use-package json-mode)
(use-package markdown-mode)
(use-package protobuf-mode)
(use-package restclient
  :init
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))
(use-package yaml-mode)

(provide 'init-lang)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-lang.el ends here
