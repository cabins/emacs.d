;;; init-lang-go.el --- config for golang -*- lexical-binding: t -*-
;;; Commentary:
;; Thanks to Eglot, we just need to install the `go-mode'.
;;; Code:

;; Golang
(use-package go-mode)

;;; optional packages
;; (use-package go-fill-struct)
;; (use-package go-impl)
;; (use-package go-gen-test)
;; (use-package go-tag)

(provide 'init-lang-go)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-lang-go.el ends here
