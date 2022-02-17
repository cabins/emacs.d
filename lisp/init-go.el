;;; init-go.el --- config for golang -*- lexical-binding: t -*-
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

(provide 'init-go)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-go.el ends here
