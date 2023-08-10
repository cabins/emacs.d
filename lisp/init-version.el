;;; init-version.el --- config for version -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Only the latest version is supported
(unless (>= emacs-major-version 28)
  (error "ONLY EMACS v28+ IS SUPPORTED!"))

(provide 'init-version)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; init-version.el ends here
