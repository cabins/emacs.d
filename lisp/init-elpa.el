;;; init-elpa.el --- initialize the elpa repository -*- lexical-binding: t -*-
;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:

;; add melpa to package-archives
(require 'package)
(setq package-check-signature nil
      load-prefer-newer t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; don't bother with the initialize, although it may cause much startup time,
;; there's no way to avoid this if you use package.el instead of other package
;; manager, like straight.el
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; these code run only once, when use-package is not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; make use-package default behavior better
;; with `use-package-always-ensure' you won't need ":ensure t" all the time
;; with `use-package-always-defer' you won't need ":defer t" all the time
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-enable-imenu-support t
      use-package-expand-minimally t)
(require 'use-package)

;; diminish & delight, as use-package optional dependency
(use-package diminish)
(use-package delight)

(provide 'init-elpa)
;;; init-elpa.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
