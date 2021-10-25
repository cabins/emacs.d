;;; init-elpa.el --- initialize the elpa repository -*- lexical-binding: t -*-
;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:

;;; add melpa to package-archives
;; (setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
;; ("melpa" . "http://melpa.org/packages/")))

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(setq package-check-signature nil
      load-prefer-newer t)

;; settings for use-package package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-enable-imenu-support t
      use-package-expand-minimally t)

(eval-when-compile
  (require 'use-package))

(provide 'init-elpa)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-elpa.el ends here
