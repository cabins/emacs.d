;;; init-elpa.el --- initialize the elpa repository -*- lexical-binding: t -*-
;;; Commentary:
;; (c) Cabins Kong, 2022-

;;; Code:

;; add melpa to package-archives
(use-package package
  :ensure nil
  :demand t
  :defer nil
  :config
  ;; (setq package-check-signature nil)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  :init
  (unless (bound-and-true-p package--initialized)
    (package-initialize)))

;; make use-package default behavior better
;; with `use-package-always-ensure' you won't need ":ensure t" all the time
;; with `use-package-always-defer' you won't need ":defer t" all the time
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-enable-imenu-support t
      use-package-expand-minimally t)

(provide 'init-elpa)
;;; init-elpa.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
