;;; init-elpa.el --- initialize the elpa repository -*- lexical-binding: t -*-
;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:

;;; settings for package archives
(setq package-archives '(("melpa" . "http://mirrors.bfsu.edu.cn/elpa/melpa/")
                         ("gnu" . "http://mirrors.bfsu.edu.cn/elpa/gnu/"))
      package-check-signature nil
      load-prefer-newer t)

(eval-when-compile
  (require 'package)
  ;; initialize the packages, avoiding a re-initialization
  (unless (bound-and-true-p package--initialized)
    (package-initialize))
  ;; settings for use-package package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (setq use-package-always-ensure t
	use-package-always-defer t
	use-package-enable-imenu-support t
	use-package-expand-minimally t)
  (require 'use-package))

(provide 'init-elpa)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-elpa.el ends here
