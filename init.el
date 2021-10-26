;;; init.el --- the entry of emacs config -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Version: 1.0
;; Homepage: https://github.com/cabins/tenon-emacs

;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:
(setq default-directory "~/")

;; update load-path
(push (expand-file-name "lisp" user-emacs-directory) load-path)

;; settings for independent packages and etc.
(require 'init-fn)
(require 'init-system)
(require 'init-elpa)
(require 'init-package)
(require 'init-builtin)
(require 'init-kbd)
(require 'init-lang)

;; load custom file at last
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
;; Local Variables:
;; byte-compile-warnings: (not unresolved obsolete)
;; End:
