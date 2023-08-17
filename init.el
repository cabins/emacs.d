;;; init.el --- the entry of emacs config -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Github: https://github.com/cabins/emacs.d

;;; Commentary:
;; (c) Cabins Kong, 2022-

;;; Code:

;; Default directory location(Not necessary, but RECOMMENDED)
(setq default-directory "~/")
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; Customized functions
(require 'init-fn)

;; Third part packages
(require 'init-package)

;; Emacs builtin packages
(require 'init-builtin)

;;Configs for OS
(require 'init-platform)

;; Configs for programming languages
(require 'init-lang)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
    (load custom-file))

(provide 'init)

;;; init.el ends here
;;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not unresolved obsolete)
;; End:
