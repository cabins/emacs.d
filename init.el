;;; init.el --- the entry of emacs config -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Github: https://github.com/cabins/emacs.d

;;; Commentary:
;; (c) Cabins Kong, 2022-

;;; Code:

;; Default directory location(Not necessary, but RECOMMENDED)
(setq default-directory "~/")

;; All the start config files, which will be autoloaded
(dolist (folder (directory-files (concat user-emacs-directory "lisp") t directory-files-no-dot-files-regexp))
  (add-to-list 'load-path folder))

;; Customized functions
(require 'init-fn)

;; Some essential configs according to different OS
(require 'init-system)

;; Package manager configs
(require 'init-elpa)

;; Emacs builtin packages
(require 'init-builtin)

;; Third part packages
(require 'init-package)

;;Configs for OS
(require 'init-platform)

;; Configs for programming languages
(require 'init-lang)

;; Miscellaneous configs
(require 'init-feature)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
    (load custom-file))

(provide 'init)

;;; init.el ends here
;;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not unresolved obsolete)
;; End:
