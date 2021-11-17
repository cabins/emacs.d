;;; init.el --- the entry of emacs config -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Version: 1.0
;; Homepage: https://github.com/cabins/tenon-emacs
;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:

;; set the startup default directory, not essential but recommended.
(setq default-directory "~/")

;; update load-path to make customized lisp codes work
(push (expand-file-name "lisp" user-emacs-directory) load-path)

(require 'init-fn)			;define the functions
(require 'init-system)			;better emacs configs
(require 'init-elpa)			;package initialize
(require 'init-builtin)			;better builtin packages
(require 'init-package)			;third-part packages
(require 'init-kbd)			;key bindings
(require 'init-lang)			;for programming

;; don't forget to load custom file at last
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file nil t))

(provide 'init)

;;; init.el ends here
;; Local Variables:
;; byte-compile-warnings: (not unresolved obsolete)
;; End:
