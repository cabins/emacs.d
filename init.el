;;; init.el --- the entry of emacs config -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Version: 1.0
;; Homepage: https://github.com/cabins

;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:

;;; trick for less start time
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

;; update load-path
(dolist (dir '("lisp" "lisp/lang"))
  (push (expand-file-name dir user-emacs-directory) load-path))

;; settings for independent packages and etc.
(require 'init-fn)
(require 'init-system)
(require 'init-elpa)
(require 'init-package)
(require 'init-builtin)
(require 'init-kbd)
(require 'init-lang)
(require 'init-ui)

;;; load custom file at last
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
