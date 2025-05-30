;;; init.el --- the entry of emacs config -*- lexical-binding: t -*-
;; Author: Cabins
;; Github: https://github.com/cabins-emacs.d
;;; Commentary: (c) Cabins Kong, 2022-
;;; Code:

;; variables definition
(defvar cabins-os-win (memq system-type '(ms-dos windows-nt cygwin)))
(defvar cabins-os-mac (eq system-type 'darwin))

;; font settings
(defvar font-name "Maple Mono Normal NF CN")
(when (find-font (font-spec :family font-name))
  (set-face-attribute 'default nil :family font-name))

;; pre-settings
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; customized functions
(require 'init-functions)

;; builtin settings
(require 'init-builtins)

;; third-part packages
(require 'init-third-packages)

;; custom file settings
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
;;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not unresolved obsolete)
;; End:
