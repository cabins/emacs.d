;;; init.el --- the entry of emacs config -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Github: https://github.com/cabins/emacs.d

;;; Commentary:
;; (c) Cabins Kong, 2022-

;;; Code:

;; set the startup default directory, not essential but recommended.
(setq default-directory "~/")

;; update load-path to make customized lisp codes work
(dolist (folder (directory-files (concat user-emacs-directory "lisp") t directory-files-no-dot-files-regexp))
  (add-to-list 'load-path folder))

;; customized functions
(require 'init-fn)

;; change Emacs default settings here, variables only (NOT include built-in packages)
(require 'init-system)

;; settings for Melpa/Elpa/GNU repos for Emacs package manager
(require 'init-elpa)

;; change default Emacs settings with built-in packages
(require 'init-builtin)

;; all the third-part packages configed here
(require 'init-package)

;; different settings depends on os platform
(require 'init-platform)

;; settings for programming languages (include IDE/LSP feature)
(require 'init-lang)

;; other features, such as UI/daemon etc.
(require 'init-feature)

;; DON'T forget to define and load custom file at last
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
    (load custom-file))

(provide 'init)

;;; init.el ends here
;;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not unresolved obsolete)
;; End:
