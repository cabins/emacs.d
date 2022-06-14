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

;; common settings (no dependecies with version/os)
(require 'init-common)

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
 ;; Local Variables:
;; byte-compile-warnings: (not unresolved obsolete)
;; End:
