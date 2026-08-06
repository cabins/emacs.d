;;; early-init.el --- Emacs 27 introduces early-init.el, which runs before init.el  -*- lexical-binding: t; -*-
;;; Commentary:
;; Runs before package and UI initializetion happens.
;;; Code:

(unless (>= emacs-major-version 31)
  (error "ONLY EMACS v31+ IS SUPPORTED!"))

;; For speed up the startup, please do NOT forget reset it to default
;; after Emacs after-init-hook, or it may cause freezes.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 8 100 100)
		  gc-cons-percentage 0.1)))

;; Prevent unwanted runtime compilation for native-comp users
;; (setq inhibit-automatic-native-compilation t)

;; No third-party packages by design: skip package.el initialization.
;; Set to t if you ever re-enable package management.
(setq package-enable-at-startup nil)

;; Clean GUI
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(load-theme 'modus-operandi t)

(provide 'early-init)

;;; early-init.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
