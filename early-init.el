;;; early-init.el --- Emacs 27 introduces early-init.el, which runs before init.el
;;; Commentary:
;; Runs before package and UI initializetion happens.
;;; Code:

(unless (>= emacs-major-version 29)
  (error "ONLY EMACS v29+ IS SUPPORTED!"))

;; For speed up the startup, please do NOT forget reset it to default
;; after Emacs after-init-hook, or it may cause freezes.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold 800000)))

;; Prevent unwanted runtime compilation for native-comp users
;; (setq inhibit-automatic-native-compilation t)

;; Package initialize occurs automatically, before `user-init-file' is loaded
;; but after `early-init-file'. If you want to handle package initialization,
;; you can prevent Emacs from doing it early by uncomment next line!
(setq package-enable-at-startup nil)

;; Clean GUI
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(provide 'early-init)

;;; early-init.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; END:
