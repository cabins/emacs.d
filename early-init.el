;;; early-init.el --- Emacs 27 introduces early-init.el, which runs before init.el
;;; Commentary:
;; Runs before package and UI initializetion happens.
;;; Code:

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
;; (setq package-enable-at-startup nil)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Load theme early to avoid the blinking
(load-theme 'wheatgrass)

;; Cleaner GUI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(fringe-mode 0)

;;; early-init.el ends here
