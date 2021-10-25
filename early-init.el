;;; early-init.el --- Emacs 27 introduces early-init.el, which runs before init.el
;;; Commentary:
;; Runs before package and UI initializetion happens.
;;; Code:

;; Package initialize occurs automatically, before `user-init-file' is loaded
;; but after `early-init-file'. If you want to handle package initialization,
;; you can prevent Emacs from doing it early by uncomment next line!
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
