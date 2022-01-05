;;; init-system.el --- configs for startup -*- lexical-binding: t -*-
;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:

;;; Emacs 28 native compile
(when (and (>= emacs-major-version 28)
	   (fboundp 'native-comp-available-p)
	   (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil)
  (setq package-native-compile t)
  (add-to-list 'native-comp-eln-load-path
	       (expand-file-name "eln-cache" user-emacs-directory)))

;;; flymake cannot find load-path solution
;; [refs] https://emacs-china.org/t/flymake/8323/19
(setq elisp-flymake-byte-compile-load-path
      (append elisp-flymake-byte-compile-load-path load-path))

;;; system coding
;; although others may add many other settings here,
;; but I think the next line is enough
(prefer-coding-system 'utf-8)

;;; emacs settings
(setq auto-save-default nil	   ; disable auto save
      auto-window-vscroll nil
      delete-by-moving-to-trash t  ; disable delete directly
      fast-but-imprecise-scrolling t
      frame-title-format "%b"
      help-window-select t
      inhibit-startup-screen t	   ; disable the startup screen splash
      inhibit-default-init t
      ;; initial-scratch-message nil
      inhibit-compacting-font-caches t
      make-backup-files nil             ; disable backup file
      ;; Mouse wheel scroll behavior
      ;; mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      next-line-add-newlines nil
      read-process-output-max (* 64 1024)
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      visible-bell nil)

;;; macOS special settings
;; <macOS> Command -> Meta, Option -> Super
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
	mac-option-modifier 'super
	ns-use-native-fullscreen t))

;;; Windows special settings
(when (boundp 'w32-get-true-file-attributes)
  (setq w32-get-true-file-attributes nil
	w32-pipe-read-delay 0
	w32-pipe-buffer-size (* 64 1024)))

;; solve the Chinese paste issue
;; let Emacs auto guess the selection coding according to the Windows settings
(unless (memq system-type '(cygwin windows-nt ms-dos))
  selection-coding-system 'utf-8)

;; daemon mode
;; after make frame with daemon mode, the font reset is needed.
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
		(with-selected-frame frame
		  (tenon/reset-font-setup)
                  (load-theme 'kaolin-dark t))))
  (tenon/reset-font-setup))

(provide 'init-system)
;;; init-system.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
