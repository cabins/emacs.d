;;; init-system.el --- configs for startup -*- lexical-binding: t -*-
;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:

;;; Emacs 28 native compile
(when (and (fboundp 'native-comp-available-p)
	   (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil)
  (add-to-list 'native-comp-eln-load-path
	       (expand-file-name "eln-cache" user-emacs-directory)))

;;; flymake cannot find load-path solution
;; [refs] https://emacs-china.org/t/flymake/8323/19
(setq elisp-flymake-byte-compile-load-path
      (append elisp-flymake-byte-compile-load-path load-path))

;;; system coding
(set-language-environment "utf-8")
(prefer-coding-system 'utf-8)
(unless (memq system-type '(cygwin windows-nt ms-dos))
  (setq selection-coding-system 'utf-8))

;;; misc
(setq auto-save-default nil	   ; disable auto save
      delete-by-moving-to-trash t  ; disable delete directly
      inhibit-startup-screen t	   ; disable the startup screen splash
      inhibit-default-init t
      inhibit-compacting-font-caches t
      initial-major-mode 'text-mode
      make-backup-files nil             ; disable backup file
      read-process-output-max (* 64 1024)
      visible-bell nil)

;; settings for scrolling
(setq-default scroll-step 1
	      scroll-preserve-screen-position t
	      scroll-up-aggressively 0.01
	      scroll-down-aggressively 0.01
	      redisplay-dont-pause t
	      auto-window-vscroll nil
	      ;; Mouse wheel scroll behavior
	      mouse-wheel-scroll-amount '(1 ((shift) . 1))
	      mouse-wheel-progressive-speed nil
	      mouse-wheel-follow-mouse 't
	      fast-but-imprecise-scrolling nil)

;;; macOS special settings
;; <macOS> Command -> Meta, Option -> Super
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
	mac-option-modifier 'super))

(provide 'init-system)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-system.el ends here
