;;; init-platform.el --- config for platform -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defvar os--windows (memq system-type '(ms-dos windows-nt cygwin)))
(defvar os--macos (eq system-type 'darwin))

(cond
 (os--windows
  ;; fix the  issue on Windows
  ;; (setq buffer-file-coding-system 'utf-8)
  (when (boundp 'w32-get-true-file-attributes)
    (setq w32-get-true-file-attributes nil
	      w32-pipe-read-delay 0
	      w32-pipe-buffer-size (* 64 1024))))
 (os--macos
  ;; <macOS> Command -> Meta, Option -> Super
  (setq mac-command-modifier 'meta
	    mac-option-modifier 'super
	    ns-use-native-fullscreen t))
 (t nil))

;; solve the Chinese paste issue
;; let Emacs auto-guess the selection coding according to the Windows/system settings
(unless os--windows
  (set-selection-coding-system 'utf-8))

(provide 'init-platform)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; init-platform.el ends here
