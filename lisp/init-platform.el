;;; init-platform.el --- config for platform -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Special configs for MS-Windows
(when (and (memq system-type '(ms-dos windows-nt cygwin))
           (boundp 'w32-get-true-file-attributes))
  (setq w32-get-true-file-attributes nil
	    w32-pipe-read-delay 0
	    w32-pipe-buffer-size (* 64 1024)))

;; Special configs for macOS
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
	    mac-option-modifier 'super
	    ns-use-native-fullscreen t))

;; solve the Chinese paste issue
;; let Emacs auto-guess the selection coding according to the Windows/system settings
(unless (memq system-type '(ms-dos windows-nt cygwin))
  (set-selection-coding-system 'utf-8))

(provide 'init-platform)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; init-platform.el ends here
