;;; init-version.el --- config for version -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(when (< emacs-major-version 26)
  (error "Configuration error! Requires Emacs 26+!"))

;; Settings for Emacs 28+ with native-compilation
(when (and (fboundp 'native-comp-available-p)
	       (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil
	    comp-deferred-compilation t
	    package-native-compile t)
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache" user-emacs-directory)))

(provide 'init-version)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; init-version.el ends here
