;;; init-common.el --- customized functions -*- lexical-binding: t -*-
;;; Commentary:
;; (c) Cabins Kong, 2022-

;;; Code:

;; customized functions
(require 'init-fn)

;; change Emacs default settings here, variables only (NOT include built-in packages)
(require 'init-system)

;; settings for Melpa/Elpa/GNU repos for Emacs package manager
(require 'init-elpa)

;; change default Emacs settings with built-in packages
(require 'init-builtin)

;; all the third-part packages configed here
(require 'init-package)

(provide 'init)

;;; init.el ends here
 ;; Local Variables:
;; byte-compile-warnings: (not unresolved obsolete)
;; End:


(provide 'init-common)
;;; init-common.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
