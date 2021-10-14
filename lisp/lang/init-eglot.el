;;; init-eglot.el --- config for eglot -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package eglot
  ;; :config
  ;; (setq eglot-ignored-server-capabilites '(:documentHighlightProvider))
  :init
  (dolist (hook '(go-mode-hook
		  python-mode-hook
		  rust-mode-hook
		  js-mode-hook))
    (add-hook hook 'eglot-ensure)))

(provide 'init-eglot)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-eglot.el ends here
