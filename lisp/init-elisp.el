;;; init-elisp.el --- config for elisp -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Lisp
;; (use-package paredit
;;   :init
;;   (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))

;; parinfer rust mode, keeps indent & parentheses balanced
;; paredit-like without hotkeys
;; (use-package parinfer-rust-mode
;;   :hook emacs-lisp-mode
;;   :init (setq parinfer-rust-auto-download t))

;; maybe smartparens is enough, and NO C-, binding
;; (use-package smartparens
;;   :init
;;   (add-hook 'prog-mode-hook 'smartparens-mode))

(provide 'init-elisp)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-elisp.el ends here
