;;; init-elisp.el --- config for elisp -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Lisp
;; (use-package paredit
;;   :init
;;   (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))

;; lispy (iedit, hydra as dependency), if you like vim style
;; (use-package lispy
;;   :init
;;   (add-hook 'emacs-lisp-mode-hook 'lispy-mode))

;; maybe smartparens is enough, and NO C-, binding
(use-package smartparens
  :init
  (add-hook 'prog-mode-hook 'smartparens-strict-mode))

(provide 'init-elisp)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-elisp.el ends here
