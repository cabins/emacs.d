;;; init-elisp.el --- config for elisp -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Lisp
;; (use-package paredit :init (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))

;; lispy (iedit, hydra as dependency)
(use-package lispy
  :init
  (add-hook 'emacs-lisp-mode-hook 'lispy-mode))

(provide 'init-elisp)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-elisp.el ends here
