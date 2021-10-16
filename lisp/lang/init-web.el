;;; init-web.el --- config for web developemnt -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package web-mode
  :init (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  :config (setq web-mode-enable-current-element-highlight t))
;; use C-j to expand emmet
(use-package emmet-mode
  :init
  (add-hook 'web-mode-hook #'emmet-mode)
  (add-hook 'css-mode-hook #'emmet-mode))

(provide 'init-web)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-web.el ends here
