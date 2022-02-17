;;; init-lang-web.el --- config for web developemnt -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; use C-j to expand emmet
(use-package emmet-mode
  :hook ((web-mode css-mode) . emmet-mode))

(use-package web-mode
  :init
  ;; use web-mode to handle vue/html files
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  :config
  (setq web-mode-enable-current-element-highlight t))

(use-package typescript-mode)

(provide 'init-lang-web)

;;; init-lang-web.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
