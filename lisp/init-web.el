;;; init-web.el --- config for web developemnt -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package typescript-mode)

;; (use-package vue-mode
;;   ;; disable the ugly background color
;;   ;; [refs] https://github.com/AdamNiederer/vue-mode#how-do-i-disable-that-ugly-background-color
;;   :config (set-face-background 'mmm-default-submode-face nil))

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  ;; use web-mode to handle vue file
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  :config (setq web-mode-enable-current-element-highlight t))

;; use C-j to expand emmet
(use-package emmet-mode
  :init
  (add-hook 'web-mode-hook #'emmet-mode)
  (add-hook 'css-mode-hook #'emmet-mode))

(provide 'init-web)

;;; init-web.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
