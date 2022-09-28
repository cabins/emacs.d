;;; init-lsp-bridge.el --- config for eglot -*- lexical-binding: t -*-
;;; Commentary:
;;
;; If you don't like eglot/lsp-mode for specific languages, some alternatives are:
;; - `java-mode' with `meghanada-mode' & `meghanada-server'
;; - `python-mode' with `elpy'

;;; Code:

(use-package lsp-bridge
  :ensure nil
  :hook ((c-mode c++-mode go-mode java-mode js-mode python-mode rust-mode web-mode) . lsp-bridge-mode)
  :bind (("C-c e a" . #'lsp-bridge-code-action))
  :config
  (defun lsp-bridge-actions-before-save()
    (add-hook 'before-save-hook #'lsp-bridge-code-format))
  (add-hook 'lsp-bridge-mode-hook #'lsp-bridge-actions-before-save))

(provide 'init-lsp-bridge)
;;; init-lsp-bridge.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
