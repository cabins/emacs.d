;;; init-rust.el --- config for rust -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package rust-mode
  :config
  (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run))

(provide 'init-rust)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-rust.el ends here
