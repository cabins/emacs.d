;;; init-lang-rust.el --- config for rust -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package rust-mode
  :config
  (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run))

(provide 'init-lang-rust)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-lang-rust.el ends here
