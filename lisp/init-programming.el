;;; init-programming.el --- configurations for Programmers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Python
(add-to-list 'eglot-server-programs
             '((python-ts-mode python-mode) . ("uvx" "ty" "server")))

;; Rust
(use-package rust-mode :ensure t)

(provide 'init-programming)

;;; init-programming.el ends here
