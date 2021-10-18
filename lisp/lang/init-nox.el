;;; init-nox.el --- settings for nox(lsp server) -*- lexical-binding: t -*-
;;; Commentary:

;; nox: https://github.com/manateelazycat/nox

;;; Code:

(unless (file-exists-p (concat user-emacs-directory "elpa/nox/nox.el"))
  (message "Installing nox...")
  (cd user-emacs-directory)
  (shell-command "git clone https://github.com/manateelazycat/nox elpa/nox")
  (message "Nox installed!"))

(add-to-list 'load-path (expand-file-name "elpa/nox" user-emacs-directory))

(require 'nox)
(dolist (hook '(go-mode-hook
		js-mode-hook
		rust-mode-hook
		python-mode-hook
		ruby-mode-hook
		java-mode-hook
		sh-mode-hook
		c-mode-common-hook
		c-mode-hook
		c++-mode-hook
		))
  (add-hook hook 'nox-ensure))

(provide 'init-nox)

;;; init-nox.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
