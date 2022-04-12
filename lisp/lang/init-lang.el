;;; init-lang.el --- configuration for IDE programming -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Homepage: github.com/cabins

;;; Commentary:
;;; Code:

;; Common features when programming
(add-hook 'prog-mode-hook
          (lambda ()
            (electric-layout-mode)
            (electric-pair-mode)
            (hs-minor-mode)
            (display-line-numbers-mode)))

;; Flymake
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :config
  (global-set-key (kbd "M-n") #'flymake-goto-next-error)
  (global-set-key (kbd "M-p") #'flymake-goto-prev-error))

;; cc-mode
(add-hook 'c-mode-common-hook 'c-toggle-auto-hungry-state)

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package highlight-parentheses
  :diminish
  :hook (prog-mode . highlight-parentheses-mode))

;; Yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all)
  :bind ("C-o" . yas-expand))
(use-package yasnippet-snippets :diminish)

;; Language Server
(require 'init-eglot) ; eglot
;; (require 'init-lsp)   ; lsp, enable this line if you like lsp-mode and disable eglot line

;; Specific Languages
(require 'init-lang-go)
(require 'init-lang-python)
(require 'init-lang-rust)
(require 'init-lang-web)

;; Program Useful text/config files
(use-package json-mode)
(use-package markdown-mode)
(use-package protobuf-mode)
(use-package yaml-mode)

;; Useful Tools
(use-package quickrun)                  ; quickrun code
(use-package restclient                 ; restclient support
  :mode (("\\.http\\'" . restclient-mode)))

(provide 'init-lang)

;;; init-lang.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
