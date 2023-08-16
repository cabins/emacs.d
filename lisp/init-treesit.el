;;; init-treesit.el --- configuration for ts-mode -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Homepage: github.com/cabins
;;; Commentary:
;;; Code:

(defun gh-repo (short-name)
  "Get the full path of Github url from SHORT-NAME."

  (concat "https://github.com/" short-name))

(use-package treesit
  :ensure nil
  :demand t
  :when (treesit-available-p)
  :custom
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
  (add-to-list 'major-mode-remap-alist '(makefile-mode . cmake-ts-mode))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))
  )

(use-package dockerfile-ts-mode
  :ensure nil
  :demand t
  :after (treesit))

(use-package go-ts-mode
  :ensure nil
  :demand t
  :after (treesit)
  :config
  (add-to-list 'treesit-language-source-alist '(gomod . ((gh-repo "camdencheek/tree-sitter-go-mod")))))

(use-package rust-ts-mode
  :ensure nil
  :demand t
  :after (treesit))

(use-package typescript-ts-mode
  :ensure nil
  :demand t
  :after (treesit))

(use-package yaml-ts-mode
  :ensure nil
  :demand t
  :after (treesit))

(provide 'init-treesit)

;;; init-treesit.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
