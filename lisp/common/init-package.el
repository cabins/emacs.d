;;; init-package.el --- initialize the plugins

;;; Commentary:
;; (c) Cabins Kong, 2022-

;;; Code:

;; Settings for company, auto-complete only for coding.
(use-package company
  :hook ((prog-mode . company-mode)
         (inferior-emacs-lisp-mode . company-mode))
  :config (setq company-minimum-prefix-length 1
                company-show-quick-access nil))

;; crux, a collection of many useful extensions/commands
;; without key-binding you can use
;; C-a for its original definition
;; M-m to the indentation of current line
;; C-M-<ARROW> for duplicate lines
;; crux commands? Pls use M-x.
(use-package crux)

;; Settings for exec-path-from-shell
;; fix the PATH environment variable issue
(use-package exec-path-from-shell
  :defer nil
  :when (or (memq window-system '(mac ns x))
            (unless (memq system-type '(windows-nt dos))
              (daemonp)))
  :init (exec-path-from-shell-initialize))

;; format all, formatter for almost languages
;; great for programmers
(use-package format-all
  :hook (prog-mode . format-all-ensure-formatter)
  :bind ("C-c f" . #'format-all-buffer))

;; gnu-elpa-keyring-update
(use-package gnu-elpa-keyring-update)

;; iedit - edit same text in one buffer or region
(use-package iedit)

;; info-colors, make the info manual as colorful
(use-package info-colors
  :hook (Info-selection . info-colors-fontify-node))

;; move-dup, move/copy line or region
(use-package move-dup
  :hook (after-init . global-move-dup-mode))

;; org-superstar
;; make the org mode more beautiful with optimized leading chars
(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config (setq org-superstar-prettify-item-bullets nil))

;; popwin
(use-package popwin
  :hook (after-init . popwin-mode))

;; Settings for which-key - suggest next key
(use-package which-key
  :hook (after-init . which-key-mode))

(provide 'init-package)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-package.el ends here
