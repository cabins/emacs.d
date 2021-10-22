;;; init-builtin.el --- initialize the builtin plugins -*- lexical-binding: t -*-
;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:

;; Abbrev
(setq-default abbrev-mode t)

;; auto save when lose focus
;; (add-hook 'focus-out-hook 'save-buffer)	; save current buffer
(add-hook 'focus-out-hook
	  (lambda () (save-some-buffers t))) ; save all opened buffers

;; auto revert
(add-hook 'after-init-hook 'global-auto-revert-mode)

;; Delete Behavior
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'after-init-hook 'delete-selection-mode)

;; Electric-Pair
(add-hook 'prog-mode-hook 'electric-indent-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'electric-layout-mode)

;; Flymake
(add-hook 'prog-mode-hook 'flymake-mode)
(remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)

;; HideShow Minor Mode
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Line Number
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Org Mode
(setq org-hide-leading-stars t
      org-startup-indented t)

;; Parentheses
(use-package paren
  :config
  (setq-default show-paren-style 'mixed
		show-paren-when-point-inside-paren t
		show-paren-when-point-in-periphery t)
  :init (add-hook 'prog-mode-hook 'show-paren-mode))

;; Recentf
(use-package recentf
  :config
  (setq-default recentf-max-menu-items 20
		recentf-max-saved-items 20
		recentf-auto-cleanup 'never)
  (add-to-list 'recentf-exclude '("~\/.emacs.d\/elpa\/")))

;; Diminish Builtins
(dolist (elem '(abbrev-mode eldoc-mode))
  (diminish elem))
(add-hook 'hs-minor-mode-hook (lambda () (diminish 'hs-minor-mode)))

(provide 'init-builtin)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-builtin.el ends here
