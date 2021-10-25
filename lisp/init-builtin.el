;;; init-builtin.el --- initialize the builtin plugins -*- lexical-binding: nil -*-
;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:

;; Abbrev
(setq-default abbrev-mode t)

;; auto save
;; auto save when frame lose focus, such as Alt-TAB
(add-function :after after-focus-change-function
	      (lambda () (save-some-buffers t)))
;; auto save when buffer changed
(mapc (lambda (command)
	(advice-add command :after
		    (lambda (&rest arg) (save-some-buffers t))))
      '(switch-to-buffer		;builtin
	other-window			;builtin
	windmove-do-window-select	;windmove-mode, builtin
	next-buffer			;builtin
	previous-buffer			;builtin
	ivy-switch-buffer		;ivy action
	aw-select))			;ace-window action

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
  :ensure nil
  :config
  (setq-default show-paren-style 'mixed
		show-paren-when-point-inside-paren t
		show-paren-when-point-in-periphery t)
  :init (add-hook 'prog-mode-hook 'show-paren-mode))

;; Recentf
(use-package recentf
  :defer 1
  :ensure nil
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

;;; init-builtin.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
