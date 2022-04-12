;;; init-builtin.el --- initialize the builtin plugins -*- lexical-binding: t -*-
;;; Commentary:
;; (c) Cabins Kong, 2022-

;;; Code:

;; use short answers for YES/NO ect.
(setq use-short-answers t)

;; make tab-width always 4
(setq-default tab-width 4)
;; only use spaces instead of TAB, use C-q TAB to input the TAB char
(setq-default indent-tabs-mode nil)

;; auto-fill-mode, Help by command or variable name
(add-hook 'after-init-hook 'auto-fill-mode)

;; auto revert
;; `global-auto-revert-mode' is provided by autorevert.el (builtin)
(add-hook 'after-init-hook 'global-auto-revert-mode)

;; auto save to the visited file
(add-hook 'after-init-hook 'auto-save-visited-mode)

;; Delete Behavior
;; `delete-selection-mode' is provided by delsel.el (builtin)
;; `delete-trailing-whitespace' is provided by simple.el (builtin)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'after-init-hook 'delete-selection-mode)

;; fido-mode
;; `fido-mode' is provided by icomplete.el
(add-hook 'after-init-hook 'fido-vertical-mode)
;; customized
(setq completions-detailed t
      completions-format 'one-column)

;; Flyspell
;; to use this package, you may install 'aspell' and dict by manual
;; for example, "pacman -S aspell" on archlinux
;; and "pacman -S pacman -S mingw64/mingw-w64-x86_64-aspell{,-en}" on msys2 (Windows)
;; for performance issue, do NOT use on Windows
(use-package flyspell
  :hook ((text-mode org-mode) . flyspell-mode))

;; ibuffer
(defalias 'list-buffers 'ibuffer)

;; minibuffer
(add-hook 'after-init-hook 'minibuffer-electric-default-mode)

;; modeline settings
;; column number is useless in most time, but useful when debug code.
(add-hook 'after-init-hook 'column-number-mode)
(setq mode-line-compact t)

;; Org Mode
(use-package org
  :ensure nil
  :config
  (setq org-hide-leading-stars t
        org-hide-emphasis-markers t
        org-startup-indented t))

;; Prettify Symbols
;; `global-prettify-symbols-mode' and `prettify-symbols-mode' are provided by prog-mode.el
(add-hook 'prog-mode-hook 'prettify-symbols-mode)

;; pulse the cursor line
(dolist (cmd '(recenter-top-bottom other-window))
  (advice-add cmd :after
              (lambda (&rest _) (pulse-momentary-highlight-one-line (point)))))

;; Recentf
(use-package recentf
  :hook (after-init . recentf-mode)
  :bind (("C-c r" . #'recentf-open-files))
  :config
  (setq-default recentf-max-menu-items 50
                recentf-max-saved-items 100)
  (add-to-list 'recentf-exclude '("~\/.emacs.d\/elpa\/")))

;; global visual line mode
(add-hook 'after-init-hook 'global-visual-line-mode)

;; windmove.el, use shift-<arrow key> to switch buffers
(use-package windmove
  :init (windmove-default-keybindings))

(provide 'init-builtin)

;;; init-builtin.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
