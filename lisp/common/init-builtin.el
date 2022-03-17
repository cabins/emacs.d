;;; init-builtin.el --- initialize the builtin plugins -*- lexical-binding: t -*-
;;; Commentary:
;; (c) Cabins Kong, 2022-

;;; Code:

;; alias for yes-or-no-pair
(defalias 'yes-or-no-p 'y-or-n-p)

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

;; settings with cc-mode
(add-hook 'c-mode-common-hook 'c-toggle-auto-hungry-state)

;; Delete Behavior
;; `delete-selection-mode' is provided by delsel.el (builtin)
;; `delete-trailing-whitespace' is provided by simple.el (builtin)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'after-init-hook 'delete-selection-mode)

;; Electric-Pair
;; default value of `electric-indent-mode' is t
(add-hook 'prog-mode-hook 'electric-layout-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; fido-mode
;; `fido-mode' is provided by icomplete.el
;;(add-hook 'after-init-hook 'fido-mode)
(if (fboundp 'fido-vertical-mode)
    (add-hook 'after-init-hook 'fido-vertical-mode)
  (add-hook 'after-init-hook 'fido-mode))

;; Flymake
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :config
  (global-set-key (kbd "M-n") #'flymake-goto-next-error)
  (global-set-key (kbd "M-p") #'flymake-goto-prev-error))

;; Flyspell
;; to use this package, you may install 'aspell' and dict by manual
;; for example, "pacman -S aspell" on archlinux
;; and "pacman -S pacman -S mingw64/mingw-w64-x86_64-aspell{,-en}" on msys2 (Windows)
;; for performance issue, do NOT use on Windows
(use-package flyspell
  :hook ((text-mode org-mode) . flyspell-mode))

;; HideShow Minor Mode
(use-package hideshow
  :init (add-hook 'hs-minor-mode-hook (lambda () (diminish 'hs-minor-mode)))
  :hook (prog-mode . hs-minor-mode))

;; ibuffer
(defalias 'list-buffers 'ibuffer)

;; Line Number
;; this package introduced in Emacs 26, so only enabled when 26+
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; minibuffer
(add-hook 'after-init-hook 'minibuffer-electric-default-mode)

;; modeline settings
;; column number is useless in most time, but useful when debug code.
(add-hook 'after-init-hook 'column-number-mode)

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
