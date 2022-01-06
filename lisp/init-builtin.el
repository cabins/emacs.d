;;; init-builtin.el --- initialize the builtin plugins -*- lexical-binding: t -*-
;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:

;; alias for yes-or-no-pair
(defalias 'yes-or-no-p 'y-or-n-p)

;; abbrev mode, as it is written in C, we can't configure it with
;; use-package with 'ensure nil' flag
(use-package abbrev
  :ensure nil
  :diminish abbrev-mode
  :init (setq-default abbrev-mode t))

;; auto save
;; `save-some-buffers' is provided by files.el (builtin)
;; `pulse-momentary-highlight-one-line' is provided by pulse.el (builtin)
(use-package pulse-and-save
  :ensure nil
  :init
  (defun pulse-save-buffers (&rest args)
    (save-some-buffers t)
    (pulse-momentary-highlight-one-line (point)))
  ;; auto save when frame lose focus, Alt-Tab
  (add-function :after after-focus-change-function #'pulse-save-buffers)
  ;; auto save when buffer changed
  (dolist (command '(other-window
                     switch-to-buffer
                     next-buffer
                     previous-buffer))
    (advice-add command :after #'pulse-save-buffers)))

;; auto revert
;; `global-auto-revert-mode' is provided by autorevert.el (builtin)
(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

;; comment or uncomment, provided by `newcomment.el'
(use-package newcomment
  :ensure nil
  :config
  (global-set-key (kbd "C-c C-;") #'comment-or-uncomment-region))

;; Delete Behavior
;; `delete-selection-mode' is provided by delsel.el (builtin)
;; `delete-trailing-whitespace' is provided by simple.el (builtin)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'after-init-hook 'delete-selection-mode)

;; Electric-Pair
;; default value of `electric-indent-mode' is t
(use-package electric
  :hook ((prog-mode . electric-layout-mode)
         (prog-mode . electric-pair-mode)))

;; fido-mode
;; `fido-mode' is provided by icomplete.el
(use-package icomplete
  :hook (after-init . fido-mode))

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
  :unless (memq system-type '(windows-nt dos cygwin))
  :hook ((text-mode org-mode) . flyspell-mode))

;; HideShow Minor Mode
(use-package hideshow
  :init (add-hook 'hs-minor-mode-hook (lambda () (diminish 'hs-minor-mode)))
  :hook (prog-mode . hs-minor-mode))

;; ibuffer
(use-package ibuffer
  :init (defalias 'list-buffers 'ibuffer))

;; Line Number
;; this package introduced in Emacs 26, so only enabled when 26+
(use-package display-line-numbers
  :if (> emacs-major-version 26)
  :hook (prog-mode . display-line-numbers-mode))

;; modeline settings
;; column number is useless in most time, but useful when debug code.
(add-hook 'after-init-hook 'column-number-mode)

;; Org Mode
(use-package org
  :ensure nil
  :config
  (setq org-hide-leading-stars t
        org-startup-indented t))

;; Prettify Symbols
;; `global-prettify-symbols-mode' is provided by prog-mode.el
(use-package prog-mode
  :ensure nil
  :config (global-prettify-symbols-mode t))

;; Recentf
(use-package recentf
  :hook (after-init . recentf-mode)
  :bind (("C-c r" . #'recentf-open-files))
  :config
  (setq-default recentf-max-menu-items 50
                recentf-max-saved-items 100)
  (add-to-list 'recentf-exclude '("~\/.emacs.d\/elpa\/")))

;; only use spaces instead of TAB, use C-q TAB to input the TAB char
(setq-default indent-tabs-mode nil)

;; windmove.el, use shift-<arrow key> to switch buffers
(use-package windmove
  :init (windmove-default-keybindings))

(provide 'init-builtin)

;;; init-builtin.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
