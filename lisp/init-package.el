;;; init-package.el --- initialize the plugins

;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:

;; All the icons
;; You may want to install the non-free font Symbola, when using Windows.
;; [Refs] https://github.com/seagle0128/doom-modeline
(use-package all-the-icons)

;; ace-window (install avy as dependency)
(use-package ace-window
  :init (bind-key "C-x o" #'ace-window))

;; Auto update packages
(use-package auto-package-update
  :init
  (setq auto-package-update-delete-old-versions t
	auto-package-update-hide-results t))

;; Never lose the cursor
(use-package beacon
  :init (add-hook 'after-init-hook 'beacon-mode))

;; Settings for company
(use-package company
  :diminish "Comp"
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0
	company-minimum-prefix-length 1))
(use-package company-posframe
  :diminish
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :init
  (add-hook 'company-mode-hook 'company-posframe-mode))

;; Ivy & Counsel & Swiper
(use-package counsel
  :diminish
  :init
  (add-hook 'after-init-hook 'ivy-mode)
  (bind-key "C-s" #'swiper)
  (bind-key "C-r" #'swiper-isearch-backward)
  (bind-key "M-x" #'counsel-M-x)
  (bind-key "C-x C-f" #'counsel-find-file)
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t))

;; crux
(use-package crux
  :init
  (bind-key "C-," #'crux-find-user-init-file)
  (bind-key "C-c C-d" #'crux-duplicate-current-line-or-region)
  (bind-key "C-a" #'crux-move-beginning-of-line))

;; diminish & delight
(use-package diminish)
(use-package delight)

;; drag stuff
(use-package drag-stuff
  :init
  (bind-key "M-<down>" #'drag-stuff-down)
  (bind-key "M-<up>" #'drag-stuff-up))

;; Settings for exec-path-from-shell
(use-package exec-path-from-shell
  :defer nil
  :if (memq window-system '(mac ns x))
  :init (exec-path-from-shell-initialize))

;; format all
(use-package format-all
  :diminish " Fmt"
  :init
  (add-hook 'prog-mode-hook 'format-all-mode)
  (bind-key "C-c f" #'format-all-buffer))

;; gnu-elpa-keyring-update
(use-package gnu-elpa-keyring-update)

;; hungry delete
(use-package hungry-delete
  :diminish
  :init
  (add-hook 'after-init-hook #'global-hungry-delete-mode))

;; iedit
(use-package iedit
  :init
  (bind-key "C-;" 'iedit-mode))

;; magit
(use-package magit)

;; olivetti
(use-package olivetti
  :init
  (setq olivetti-body-width .7)
  (add-hook 'org-mode-hook 'olivetti-mode))

;; org-bullets
(use-package org-bullets
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode))

;; pdf-tools
(use-package pdf-tools
  :init
  (add-hook 'after-init-hook 'pdf-tools-install))

;; popwin
(use-package popwin
  :init
  (add-hook 'after-init-hook 'popwin-mode))

;; powerline
(use-package powerline
  :commands (powerline-reset)
  :init
  ;; call powerline-reset after load new theme
  (advice-add 'load-theme :after (lambda (theme &rest args) (powerline-reset)))
  (advice-add 'disable-theme :after (lambda (theme) (powerline-reset)))
  (add-hook 'after-init-hook 'powerline-default-theme))

;; Show the delimiters as rainbow color
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
(use-package highlight-parentheses
  :diminish
  :init (add-hook 'prog-mode-hook 'highlight-parentheses-mode))

;; undo tree
(use-package undo-tree
  :diminish
  :init (add-hook 'after-init-hook 'global-undo-tree-mode))

;; Settings for which-key - suggest next key
(use-package which-key
  :diminish
  :init
  (add-hook 'after-init-hook 'which-key-mode))

;; Settings for yasnippet
(use-package yasnippet
  :diminish
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (bind-key "C-o" #'yas-expand))
(use-package yasnippet-snippets)

(provide 'init-package)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-package.el ends here
