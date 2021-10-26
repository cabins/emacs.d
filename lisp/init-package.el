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
  :diminish
  :config
  (setq beacon-size 10
	beacon-blink-when-window-scrolls nil)
  :init (add-hook 'after-init-hook 'beacon-mode))

;; Settings for company
(use-package company
  :diminish "Cmp"
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0
	company-minimum-prefix-length 1))
(use-package company-prescient
  :init
  (add-hook 'company-mode-hook 'company-prescient-mode))

;; Ivy & Counsel & Swiper
(use-package counsel
  :config
  (diminish 'ivy-mode)
  :init
  (add-hook 'after-init-hook 'ivy-mode)
  (bind-key "C-s" #'swiper)
  (bind-key "C-r" #'swiper-isearch-backward)
  (bind-key "M-x" #'counsel-M-x)
  (bind-key "C-x C-f" #'counsel-find-file)
  (bind-key "C-c r" #'counsel-recentf)
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t))
(use-package ivy-prescient
  :init
  (add-hook 'ivy-mode-hook 'ivy-prescient-mode))

;; crux
(use-package crux
  :init
  (bind-key "C-," #'crux-find-user-init-file)
  (bind-key "C-c C-d" #'crux-duplicate-current-line-or-region)
  (bind-key "C-a" #'crux-move-beginning-of-line))

;; diminish & delight
(use-package diminish)
(use-package delight)

;; Settings for exec-path-from-shell
(use-package exec-path-from-shell
  :defer nil
  :if (memq window-system '(mac ns x))
  :init (exec-path-from-shell-initialize))

;; format all
(use-package format-all
  :diminish
  :init
  (add-hook 'prog-mode-hook 'format-all-mode)
  (add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
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

;; info-colors
(use-package info-colors
  :init
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

;; magit
(use-package magit)

;; marginalia: show description in minibuffer
(use-package marginalia
  :init
  (add-hook 'after-init-hook 'marginalia-mode))

;; move-text, move line or region with M-<up>/<down>
(use-package move-text
  :init
  (add-hook 'after-init-hook 'move-text-default-bindings))

;; neotree
(use-package neotree
  :config
  (setq neo-autorefresh t
	neo-theme 'nerd
	neo-click-changes-root t
	neo-smart-open t)
  :init
  (bind-key "<f8>" 'neotree-toggle))

;; olivetti
(use-package olivetti
  :init
  (setq olivetti-body-width .7)
  (add-hook 'org-mode-hook 'olivetti-mode))

;; org-bullets
(use-package org-superstar
  :init
  (add-hook 'org-mode-hook 'org-superstar-mode))

;; pdf-tools
(use-package pdf-tools
  :init
  (add-hook 'after-init-hook 'pdf-loader-install))

;; popwin
(use-package popwin
  :init
  (add-hook 'after-init-hook 'popwin-mode))

;; powerline
;; (use-package powerline
;;   :commands (powerline-reset)
;;   :init
;;   ;; call powerline-reset after load new theme
;;   (advice-add 'load-theme :after (lambda (theme &rest args) (powerline-reset)))
;;   (advice-add 'disable-theme :after (lambda (theme) (powerline-reset)))
;;   (add-hook 'after-init-hook 'powerline-default-theme))

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
  :diminish "Yas"
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (bind-key "C-o" #'yas-expand))
(use-package yasnippet-snippets :diminish)

(provide 'init-package)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-package.el ends here
