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
  :bind ("C-x o" . ace-window))

;; Auto update packages
(use-package auto-package-update
  :init (setq auto-package-update-delete-old-versions t
	      auto-package-update-hide-results t))

;; Never lose the cursor
(use-package beacon
  :diminish
  :config (setq beacon-size 10
		beacon-blink-when-window-scrolls nil)
  :hook (after-init . beacon-mode))

;; Settings for company
(use-package company
  :diminish "Cmp"
  :hook (after-init . global-company-mode)
  :config (setq company-idle-delay 0
		company-minimum-prefix-length 1))
(use-package company-prescient
  :hook (company-mode . company-prescient-mode))

;; Ivy & Counsel & Swiper
(use-package counsel
  :hook (after-init . ivy-mode)
  :bind (( "C-s" . swiper)
	 ( "C-r" . swiper-isearch-backward)
	 ( "M-x" . counsel-M-x)
	 ( "C-x C-f" . counsel-find-file)
	 ( "C-c r" . counsel-recentf))
  :init
  (add-hook 'ivy-mode-hook (lambda () (diminish 'ivy-mode)))
  (setq enable-recursive-minibuffers t))
(use-package ivy-prescient
  :hook (ivy-mode . ivy-prescient-mode))

;; crux
(use-package crux
  :bind (("C-c C-d" . #'crux-duplicate-current-line-or-region)
	 ("C-a" . #'crux-move-beginning-of-line)))

;; Settings for exec-path-from-shell
(use-package exec-path-from-shell
  :defer nil
  :if (memq window-system '(mac ns x))
  :init (exec-path-from-shell-initialize))

;; format all
(use-package format-all
  :diminish
  :hook ((prog-mode . format-all-mode)
	 (format-all-mode . format-all-ensure-formatter))
  :bind ("C-c f" . #'format-all-buffer))

;; gnu-elpa-keyring-update
(use-package gnu-elpa-keyring-update)

;; hungry delete
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode))

;; iedit
(use-package iedit
  :bind ("C-M-;" . iedit-mode))

;; info-colors
(use-package info-colors
  :hook (Info-selection . info-colors-fontify-node))

;; magit
(use-package magit)

;; marginalia: show description in minibuffer
(use-package marginalia
  :hook (after-init . marginalia-mode))

;; move-text, move line or region with M-<up>/<down>
(use-package move-text
  :hook (after-init . move-text-default-bindings))

;; neotree
(use-package neotree
  :config (setq neo-autorefresh t
		neo-theme 'nerd
		neo-click-changes-root t
		neo-smart-open t)
  :bind ("<f8>" . neotree-toggle))

;; olivetti
(use-package olivetti
  :init (setq olivetti-body-width .7)
  :hook (org-mode . olivetti-mode))

;; org-bullets
(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

;; pdf-tools
(use-package pdf-tools
  :hook (after-init . pdf-loader-install))

;; popwin
(use-package popwin
  :hook (after-init . popwin-mode))

;; Show the delimiters as rainbow color
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package highlight-parentheses
  :diminish
  :hook (prog-mode . highlight-parentheses-mode))

;; undo tree
(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode))

;; Settings for which-key - suggest next key
(use-package which-key
  :diminish
  :hook (after-init . which-key-mode))

;; Settings for yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :bind ("C-o" . yas-expand))
(use-package yasnippet-snippets :diminish)

(provide 'init-package)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-package.el ends here
