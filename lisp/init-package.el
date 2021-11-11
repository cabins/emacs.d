;;; init-package.el --- initialize the plugins

;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:

;; All the icons
;; You may want to install the non-free font Symbola, when using Windows.
;; [Refs] https://github.com/seagle0128/doom-modeline
(use-package all-the-icons)

;; async
(use-package async)

;; Auto update packages
;; this maybe useful, if you want to update all the packages with command, just like me
(use-package auto-package-update
  :init (setq auto-package-update-delete-old-versions t
	      auto-package-update-hide-results t))

;; Settings for company, auto-complete for texting, coding, etc.
(use-package company
  :diminish "Cmp"
  :hook (after-init . global-company-mode)
  :config (setq company-minimum-prefix-length 1))
(use-package company-prescient
  :hook (company-mode . company-prescient-mode))

;; ctrlf, good isearch alternative
(use-package ctrlf
  :hook (after-init . ctrlf-mode))

;; make fido vertical style
(use-package ido-vertical-mode
  :hook ((after-init . fido-mode)
         (fido-mode . fido-vertical-mode)))

;; crux, a collection of many useful extensions/commands
(use-package crux
  :bind (("C-c C-d" . #'crux-duplicate-current-line-or-region)
	 ("C-a" . #'crux-move-beginning-of-line)))

;; Settings for exec-path-from-shell
;; fix the PATH environment variable issue
(use-package exec-path-from-shell
  :defer nil
  :if (memq window-system '(mac ns x))
  :init (exec-path-from-shell-initialize))

;; focus mode
(use-package focus)

;; format all, formatter for almost languages
;; great for programmers
(use-package format-all
  :diminish
  :hook (prog-mode . format-all-ensure-formatter)
  :bind ("C-c f" . #'format-all-buffer))

;; gnu-elpa-keyring-update
(use-package gnu-elpa-keyring-update)

;; hungry delete, delete many spaces as one
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode))

;; iedit
(use-package iedit
  :bind ("C-M-;" . iedit-mode))

;; info-colors, make the info manual as colorful
(use-package info-colors
  :hook (Info-selection . info-colors-fontify-node))

;; magit
(use-package magit)

;; marginalia: show description in mini buffer
(use-package marginalia
  :hook (after-init . marginalia-mode))

;; move-text, move line or region with M-<up>/<down>
(use-package move-text
  :hook (after-init . move-text-default-bindings))

;; neotree, file tree manager
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

;; org-superstar
;; make the org mode more beautiful with optimized leading chars
(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

;; pdf-tools, open pdf files in Emacs
;; it'll build the essential dependencies automatically
;; if you use Windows, prefer to msys2 installed
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
