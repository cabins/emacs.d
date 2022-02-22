;;; init-package.el --- initialize the plugins

;;; Commentary:
;; (c) Cabins Kong, 2022-

;;; Code:

;; Auto update packages
;; this maybe useful, if you want to update all the packages with command, just like me
(use-package auto-package-update
  :init (setq auto-package-update-delete-old-versions t
	          auto-package-update-hide-results t))

;; Settings for company, auto-complete for texting, coding, etc.
(use-package company
  :diminish "Cmp"
  :hook (after-init . global-company-mode)
  :config (setq company-minimum-prefix-length 1
                ;;company-echo-delay 0
                ;;company-show-numbers t
                company-show-quick-access t
                ))

;; ctrlf, good isearch alternative
(use-package ctrlf
  :hook (after-init . ctrlf-mode))

;; crux, a collection of many useful extensions/commands
(use-package crux
  :bind (("C-c C-d" . #'crux-duplicate-current-line-or-region)
	     ("C-a" . #'crux-move-beginning-of-line)))

;; Settings for exec-path-from-shell
;; fix the PATH environment variable issue
(use-package exec-path-from-shell
  :defer nil
  :when (or (memq window-system '(mac ns x))
            (unless (memq system-type '(ms-dos windows-nt))
              (daemonp)))
  :init
  (exec-path-from-shell-copy-env "CLASSPATH")
  (exec-path-from-shell-initialize))

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

;; move-dup, move/copy line or region
(use-package move-dup
  :hook (after-init . global-move-dup-mode))

;; neotree, file tree manager
(use-package neotree
  :commands (neo-buffer--lock-width neo-buffer--unlock-width)
  :config (setq neo-autorefresh t
		        neo-theme 'nerd
		        neo-click-changes-root t
		        neo-smart-open t)
  :bind ("<f8>" . neotree-toggle))

;; org-superstar
;; make the org mode more beautiful with optimized leading chars
(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-prettify-item-bullets t
        org-superstar-headline-bullets-list '("🐭" "🐮" "🐯" "🐰" "🐲" "🐸" "🐴" "🐑" "🐵" "🐔" "🐶" "🐷")))

;; popwin
(use-package popwin
  :hook (after-init . popwin-mode))

;; Show the delimiters as rainbow color
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package highlight-parentheses
  :diminish
  :hook (prog-mode . highlight-parentheses-mode))

;; Settings for which-key - suggest next key
(use-package which-key
  :diminish
  :hook (after-init . which-key-mode))

;; Settings for yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all)
  :bind ("C-o" . yas-expand))
(use-package yasnippet-snippets :diminish)

(provide 'init-package)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-package.el ends here