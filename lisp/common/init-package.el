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
                company-show-quick-access t))

;; ctrlf, good isearch alternative
(use-package ctrlf
  :hook (after-init . ctrlf-mode))

;; crux, a collection of many useful extensions/commands
;; without key-binding you can use
;; C-a for its original definition
;; M-m to the indentation of current line
;; C-M-<ARROW> for duplicate lines
;; crux commands? Pls use M-x.
(use-package crux)

;; Settings for exec-path-from-shell
;; fix the PATH environment variable issue
(use-package exec-path-from-shell
  :defer nil
  :when (or (memq window-system '(mac ns x))
            (unless (memq system-type '(windows-nt dos))
              (daemonp)))
  :init
  ;; (exec-path-from-shell-copy-env "CLASSPATH")
  ;; (exec-path-from-shell-copy-env "JAVA_HOME")
  (exec-path-from-shell-initialize))

;; format all, formatter for almost languages
;; great for programmers
(use-package format-all
  :diminish
  :hook (prog-mode . format-all-ensure-formatter)
  :bind ("C-c f" . #'format-all-buffer))

;; gnu-elpa-keyring-update
(use-package gnu-elpa-keyring-update)

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

;; pyim - Chinese input methods
(use-package pyim
  :init
  (setq default-input-method "pyim")
  :config
  ; add flypy method
  (pyim-scheme-add
   '(flypy
     :document "小鹤音形输入法"
     :class xingma
     :first-chars "abcdefghijklmnopqrstuvwxyz"
     :rest-chars "abcdefghijklmnopqrstuvwxyz'"
     :code-prefix "flypy/" ;词库中所有的 code 都以 "flypy/" 开头，防止和其它词库冲突。
     :code-split-length 4 ;默认将用户输入切成 4 个字符长的 code 列表（不计算 code-prefix）
     :code-maximum-length 4 ;词库中，code 的最大长度（不计算 code-prefix）
     :prefer-triggers nil))
  (pyim-extra-dicts-add-dict
   '(:name "flypy" :file (concat user-emacs-directory "pyim-flypy.pyim")))
  (define-key pyim-mode-map ";"
              (lambda ()
                (interactive)
                (pyim-select-word-by-number 2)))
  (pyim-default-scheme 'flypy))

;; Settings for which-key - suggest next key
(use-package which-key
  :diminish
  :hook (after-init . which-key-mode))

(provide 'init-package)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-package.el ends here
