;;; init-third-packages --- settings for third-party packages (sorted by package names)
;;; Commentary:
;;; Code:

;; benchmark-init
(use-package benchmark-init
  :ensure t
  :config (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Settings for company, auto-complete only for coding.
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config (setq company-show-quick-access 'left
		company-minimum-prefix-length 1
		company-format-margin-function nil))

;; Settings for exec-path-from-shell
;; fix the PATH environment variable issue
(use-package exec-path-from-shell
  :ensure t
  :when (or (memq window-system '(mac ns x))
	    (unless cabins-os-win
	      (daemonp)))
  :init (exec-path-from-shell-initialize))

;; format all, formatter for almost languages
;; great for programmers
(use-package format-all :ensure t
  ;; enable format on save with format-all-mode
  ;; :hook ((prog-mode . format-all-mode)
  ;; 	   (format-all-mode . format-all-ensure-formatter))
  ;; and bind a shortcut to manual format
  :bind ("C-c f" . #'format-all-region-or-buffer))

;; iedit - edit same text in one buffer or region
(use-package iedit
  :ensure t
  :bind ("C-;" . iedit-mode))

;; move-dup, move/copy line or region
(use-package move-dup
  :ensure t
  :hook (after-init . global-move-dup-mode))

;; Markdown file support
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\markdown\\'" . markdown-mode)))

;; Protobuf file support
(use-package protobuf-mode
  :ensure t
  :mode "\\.proto\\'")

;; Run code
(use-package quickrun
  :ensure t
  :when (derived-mode-p 'prog-mode))

;; HTTP Request
(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode)))


(provide 'init-third-packages)

;;; init-third-packages.el ends here
