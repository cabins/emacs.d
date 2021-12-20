;;; init-eglot.el --- config for eglot -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package eglot
  :hook ((c-mode c++-mode go-mode java-mode js-mode python-mode rust-mode web-mode) . eglot-ensure)
  :bind (("C-c e f" . #'eglot-format)
         ("C-c e i" . #'eglot-code-action-organize-imports)
         ("C-c e q" . #'eglot-code-action-quickfix))
  :config
  ;; (setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (defun eglot-actions-before-save()
    (add-hook 'before-save-hook (lambda ()
                                  (call-interactively #'eglot-format)
                                  (call-interactively #'eglot-code-action-organize-imports))))
  (add-to-list 'eglot-server-programs '(web-mode "vls"))
  (add-hook 'eglot--managed-mode-hook #'eglot-actions-before-save))

(add-hook 'java-mode-hook
          (lambda ()
            (let* ((jarpath "eglot/server/java/jdt-language-server-1.5.0-202110191539/plugins/org.eclipse.equinox.launcher_1.6.400.v20210924-0641.jar")
                   (eglot-server-java (expand-file-name jarpath user-emacs-directory)))
              (if (file-exists-p eglot-server-java)
                  (setenv "CLASSPATH" (concat (getenv "CLASSPATH") path-separator eglot-server-java))))))

(provide 'init-eglot)
;;; init-eglot.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
