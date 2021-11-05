;;; init-eglot.el --- config for eglot -*- lexical-binding: nil -*-
;;; Commentary:

;;; Code:

(use-package eglot
  :config
  ;; (setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (add-to-list 'eglot-server-programs '(web-mode "vls"))
  :init
  (dolist (hook '(c-mode-hook
                  c++-mode-hook
                  go-mode-hook
                  java-mode-hook
                  js-mode-hook
                  python-mode-hook
                  rust-mode-hook
                  web-mode-hook))
    (add-hook hook 'eglot-ensure)
    (add-hook hook (lambda () (add-hook 'before-save-hook
                                        (lambda ()
                                          (call-interactively 'eglot-code-action-organize-imports)))))))

(add-hook 'java-mode-hook
          (lambda ()
            (let ((eglot-server-java (expand-file-name
                                      "eglot/server/java/jdt-language-server-1.5.0-202110191539/plugins/org.eclipse.equinox.launcher_1.6.400.v20210924-0641.jar"
                                      user-emacs-directory)))
              (if (file-exists-p eglot-server-java)
                  (setenv "CLASSPATH" (concat (getenv "CLASSPATH") ";" eglot-server-java))))))

(provide 'init-eglot)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-eglot.el ends here
