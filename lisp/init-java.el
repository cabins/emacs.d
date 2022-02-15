;;; init-java.el --- config for Java -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Java

;; define the jdt-language-server manually
(defun eglot-server-java-setup ()
  (setq eglot-server-java
        (expand-file-name
         "eglot/server/java/plugins/org.eclipse.equinox.launcher_1.6.400.v20210924-0641.jar"
         user-emacs-directory))

  (if (file-exists-p eglot-server-java)
      (setenv "CLASSPATH"
              (concat (getenv "CLASSPATH") path-separator eglot-server-java))))

(add-hook 'java-mode-hook #'eglot-server-java-setup)

(provide 'init-java)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-java.el ends here
