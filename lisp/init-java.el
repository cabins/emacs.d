;;; init-java.el --- config for Java -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Java

(add-hook 'java-mode-hook
          (lambda ()
            (let* ((jarpath "eglot/server/java/plugins/org.eclipse.equinox.launcher_1.6.400.v20210924-0641.jar")
                   (eglot-server-java (expand-file-name jarpath user-emacs-directory)))
              (if (file-exists-p eglot-server-java)
                  (setenv "CLASSPATH"
                          (concat (getenv "CLASSPATH") path-separator eglot-server-java))))))

(provide 'init-java)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-java.el ends here
