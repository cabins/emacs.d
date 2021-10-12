;;; init-python.el --- config for python -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defmacro check-run-execute (exec-file &rest body)
  "Find the EXEC-FILE and run the BODY."

  `(if (not (executable-find ,exec-file))
       (message "[ERROR]: <%s> not found!" ,exec-file)
     ,@body))

;;;###autoload
(defun python-isort ()
  "Sort the imports with isort."
  (interactive)
  (check-run-execute "isort"
		     (shell-command-on-region
		      (point-min) (point-max)
		      "isort --atomic --profile=black -"
		      (current-buffer) t)))

;;;###autoload
(defun python-remove-all-unused-imports ()
  "Remove all the unused imports, do NOT use pyimport, as it has bugs.
eg.from datetime import datetime."
  (interactive)
  (check-run-execute "autoflake"
		     (shell-command
		      (format "autoflake -i --remove-all-unused-imports %s"
			      (buffer-file-name)))
		     (revert-buffer t t t)))

(add-hook
 'python-mode-hook
 (lambda ()
   (add-hook 'before-save-hook #'python-isort nil t)
   (define-key python-mode-map (kbd "C-c p s") 'python-isort)
   (define-key python-mode-map (kbd "C-c p r") 'python-remove-all-unused-imports)))

(provide 'init-python)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-python.el ends here
