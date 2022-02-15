;;; init-fn.el --- define some useful interactive functions -*- lexical-binding: t -*-
;; Author: Cabins
;; Maintainer: Cabins

;;; Commentary:
;; custom commands or debugging functions.
;; (c) Cabins Kong, 2020-2021

;;; Code:

;;;###autoload
(defun tenon/reload-init-file ()
  "Reload Emacs init file."

  (interactive)
  (load-file user-init-file))

;;;###autoload
(defmacro tenon/timer (&rest body)
  "Measure the time of code BODY running."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;;;###autoload
(defun tenon/update-config ()
  "Update tenon config."

  (interactive)
  (cd user-emacs-directory)
  (shell-command "git pull"))

(provide 'init-fn)

;;; init-fn.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
