;;; init-functions.el --- provides some useful functions.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun efs/display-startup-time ()
  "Statistic for the startup time."

  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;;;###autoload
(defun preferences()
  "Nothing, but alias like `crux-find-user-init-file', inspired by VSCode."

  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-,") 'preferences)

;;;###autoload
(defun preference-custom()
  "Nothing, but alias like `crux-find-user-custom-file', inspired by VSCode."

  (interactive)
  (find-file custom-file))

;;;###autoload
(defun input-chinese-methods()
  "Enable the Chinese input methods"

  (interactive)
  (require 'init-input-methods)
  (toggle-input-method))

(provide 'init-functions)

;;; init-functions.el ends here
