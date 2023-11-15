;;; init-functions.el --- provides some useful functions.
;;; Commentary:
;;; Code:

(defun efs/display-startup-time ()
  "Statistic for the startup time."

  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(defun dark-theme ()
  "Activate dark theme."

  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme 'modus-vivendi t))

(defun light-theme ()
  "Activate light theme."

  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme 'modus-operandi t))

;;;###autoload
(defun preferences()
  "Nothing, but alias like `crux-find-user-init-file', inspired by VSCode."

  (interactive)
  (find-file user-init-file))

;;;###autoload
(defun preference-custom()
  "Nothing, but alias like `crux-find-user-custom-file', inspired by VSCode."

  (interactive)
  (find-file custom-file))

(provide 'init-functions)

;;; init-functions.el ends here
