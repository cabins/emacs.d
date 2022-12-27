;;; init-utils-interactive --- some interactively called functions
;;; Commentary:
;;; Code

;; ********** some useful functions / alias **********

;;;###autoload
(defun preferences()
  "Nothing, but alias for `crux-find-user-init-file', inspired by VSCode."

  (interactive)
  (funcall-interactively 'crux-find-user-init-file)
  (funcall-interactively 'delete-other-windows))

;;;###autoload
(defun preferences-custom()
  "Nothing, but alias for `crux-find-user-custom-file', inspired by VSCode."

  (interactive)
  (funcall-interactively 'crux-find-user-custom-file)
  (funcall-interactively 'delete-other-windows))

;; ********** keybindings **********
(global-set-key (kbd "C-,") 'preferences)
(global-set-key (kbd "<M-RET>") #'toggle-frame-maximized)

(provide 'init-utils-interactive)

;;; init-utils-interactive.el ends here
