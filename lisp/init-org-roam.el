;;; init-org-roam --- org roam configurations
;;; Commentary:
;;; Code:

(use-package org-roam
  :ensure t
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (setq org-roam-database-connector 'sqlite-builtin)
  (org-roam-db-autosync-mode))

(provide 'init-org-roam)

;;; init-org-roam.el ends here
