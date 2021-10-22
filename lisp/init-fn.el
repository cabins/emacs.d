;;; init-fn.el --- define some useful interactive functions -*- lexical-binding: t -*-
;; Author: Cabins
;; Maintainer: Cabins

;;; Commentary:
;; the functions are all for debuging.
;; (c) Cabins Kong, 2020-2021

;;; Code:

;;;###autoload
(defmacro tenon/timer (&rest body)
  "Measure the time of code BODY running."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;;;###autoload
(defun tenon/tmp-reset-elpa-repo ()
  "Reset Elpa temporary.  Useful when emacs-china sync fails."

  (interactive)
  (setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.org/packages/"))))

;;;###autoload
(defun tenon/cleaner-gui ()
  "Make GUI cleaner, with less UI elements."

  (interactive)
  (when (not (eq scroll-bar-mode -1)) (scroll-bar-mode -1))
  (when (not (eq tool-bar-mode -1)) (tool-bar-mode -1))
  ;; menu-bar, keep on macos
  (if (and (display-graphic-p) (eq system-type 'darwin))
      (menu-bar-mode 1)
    (menu-bar-mode -1))
  (fringe-mode 0))

(add-hook 'window-setup-hook 'tenon/cleaner-gui)

;;;###autoload
(defun tenon/reload-init-file ()
  "Reload Emacs init file."

  (interactive)
  (load-file user-init-file))

;;;###autoload
(defun tenon/update-config ()
  "Update tenon config."

  (interactive)
  (cd user-emacs-directory)
  (shell-command "git pull"))

;;;###autoload
(defun tenon/change-theme ()
  "Change theme."

  (interactive)
  (if custom-enabled-themes
      (let ((theme-list custom-enabled-themes))
	(call-interactively 'load-theme)
	(dolist (current-theme theme-list)
	  (disable-theme current-theme)))
    (call-interactively 'load-theme)))


(defun font-available (font-list)
  "Get the first available font from FONT-LIST."
  (catch 'font
    (dolist (font font-list)
      (if (member font (font-family-list))
	  (throw 'font font)))))

;;;###autoload
(defun tenon/setup-font ()
  "Font setup."

  (interactive)
  (let* ((enfonts '("Cascadia Code" "Source Code Pro" "Courier New" "Monaco"))
	 (cnfonts '("STKaiti" "华文楷体" "STHeiti" "华文黑体" "微软雅黑"))
	 (cnfont (font-available cnfonts))
	 (enfont (font-available enfonts)))
    (when enfont
      (set-face-attribute 'default nil :family enfont)
      (set-face-attribute 'fixed-pitch nil :family enfont)
      (set-face-attribute 'variable-pitch nil :family enfont))
    (when cnfont
      (dolist (charset '(kana han cjk-misc bopomofo))
	(set-fontset-font t charset cnfont))
      (setq face-font-rescale-alist
	    (mapcar (lambda (item) (cons item 1.2)) cnfonts)))))

(provide 'init-fn)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-fn.el ends here
