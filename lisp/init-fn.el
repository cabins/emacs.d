;;; init-fn.el --- define some useful interactive functions -*- lexical-binding: t -*-
;; Author: Cabins
;; Maintainer: Cabins

;;; Commentary:
;; custom commands or debugging functions.
;; (c) Cabins Kong, 2020-2021

;;; Code:

;;;###autoload
(defun tenon/cleaner-gui ()
  "Make GUI cleaner, with less UI elements."

  (interactive)
  (when (not (eq scroll-bar-mode -1)) (scroll-bar-mode -1))
  (when (not (eq tool-bar-mode -1)) (tool-bar-mode -1))
  ;; menu-bar, keep on macOS
  (if (and (display-graphic-p) (eq system-type 'darwin))
      (menu-bar-mode 1)
    (menu-bar-mode -1))
  (fringe-mode 0))

(add-hook 'window-setup-hook 'tenon/cleaner-gui)

;;;###autoload
(defun tenon/change-theme ()
  "Change theme."

  (interactive)
  (let ((theme-list custom-enabled-themes))
    (call-interactively 'load-theme)
    (unless (equal custom-enabled-themes theme-list)
      (mapcar #'disable-theme theme-list))))

;;;###autoload
(defun tenon/preferences ()
  "Open the init file."

  (interactive)
  (find-file user-init-file))

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

(defun available-font (font-list)
  "Get the first available font from FONT-LIST."

  (catch 'font
    (dolist (font font-list)
      (if (member font (font-family-list))
	  (throw 'font font)))))

;;;###autoload
(defun tenon/reset-font-setup ()
  "Font setup."

  (interactive)
  (let* ((efl '("Cascadia Code" "Source Code Pro" "Courier New" "Monaco"))
	 (cfl '("STKaiti" "华文楷体" "STHeiti" "华文黑体" "微软雅黑"))
	 (cf (available-font cfl))
	 (ef (available-font efl)))
    (when ef
      (dolist (face '(default fixed-pitch fixed-pitch-serif variable-pitch))
	(set-face-attribute face nil :family ef)))
    (when cf
      (dolist (charset '(kana han cjk-misc bopomofo))
	(set-fontset-font t charset cf))
      (setq face-font-rescale-alist
	    (mapcar (lambda (item) (cons item 1.2)) cfl)))))

;;;###autoload
(defun tenon/tenon-theme-alpha ()
  "Tenon theme under alpha test."

  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  ;; default face
  (set-face-attribute 'default nil :foreground "#a46398")
  ;; modeline face
  (set-face-attribute 'mode-line nil
                      :foreground "#ffffff"
                      :background "#a46398"
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :foreground "#ffffff"
                      :background "#c8a1b7"
                      :box nil))

(provide 'init-fn)
;;; init-fn.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
