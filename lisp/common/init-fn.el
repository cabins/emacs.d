;;; init-fn.el --- customized functions -*- lexical-binding: t -*-
;;; Commentary:
;; (c) Cabins Kong, 2022-

;;; Code:
(require 'subr-x)

(defun cabins/available-font (font-list)
  "Get the first available font from FONT-LIST."

  (cl-loop for font in font-list
           when (member font (font-family-list))
           return font))

;;;###autoload
(defun cabins/font-setup-charset(font-list charset-list)
  "Settings for CHARSET-LIST with fonts from FONT-LIST."

  (let ((font (cabins/available-font font-list)))
    (when font
      (dolist (charset charset-list)
	    (set-fontset-font t charset font))
      (add-to-list 'face-font-rescale-alist `(,font . 1.2)))))

;;;###autoload
(defun cabins/font-setup ()
  "Font setup."

  (interactive)
  ;; Default font settings
  (dolist (face '(default fixed-pitch fixed-pitch-serif variable-pitch))
	(set-face-attribute face nil :family (cabins/available-font '("Cascadia Code PL" "Consolas" "Menlo" "Monaco" "Ubuntu Mono" "Courier"))))
  ;; Chinese font settings
  (cabins/font-setup-charset '("楷体" "STKaiti" "黑体" "STHeiti" "微软雅黑" "文泉译微米黑" "Noto Sans SC") '(han))
  ;; Emoji font settings
  (cabins/font-setup-charset '("Apple Color Emoji" "Noto Color Emoji" "Segoe UI Emoji" "Symbola" "Symbol") '(emoji)))
(add-hook 'after-init-hook #'cabins/font-setup)

;;;###autoload
(defun tenon--cleaner-ui ()
  "Remove all the unnecessary elements."

  (when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
    (scroll-bar-mode -1))

  (when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
    (tool-bar-mode -1))

  (if (and (fboundp 'menu-bar-mode)
           (display-graphic-p))
      (menu-bar-mode +1)
    (menu-bar-mode -1))

  ;; tooltips in echo-aera
  (when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
    (tooltip-mode -1)))

;;;###autoload
(defun cabins/available-theme (theme-list)
  "Get the first available theme from THEME-LIST."

  (cl-loop for theme in theme-list
           when (member theme (custom-available-themes))
           return theme))

;;;###autoload
(defun cabins/default-light-theme ()
  "Get the default built-in light theme."

  (let ((theme-list '(modus-operandi
                      leuven
                      tsdh-light
                      tango
                      whiteboard)))
    (cabins/available-theme theme-list)))

;;;###autoload
(defun cabins/default-dark-theme ()
  "Get the default built-in dark theme."

  (let ((theme-list '(modus-vivendi
                      leuven-dark
                      tsdh-dark
                      tango-dark
                      wombat
                      dichromacy)))
    (cabins/available-theme theme-list)))

;;;###autoload
(defun cabins/set-theme-on-windows ()
  "Set theme on Windows 10 based on system dark mode."

  (interactive)
  (when (memq system-type '(ms-dos windows-nt cygwin))
    (if (eq 'modus-operandi (cabins/default-light-theme))
        (setq modus-themes-mode-line '(borderless (padding . 4) (height . 0.9)))
      )
    (let* ((cmd "powershell (Get-ItemProperty -Path HKCU:\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize -Name AppsUseLightTheme).AppsUseLightTheme")
           (mode (string-trim (shell-command-to-string cmd))))
      (if (equal mode "1")
          (load-theme (cabins/default-light-theme) t)
        (load-theme (cabins/default-dark-theme) t)))))
(add-hook 'after-init-hook #'cabins/set-theme-on-windows)

(provide 'init-fn)
;;; init-fn.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
