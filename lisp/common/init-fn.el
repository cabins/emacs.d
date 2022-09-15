;;; init-fn.el --- customized functions -*- lexical-binding: t -*-
;;; Commentary:
;; (c) Cabins Kong, 2022-

;;; Code:
(require 'subr-x)

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun cabins/font-setup ()
  "Font setup."

  (interactive)
  (when (display-graphic-p)
    ;; Default font
    (cl-loop for font in '("Cascadia Code" "Fira Code" "Jetbrains Mono" "SF Mono" "Hack" "Source Code Pro" "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil :family font))

    ;; Unicode characters
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (set-fontset-font t 'unicode font nil 'prepend))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji")
             when (font-installed-p font)
             return (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))

    ;; Chinese characters
    (cl-loop for font in '("WenQuanYi Micro Hei" "PingFang SC" "Microsoft Yahei UI" "Microsoft Yahei" "STFangsong")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.2)))
                      (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family font))))))

;;;###autoload
(defun tenon--cleaner-ui ()
  "Remove all the unnecessary elements."

  ;; tooltips in echo-aera
  (when (fboundp 'tooltip-mode) (tooltip-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  ;; for menu-bar, only show when runs on GUI mode
  (if (and (display-graphic-p) (fboundp 'menu-bar-mode))
      (menu-bar-mode +1)
    (menu-bar-mode -1)))

;;;###autoload
(defun cabins/available-theme (theme-list)
  "Get the first available theme from THEME-LIST."

  (cl-loop for theme in theme-list
           when (member theme (custom-available-themes))
           return theme))

(defun cabins/os-dark-mode()
  "Check the os dark mode, only support Windows for now."

  (when (memq system-type '(ms-dos windows-nt cygwin))
    (let* ((cmd "powershell (Get-ItemProperty -Path HKCU:\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize -Name AppsUseLightTheme).AppsUseLightTheme")
           (mode (string-trim (shell-command-to-string cmd))))
      (if (equal mode "1") t nil)))
  ;; TODO support for macOS
  ;; TODO support for linux, GNOME only
  )

(defun cabins/load-theme()
  "Load theme, Auto change color scheme according to system dark mode on Windows."

  (interactive)
  (let ((light-theme (cabins/available-theme '(modus-operandi leuven tsdh-light tango whiteboard)))
        (dark-theme (cabins/available-theme '(modus-vivendi leuven-dark tsdh-dark tango-dark wombat dichromacy))))

    (if (cabins/os-dark-mode)
        (load-theme light-theme t)
      (load-theme dark-theme t))))

(provide 'init-fn)
;;; init-fn.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
