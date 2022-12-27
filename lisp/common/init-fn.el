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
    (cl-loop for font in '("Courier Prime" "Cascadia Code" "Fira Code" "Jetbrains Mono" "Hack" "Source Code Pro" "Menlo" "Monaco" "Consolas")
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
    (cl-loop for font in '("霞鹜文楷" "WenQuanYi Micro Hei" "PingFang SC" "Microsoft Yahei UI" "Microsoft Yahei" "STFangsong")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.2)))
                      (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family font))))))

;;;###autoload
(defun make-ui-cleaner ()
  "Remove all the unnecessary elements."

  ;; tooltips in echo-aera
  (when (fboundp 'tooltip-mode) (tooltip-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  ;; for menu-bar, only show when runs on GUI mode & macos
  (when (fboundp 'menu-bar-mode)
    (if (and (display-graphic-p) (eq system-type 'darwin))
      (menu-bar-mode +1)
    (menu-bar-mode -1))))

;;;###autoload
(defun cabins/available-theme (theme-list)
  "Get the first available theme from THEME-LIST."

  (cl-loop for theme in theme-list
           when (member theme (custom-available-themes))
           return theme))

(defun cabins/os-dark-mode()
  "Check the os dark mode, only support Windows for now."

  (let* ((cmd (cond
               ((member system-type '(ms-dos windows-nt cygwin))
                "powershell (Get-ItemProperty -Path HKCU:\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize -Name AppsUseLightTheme).AppsUseLightTheme")
               ((eq system-type 'darwin)
                "defaults read -g AppleInterfaceStyle")
               ((eq system-type 'gnu/linux)
                "gsettings get org.gnome.desktop.interface color-scheme")))
         (mode (string-trim (shell-command-to-string cmd))))
    (message mode)
    (if (member mode '("0" "Dark" "'prefer-dark'")) t nil)))

(defun cabins/load-theme()
  "Load theme, Auto change color scheme according to system dark mode on Windows."

  (interactive)
  (when (display-graphic-p)
    (let ((light-theme (cabins/available-theme '(modus-operandi leuven tsdh-light tango whiteboard)))
          (dark-theme (cabins/available-theme '(dracula modus-vivendi leuven-dark tsdh-dark tango-dark wombat dichromacy))))
      (if (cabins/os-dark-mode)
          (load-theme dark-theme t)
        (load-theme light-theme t)))))

(add-hook 'emacs-startup-hook 'cabins/font-setup)
(add-hook 'emacs-startup-hook 'cabins/load-theme)

(provide 'init-fn)
;;; init-fn.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
