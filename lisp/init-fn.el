;;; init-fn.el --- customized functions -*- lexical-binding: t -*-
;;; Commentary:
;; (c) Cabins Kong, 2022-

;;; Code:
(require 'subr-x)

;;;;;;;;;;;;;;;;;;;
;; FONT SETTINGS ;;
;;;;;;;;;;;;;;;;;;;

(defun cabins/set-font-common (character font-list &optional scale-factor)
  "Set fonts for multi CHARACTER from FONT-LIST and modify style with SCALE-FACTOR."

  (cl-loop for font in font-list
           when (find-font (font-spec :name font))
           return (if (not character)
                      (set-face-attribute 'default nil :family font)
                    (if scale-factor (setq face-font-rescale-alist `((,font . ,scale-factor))))
                    (set-fontset-font t character (font-spec :family font) nil 'prepend))))

(defun cabins/font-setup ()
  "Font setup."

  (interactive)
  (when (display-graphic-p)
    ;; Default font
    (cabins/set-font-common nil '("Sometype Mono" "Cascadia Code PL" "Menlo" "Consolas"))
    ;; Unicode characters
    (cabins/set-font-common 'unicode '("Segoe UI Symbol" "Symbola" "Symbol"))
    ;; Emoji
    (cabins/set-font-common 'emoji '("Noto Color Emoji" "Apple Color Emoji"))
    ;; Chinese characters
    (cabins/set-font-common 'han '("楷体" "WenQuanYi Micro Hei" "PingFang SC" "Microsoft Yahei UI") 1.2)))

;;;;;;;;;;;;;;;;;
;; UI SETTINGS ;;
;;;;;;;;;;;;;;;;;

;;;###autoload
(defun make-ui-cleaner ()
  "Remove all the unnecessary elements."

  (when (display-graphic-p)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)))

;;;;;;;;;;;;;;;;;;;;
;; THEME SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun cabins/available-theme (theme-list)
  "Get the first available theme from THEME-LIST."

  (cl-loop for theme in theme-list
           when (member theme (custom-available-themes))
           return (load-theme theme t)))

(defun cabins/load-theme()
  "Load theme, Auto change color scheme according to system dark mode on Windows."

  (interactive)
  (when (display-graphic-p)
    ;; choose theme according to system dark/light mode
    (if (let* ((cmd (cond
                     ((member system-type '(ms-dos windows-nt cygwin))
                      "powershell (Get-ItemProperty -Path HKCU:\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize -Name AppsUseLightTheme).AppsUseLightTheme")
                     ((eq system-type 'darwin)
                      "defaults read -g AppleInterfaceStyle")
                     ((eq system-type 'gnu/linux)
                      "gsettings get org.gnome.desktop.interface color-scheme")))
               (mode (string-trim (shell-command-to-string cmd))))
          (if (member mode '("0" "Dark" "'prefer-dark'"))
              t
            nil))
        (cabins/available-theme '(modus-vivendi leuven-dark tsdh-dark tango-dark))
      (cabins/available-theme '(modus-operandi leuven tsdh-light tango)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERACTIVE FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun preferences()
  "Nothing, but alias like `crux-find-user-init-file', inspired by VSCode."

  (interactive)
  (find-file-other-window user-init-file))

;;;###autoload
(defun preferences-custom()
  "Nothing, but alias like `crux-find-user-custom-file', inspired by VSCode."

  (interactive)
  (find-file-other-window custom-file))

;; ********** keybindings **********
(global-set-key (kbd "C-,") 'preferences)
(global-set-key (kbd "<M-RET>") #'toggle-frame-maximized)

;;(add-hook 'tty-setup-hook #'make-ui-cleaner)
(add-hook 'emacs-startup-hook
          (lambda ()
            (cabins/font-setup)
            (cabins/load-theme)))

(when (daemonp)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (cabins/font-setup)
                (cabins/load-theme)))))

(provide 'init-fn)
;;; init-fn.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
