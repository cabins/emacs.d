;;; init-ui.el --- settings for Emacs UI -*- lexical-binding: t -*-

;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:

;; adjust the fonts
(defun font-available (font-list)
  "Get the first available font from FONT-LIST."
  (catch 'font
    (dolist (font font-list)
      (if (member font (font-family-list))
	  (throw 'font font)))))

;;;###autoload
(defun tenon/setup-font ()
  "Font setup."

  (let* ((enfonts '("Cascadia Code" "Source Code Pro" "Consolas" "Ubuntu Mono" "Monaco"))
	 (cnfonts '("STKaiti" "华文楷体" "STHeiti" "华文黑体" "微软雅黑" "文泉驿微米黑"))
	 (cnfont (font-available cnfonts))
	 (enfont (font-available enfonts)))
    (if enfont
	(set-face-attribute 'default nil
			    :font (format "%s-%d" enfont 10.0))
      (message "Failed to set default font."))
    (if cnfont
	(set-fontset-font "fontset-default" 'han
			  (font-spec :family cnfont
				     :size (if (eq system-type 'darwin) 12.5 11.0)))
      (message "Failed to set Chinese font."))))

(tenon/setup-font)
(tenon/cleaner-gui)
(load-theme 'leuven)

;; settings for daemon mode
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (when (window-system frame)
	      (tenon/setup-font))))

(provide 'init-ui)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-ui.el ends here
