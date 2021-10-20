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

(add-hook 'after-init-hook 'tenon/cleaner-gui)

;; settings for daemon mode
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
		(with-selected-frame frame
		  (tenon/setup-font))
		))
  (tenon/setup-font))


(provide 'init-ui)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-ui.el ends here
