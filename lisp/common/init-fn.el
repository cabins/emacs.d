;;; init-fn.el --- customized functions -*- lexical-binding: t -*-
;;; Commentary:
;; (c) Cabins Kong, 2022-

;;; Code:

;;;###autoload
(defun tenon--change-theme ()
  "Change theme."

  (interactive)
  (let ((theme-list custom-enabled-themes))
    (call-interactively 'load-theme)
    (unless (equal custom-enabled-themes theme-list)
      (mapcar #'disable-theme theme-list))))

;;;###autoload
(defmacro tenon--timer (&rest body)
  "Measure the time of code BODY running."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defun tenon--available-font (font-list)
  "Get the first available font from FONT-LIST."

  (catch 'font
    (dolist (font font-list)
      (if (member font (font-family-list))
	      (throw 'font font)))))

;;;###autoload
(defun tenon--font-setup ()
  "Font setup."

  (interactive)
  (let* ((efl '("Cascadia Code" "Source Code Pro" "JetBrains Mono" "Courier New" "Monaco" "Ubuntu Mono"))
	     (cfl '("黑体" "楷体" "STHeiti" "STKaiti" "微软雅黑" "文泉译微米黑"))
         (eml '("Apple Color Emoji" "Segoe UI Emoji" "Noto Color Emoji" "Symbola" "Symbol"))
	     (cf (tenon--available-font cfl))
	     (ef (tenon--available-font efl))
         (em (tenon--available-font eml)))
    (when ef
      (dolist (face '(default fixed-pitch fixed-pitch-serif variable-pitch))
	    (set-face-attribute face nil :family ef)))
    (when em
      (dolist (charset `(unicode unicode-bmp ,(if (> emacs-major-version 27) 'emoji 'symbol)))
        (set-fontset-font t charset em nil 'prepend)))
    (when cf
      (dolist (charset '(kana han cjk-misc bopomofo))
	    (set-fontset-font t charset cf))
      (setq face-font-rescale-alist
	        (mapcar (lambda (item) (cons item 1.2)) '(cf em))))))

;;;autoload
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

(provide 'init-fn)
;;; init-fn.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
