;;; init-vue.el --- config for vue -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package vue-mode
  ;; disable the ugly background color
  ;; [refs] https://github.com/AdamNiederer/vue-mode#how-do-i-disable-that-ugly-background-color
  :config (set-face-background 'mmm-default-submode-face nil))

(provide 'init-vue)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-vue.el ends here
