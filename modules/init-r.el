;;; package --- Summary

;;; Commentary:

;; Flycheck requires R package lintr from https://github.com/jimhester/lintr
;; ESS requires XQuartz from xquartz.macosforge.org

;;; Code:

(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Speaks Statistics (ESS)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ess-site)

(provide 'init-r)
;;; init-r.el ends here
