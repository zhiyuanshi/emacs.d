;;; package --- Summary

;;; Commentary:

;; Requirements:

;;; Code:


;;;;;;;;;;;
;; web-mode
;;;;;;;;;;;

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


;;;;;;
;; CSS
;;;;;;

(setq css-indent-offset 2)
(setq scss-compile-at-save nil)

;; Colorize color names in CSS files.
(add-hook 'css-mode-hook 'rainbow-mode)


(provide 'init-web)
;;; init-web.el ends here
