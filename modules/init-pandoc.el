;;; package --- Summary

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;
;; pandoc-mode
;;;;;;;;;;;;;;

(require 'pandoc-mode)
(add-hook 'markdown-mode-hook 'turn-on-pandoc)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)


(provide 'init-pandoc)
;;; init-pandoc.el ends here
