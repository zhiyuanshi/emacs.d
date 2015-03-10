(setq-default TeX-PDF-mode t)

;; Decrease font size of section titles
;; The default factor is 1.1.
(setq font-latex-fontify-sectioning 1)

(require 'auto-complete-auctex)

(latex-preview-pane-enable)

(setq doc-view-continuous t)

(provide 'init-latex)
