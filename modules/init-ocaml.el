;;; package --- Summary

;;; Commentary:

;;; Code:

;; https://github.com/diml/utop#integration-with-the-tuaregtyperex-mode

(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
(add-hook 'typerex-mode-hook 'utop-setup-ocaml-buffer)

(provide 'init-ocaml)
;;; init-ocaml.el ends here
