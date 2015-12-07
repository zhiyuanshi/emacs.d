;; As instructed by `brew install coq'
(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

;; As instructed by `brew install proof-general'
;; (load-file "/usr/local/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")

;; https://github.com/cpitclaudel/company-coq
;; Open .v files with Proof-General's coq-mode
(require 'proof-site)

;; https://github.com/cpitclaudel/company-coq
;; Load company-coq when opening Coq files
(add-hook 'coq-mode-hook #'company-coq-initialize)

(provide 'init-coq)
