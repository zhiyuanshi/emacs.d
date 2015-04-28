;; As instructed by `brew install coq'
(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

;; As instructed by `brew install proof-general'
(load-file "/usr/local/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")

(provide 'init-coq)
