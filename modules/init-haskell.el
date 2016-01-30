;;; package --- Summary

;;; Commentary:

;; Programs expected to be available at command line:
;; * hoogle
;; * haskell-docs

;;; Code:

(require 'haskell-mode)
(require 'haskell-cabal)
(require 'which-func)
(require 'company)


;;;;;;;;;;;;;;;
;; Haskell Mode
;;;;;;;;;;;;;;;

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)


;; 5 Unicode support
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)


;; 6 Indentation
;; Choose =haskell-indent= because: C-c C-. => 'haskell-indent-align-guards-and-rhs, nice! :)
;; Cf. https://github.com/haskell/haskell-mode/wiki/Indentation
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
     (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)))


;; 7 Declaration scannning
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

(eval-after-load "which-func"
  '(add-to-list 'which-func-modes 'haskell-mode))


;; 8 Compilation
(eval-after-load "haskell-mode"
    '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

(eval-after-load "haskell-cabal"
    '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))


;; 10 Interactive Haskell
(eval-after-load "haskell-mode"
  '(progn
    (define-key haskell-mode-map (kbd "C-x C-d") nil)
    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
    (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c M-.") nil)
    (define-key haskell-mode-map (kbd "C-c C-d") nil)
    (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))


(custom-set-variables
  '(haskell-compile-cabal-build-command "stack build")
  '(haskell-font-lock-symbols t)
  '(haskell-interactive-popup-errors nil)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t)
  '(haskell-process-suggest-haskell-docs-imports t)
  '(haskell-process-suggest-hoogle-imports t)
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-type 'stack-ghci))


;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck for Haskell
;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))


;;;;;;;;;;;;;;
;; Company GHC
;;;;;;;;;;;;;;

(add-to-list 'company-backends 'company-ghc)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; structured-haskell-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'shm)
;; (require 'shm-case-split)
;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)
;; (set-face-background 'shm-current-face "#eee8d5")
;; (set-face-background 'shm-quarantine-face "lemonchiffon")
;; (setq shm-idle-timeout 0)

;; (after-load 'shm-case-split
;;   (define-key shm-map (kbd "C-c C-s") 'shm/case-split))

;;;;;;;;;;;;;;;
;; From purcell
;;;;;;;;;;;;;;;

;; (dolist (hook '(haskell-mode-hook inferior-haskell-mode-hook haskell-interactive-mode-hook))
;;   (add-hook hook 'turn-on-haskell-doc-mode)
;;   (add-hook hook (lambda () (subword-mode +1))))


(provide 'init-haskell)
;;; init-haskell.el ends here
