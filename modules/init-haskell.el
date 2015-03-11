;; Haskell Mode
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; Choose =haskell-indent= because: C-c C-. => 'haskell-indent-align-guards-and-rhs, nice! :)
;; Cf. https://github.com/haskell/haskell-mode/wiki/Indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

(customize-set-variable 'haskell-interactive-popup-errors nil)
(customize-set-variable 'haskell-process-auto-import-loaded-modules t)
(customize-set-variable 'haskell-process-log t)
(customize-set-variable 'haskell-process-suggest-haskell-docs-imports t)
(customize-set-variable 'haskell-process-suggest-hoogle-imports t)
(customize-set-variable 'haskell-process-suggest-remove-import-lines t)
(customize-set-variable 'haskell-process-type 'cabal-repl)


;; flycheck-haskell
(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)


;; ac-haskell-process
;; Enable Haskell completion source
(add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
(add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)

;; Enable auto-complete in haskell-interactive-mode
(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'haskell-interactive-mode))

;; If you want to trigger auto-complete using TAB in REPL buffers, you may want to
;; put auto-complete into your completion-at-point-functions:
(add-hook 'auto-complete-mode-hook       'set-auto-complete-as-completion-at-point-function)
(add-hook 'haskell-interactive-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'haskell-mode-hook             'set-auto-complete-as-completion-at-point-function)

;; You can use ac-haskell-process-popup-doc to pop up documentation for the symbol
;; at point:
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c C-d") 'ac-haskell-process-popup-doc))


;; structured-haskell-mode
(require 'shm)
(require 'shm-case-split)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)
(set-face-background 'shm-current-face "#eee8d5")
(set-face-background 'shm-quarantine-face "lemonchiffon")
;; (setq shm-idle-timeout 0)


;; From purcell
(dolist (hook '(haskell-mode-hook inferior-haskell-mode-hook haskell-interactive-mode-hook))
  (add-hook hook 'turn-on-haskell-doc-mode)
  (add-hook hook (lambda () (subword-mode +1))))

(provide 'init-haskell)
