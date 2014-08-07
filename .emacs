;;; .emacs --- an idempotent .emacs for immediate deployment

;;; Commentary:

;;; Code:

;; common Lisp goodies, loop
(require 'cl)
(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(defvar my-packages
  '(evil
    fiplr
    flycheck
    company
    smex
    flx-ido
    ;; haskell
    haskell-mode
    company-ghc
    shm
    ;; ruby
    inf-ruby
    flymake-ruby
    robe
    base16-theme))

(defun my-packages-installed-p ()
  (every #'package-installed-p my-packages))

(unless (my-packages-installed-p)
  (package-refresh-contents)
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Frame-Parameters.html
(add-to-list 'default-frame-alist '(width  . 110))
(add-to-list 'default-frame-alist '(height . 45))
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-15"))

(load-theme 'tango t)

(menu-bar-mode 1)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq inhibit-startup-screen t)

(global-linum-mode 1)
(line-number-mode 1)
(column-number-mode 1)

(show-paren-mode 1)

;; tabs
(setq-default indent-tabs-mode nil)

;; save the state of Emacs from one session to another
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
(desktop-save-mode 1)

(prefer-coding-system 'utf-8)

(setq make-backup-files nil) ;; don't create backup~ files
(setq auto-save-default nil) ;; don't create #autosave# files
(setq mode-require-final-newline nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


;; evil
(require 'evil)
(evil-mode 1)

;; fiplr
(global-set-key (kbd "C-x f") 'fiplr-find-file)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; company
;; (add-hook 'after-init-hook 'global-company-mode)
(global-company-mode)

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; flx-ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; haskell-mode
(add-hook 'haskell-mode-hook 'structured-haskell-mode)

;; company-ghc
(push 'company-ghc company-backends)

;; structured-haskell-mode
(require 'shm)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)
(set-face-background 'shm-current-face "#eee8d5")
(set-face-background 'shm-quarantine-face "lemonchiffon")

;; proof-general
(setq proof-three-window-mode-policy 'hybrid)

;; flymake-ruby
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; robe
(add-hook 'ruby-mode-hook 'robe-mode)
(push 'company-robe company-backends)

(provide '.emacs)
;;; .emacs ends here
