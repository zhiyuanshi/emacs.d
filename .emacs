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
  '(evil evil-surround
    projectile fiplr imenu-anywhere
    dired+ direx
    flycheck
    auto-complete ;; company
    smartparens
    smex flx-ido popwin
    ;; haskell
    haskell-mode
    ;; company-ghc
    shm
    ;; ruby
    inf-ruby
    ;; company-inf-ruby
    robe
    ;; web
    rinari
    web-mode
    coffee-mode
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
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-15"))

(load-theme 'tango t)

;; GUI
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))
(menu-bar-mode 1)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq inhibit-startup-screen t)

(global-linum-mode 1)
(line-number-mode 1)
(column-number-mode 1)

(show-paren-mode 1)

;; tabs
(setq tab-width 2)
(setq-default indent-tabs-mode nil)

;; save the state of Emacs from one session to another
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
(desktop-save-mode 1)

;; https://ghc.haskell.org/trac/ghc/wiki/Emacs#MakethequotesinGHCerrormessagesdisplaynicely
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq make-backup-files nil) ;; don't create backup~ files
(setq auto-save-default nil) ;; don't create #autosave# files
(setq mode-require-final-newline nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;; https://ghc.haskell.org/trac/ghc/wiki/Emacs#Untabifyingabuffer
(defun untabify-current-buffer ()
  "Untabify current buffer."
  (interactive)
  (save-excursion (untabify (point-min) (point-max))))

(add-hook 'before-save-hook 'untabify-current-buffer)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; https://github.com/lewang/flx
(setq gc-cons-threshold 20000000)

;; evil
(require 'evil)
(evil-mode 1)

;; evil-surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;; projectile
(projectile-global-mode)

;; fiplr
(global-set-key (kbd "C-x f") 'fiplr-find-file)

;; dired+
(require 'dired+)
(diredp-toggle-find-file-reuse-dir 1)

;; direx
(require 'direx)
(global-set-key (kbd "C-x C-d") 'direx:jump-to-directory)

;; smartparens
(smartparens-global-mode 1)
(require 'smartparens-config) ;; the default configuration

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-use-fuzzy t)

;; company
;; (add-hook 'after-init-hook 'global-company-mode)
;; (global-company-mode)

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

;; imenu
(add-hook 'ruby-mode-hook 'imenu-add-menubar-index)
(setq imenu-auto-rescan t)

;; imenu-anywhere
(global-set-key (kbd "C-x m") 'imenu-anywhere)

;; popwin
(require 'popwin)
(popwin-mode 1)

;; haskell-mode
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-suggest-hoogle-imports t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t)
  '(haskell-process-type 'cabal-repl))

;; company-ghc
;; (push 'company-ghc company-backends)

;; structured-haskell-mode
(require 'shm)
(require 'shm-case-split)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)
(define-key shm-map (kbd "C-c C-s") 'shm/case-split)
(set-face-background 'shm-current-face "#eee8d5")
(set-face-background 'shm-quarantine-face "lemonchiffon")

;; proof-general

;; ruby-mode
;; Rake files are ruby, too, as are gemspecs, rackup files, and gemfiles.
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rabl\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Podfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.podspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Puppetfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Berksfile\\'" . ruby-mode))

;; inf-ruby
(add-hook 'ruby-mode-hook
  (lambda ()
    (local-set-key (kbd "C-c C-c") 'inf-ruby-console-auto)))

;; company-inf-ruby
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-inf-ruby))

;; robe
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)
;; (push 'company-robe company-backends)

;; rinari
(require 'rinari)

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; coffee-mode
(require 'coffee-mode)
(custom-set-variables
  '(coffee-tab-width 2))

(provide '.emacs)
;;; .emacs ends here
