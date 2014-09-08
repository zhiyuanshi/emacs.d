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
  '(
    ;; Better defaults
    anzu
    discover-my-major
    flx-ido
    grizzl
    helm
    popwin
    smex
    smooth-scrolling

    ;; Text editing
    ace-jump-mode
    expand-region

    ;; Evil mode
    evil
    evil-leader
    evil-nerd-commenter
    evil-surround

    ;; File system browser
    dired+

    ;; Fuzzy finder for quickly opening files
    projectile

    ;; Fast project-wide search and replace
    ag
    imenu-anywhere

    ;; Multiple cursors and selections
    multiple-cursors

    ;; Snippets
    yasnippet

    ;; Syntax checking
    flycheck

    ;; Auto-completion
    auto-complete
    smartparens

    ;; Color themes
    base16-theme
    color-theme-solarized ;; Replace bbatsov's version due to its unpleasant Ruby syntax highlighting

    ;; Git
    git-messenger
    github-browse-file
    magit

    ;; Haskell
    flycheck-haskell
    haskell-mode
    shm

    ;; Ruby
    inf-ruby
    robe

    ;; Web
    coffee-mode
    js2-mode
    projectile-rails
    rinari
    sass-mode
    web-mode

    ;; Markup
    pandoc-mode
    ))

(defun my-packages-installed-p ()
  (every #'package-installed-p my-packages))

(unless (my-packages-installed-p)
  (package-refresh-contents)
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Frame-Parameters.html
(add-to-list 'default-frame-alist '(width  . 100))
(add-to-list 'default-frame-alist '(height . 45))
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-15"))

(load-theme 'dichromacy t)

;; GUI
(setq frame-title-format
  '("" (:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name))
                  "%b")) " - Emacs"))
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

;; Truncate lines and don't use word-wrapping for code, but do the opposites for text.
(add-hook 'text-mode-hook '(lambda ()
  (setq truncate-lines nil
        word-wrap t)))

(add-hook 'prog-mode-hook '(lambda ()
  (setq truncate-lines t
        word-wrap nil)))

(setq-default fill-column 80)

;; save the state of Emacs from one session to another
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
(desktop-save-mode 1)

;; https://ghc.haskell.org/trac/ghc/wiki/Emacs#MakethequotesinGHCerrormessagesdisplaynicely
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq make-backup-files nil) ;; don't create backup~ files
(setq auto-save-default nil) ;; don't create #autosave# files
(setq mode-require-final-newline nil)

(defalias 'yes-or-no-p 'y-or-n-p)

;; https://ghc.haskell.org/trac/ghc/wiki/Emacs#Untabifyingabuffer
(defun untabify-current-buffer ()
  "Untabify current buffer."
  (interactive)
  (save-excursion (untabify (point-min) (point-max))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; https://github.com/lewang/flx
(setq gc-cons-threshold 20000000)

;; evil-leader

;; Note: You should enable global-evil-leader-mode before you enable evil-mode,
;; otherwise evil-leader won’t be enabled in initial buffers (*scratch*, *Messages*, …).
(global-evil-leader-mode)
(evil-leader/set-leader "SPC")
(evil-leader/set-key
  "a" 'mark-whole-buffer
  "b" 'ido-switch-buffer
  "d" 'dired-jump
  "e" 'ido-find-file
  "f" 'projectile-find-file
  "j" 'ace-jump-mode
  "J" 'ace-jump-mode-pop-mark
  "k" 'kill-buffer-and-window
  "m" 'imenu-anywhere
  "p" 'projectile-switch-project
  "q" 'delete-window
  "w" 'save-buffer
  "=" 'align-regexp
  "?" 'git-messenger:popup-message
  "/" 'ag-regexp)

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; ace-jump-mode
;; Enable a more powerful jump back function from ace jump mode
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))

(setq ace-jump-mode-gray-background nil)
(setq ace-jump-mode-scope 'window)

(global-set-key (kbd "<C-tab>") 'next-buffer)

;; anzu
(require 'anzu)
(diminish 'anzu-mode)
(global-anzu-mode +1)

(define-key 'help-command (kbd "C-m") 'discover-my-major)

;; evil
(require 'evil)
(evil-mode 1)

;; evil-surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;; evil-nerd-commenter
(evilnc-default-hotkeys)

;; expand-region
(global-set-key (kbd "C-'") 'er/expand-region)

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; projectile
(projectile-global-mode)
(setq projectile-completion-system 'grizzl)

;; projectile-rails
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; ag
(setq ag-highlight-search t)
(setq ag-reuse-buffers t)

;; dired+
(require 'dired+)
(diredp-toggle-find-file-reuse-dir 1)

;; smartparens
(smartparens-global-mode 1)
(require 'smartparens-config) ;; the default configuration

;; git-messenger
;; Always show detail message
(setq git-messenger:show-detail t)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-use-fuzzy t)

(add-hook 'robe-mode-hook 'ac-robe-setup)

;; flx-ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; imenu
(add-hook 'ruby-mode-hook 'imenu-add-menubar-index)
(setq imenu-auto-rescan t)

;; popwin
(require 'popwin)
(popwin-mode 1)

;; smooth-scrolling
(require 'smooth-scrolling)

;; http://stackoverflow.com/questions/445873/how-can-i-make-emacs-mouse-scrolling-slower-and-smoother
;; http://www.emacswiki.org/emacs/SmoothScrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 40) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; haskell-mode
;; (add-hook 'haskell-mode-hook
;;   (lambda () (set-input-method "TeX")))

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; Choose this one because: C-c C-. => 'haskell-indent-align-guards-and-rhs, nice! :)
;; Cf. https://github.com/haskell/haskell-mode/wiki/Indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

;; https://github.com/magnars/.emacs.d/blob/master/defuns/lisp-defuns.el
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
    (prin1 (eval (read (current-kill 0)))
           (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; https://github.com/magnars/.emacs.d/blob/master/key-bindings.el
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;; structured-haskell-mode
(require 'shm)
(require 'shm-case-split)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)
(define-key shm-map (kbd "C-c C-s") 'shm/case-split)
(set-face-background 'shm-current-face "#eee8d5")
(set-face-background 'shm-quarantine-face "lemonchiffon")
(setq shm-idle-timeout 0)

;; flycheck-haskell
(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)

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

;; robe
(add-hook 'ruby-mode-hook 'robe-mode)

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

(setq js-indent-level 2)
;; https://github.com/lunaryorn/.emacs.d/blob/master/init.el
(setq-default js2-basic-offset 2)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; coffee-mode
(require 'coffee-mode)

(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))

;; pandoc-mode
(add-hook 'markdown-mode-hook 'turn-on-pandoc)

(custom-set-variables
  '(coffee-tab-width 2)
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-suggest-hoogle-imports t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t)
  '(haskell-process-type 'cabal-repl))

(provide '.emacs)
;;; .emacs ends here
