
(require 'cl)

;; Add directories to Emacs's `load-path'
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(defvar user-packages
  '(;; winner-mode
    ace-jump-mode
    ag
    anzu
    base16-theme
    color-theme-solarized ;; Replace bbatsov's version due to its unpleasant Ruby syntax highlighting
    company
    company-flx
    dash ;; Included in https://github.com/chrisdone/emacs-haskell-config/blob/stack-mode/init.el
    diminish
    dired+
    dired-details+
    discover-my-major
    elpy
    elscreen
    ess
    evil
    evil-leader
    evil-nerd-commenter
    evil-surround
    expand-region
    flx-ido
    flycheck
    framemove
    fuzzy
    git-messenger
    github-browse-file
    grizzl
    helm
    helm-ag
    helm-projectile
    helm-swoop
    ido-ubiquitous
    imenu-anywhere
    multiple-cursors
    neotree
    org
    popwin
    projectile
    quickrun
    rainbow-delimiters
    rainbow-mode
    skewer-mode
    slime
    smart-mode-line
    smartparens
    smex
    smooth-scrolling
    spaceline

    ;; If we include this, automatic package installation process will hang.
    ;; Install tuareg-mode manually.
    ;; tuareg

    unicode-fonts
    wc-mode
    yasnippet
    zenburn-theme
    zlc ;; Zsh like completion system for Emacs
    ))

(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

(defun user-packages-installed-p ()
  (every #'package-installed-p user-packages))

(unless (user-packages-installed-p)
  (package-refresh-contents)
  (dolist (p user-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(global-evil-leader-mode)
(evil-leader/set-leader "SPC")
(evil-leader/set-key
  "SPC" 'ace-jump-char-mode'
  "," 'user/config-emacs
  "=" 'align-regexp
  "a" 'mark-whole-buffer
  "b" 'helm-buffers-list
  "c" 'wc-mode
  "d" 'dired-jump-other-window
  "e" 'helm-find-files
  "f" 'projectile-find-file
  "g" 'helm-projectile-ag
  "h" 'github-browse-file
  "i" 'imenu-anywhere
  "k" 'kill-this-buffer
  "m" 'delete-other-windows
  "n" 'make-frame-command
  "o" 'other-frame
  "p" 'projectile-switch-project
  "q" 'delete-window
  "r" 'projectile-replace
  "s" 'helm-swoop
  "t" 'neotree-toggle
  "w" 'save-buffer
  "x" 'delete-frame)

(defun text-scale-reset ()
  "Reset text scale to 0."
  (interactive)
  (text-scale-set 0))

;; http://stackoverflow.com/questions/18783227/emacs-zoom-in-out-globally
;; http://www.emacswiki.org/emacs/GlobalTextScaleMode
;; (defadvice text-scale-increase (around all-buffers (arg) activate)
;;   (dolist (buffer (buffer-list))
;;     (with-current-buffer buffer
;;       ad-do-it)))

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key [C-mouse-4] 'text-scale-increase)

(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key [C-mouse-5] 'text-scale-decrease)

(global-set-key (kbd "C-0") 'text-scale-reset)


(global-set-key (kbd "<C-tab>") 'elscreen-next)

;; If we don't this, <C-tab> wll get overridden in Org mode.
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(add-hook 'org-mode-hook
  (lambda ()
    (local-unset-key (kbd "<C-tab>"))))


;; https://github.com/magnars/.emacs.d/blob/master/key-bindings.el
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

(define-key 'help-command (kbd "C-m") 'discover-my-major)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; (global-set-key (kbd "C-x 4 u") 'winner-undo)
;; (global-set-key (kbd "C-x 4 r") 'winner-redo)

(define-key evil-normal-state-map (kbd "\\") 'ace-jump-char-mode)
(define-key evil-visual-state-map (kbd "\\") 'ace-jump-char-mode)

(define-key evil-normal-state-map (kbd "|") 'ace-jump-mode-pop-mark)
(define-key evil-visual-state-map (kbd "|") 'ace-jump-mode-pop-mark)

(global-set-key (kbd "C-`") 'er/expand-region)
(global-set-key (kbd "C-~") 'er/contract-region)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Frame-Parameters.html
(add-to-list 'default-frame-alist '(width  . 100))
(add-to-list 'default-frame-alist '(height . 60))

(if (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(font . "Ubuntu Mono-16"))
  (add-to-list 'default-frame-alist '(font . "Ubuntu Mono-14")))

;; (load-theme 'solarized t)

(menu-bar-mode 1)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq frame-title-format
  '("" (:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name))
                  "%b")) " - Emacs"))

;; http://stackoverflow.com/questions/445873/how-can-i-make-emacs-mouse-scrolling-slower-and-smoother
;; http://www.emacswiki.org/emacs/SmoothScrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 40) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; No splash screen please ... jeez
(setq inhibit-startup-screen t)

;; Display of line numbers in the left margin
(global-linum-mode 1)

;; Always display line and column numbers
(line-number-mode 1)
(column-number-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)


;;;;;;;;;;
;; NeoTree
;;;;;;;;;;

;; http://www.emacswiki.org/emacs/NeoTree#toc8
(setq projectile-switch-project-action 'neotree-projectile-action)


;;;;;;;;;;;;;;;;;;
;; Smart Mode Line
;;;;;;;;;;;;;;;;;;

(setq sml/no-confirm-load-theme t)
(sml/setup)


;;;;;;;;;;;;
;; Spaceline
;;;;;;;;;;;;

(require 'spaceline-config)
(spaceline-spacemacs-theme)
(spaceline-helm-mode)

;; Tabs
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Use normal tabs and display each tab as 8 spaces in Makefiles
(add-hook 'makefile-mode-hook 'indent-tabs-mode)
(add-hook 'makefile-mode-hook (lambda () (setq tab-width 8)))

;; Truncate lines and don't use word-wrapping for code, but do the opposites for text.
(require 'wc-mode)
(add-hook 'text-mode-hook (lambda ()
  flyspell-mode
  ; (turn-on-auto-fill)
  (visual-line-mode 1)
  wc-mode))

(add-hook 'prog-mode-hook (lambda ()
  (setq truncate-lines t
        word-wrap nil)))

;; Lines should be 80 characters wide, not 72
(setq-default fill-column 80)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; (add-hook 'before-save-hook 'untabify-current-buffer)

;; This setting can be too aggresive.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Show trailing whitespace and empty lines, but not in help buffers
;; https://ghc.haskell.org/trac/ghc/wiki/Emacs#Highlighttrailingwhitespaces
(setq-default show-trailing-whitespace nil)
(setq-default indicate-empty-lines t)

(add-hook 'special-mode-hook (lambda ()
  (setq show-trailing-whitespace nil)
  (setq indicate-empty-lines nil)))

;; (setq mode-require-final-newline nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; https://ghc.haskell.org/trac/ghc/wiki/Emacs#Untabifyingabuffer
(defun untabify-current-buffer ()
  "Untabify current buffer."
  (interactive)
  (save-excursion (untabify (point-min) (point-max))))

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

;; https://github.com/magnars/.emacs.d/blob/master/appearance.el
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(defun user/config-emacs ()
  "Open my init.org."
  (interactive)
  (find-file (expand-file-name "init.org" (getenv "EMACSD"))))

;; Save the state of Emacs from one session to another
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
;; (desktop-save-mode 1)

(setq make-backup-files nil) ;; don't create backup~ files
(setq auto-save-default nil) ;; don't create #autosave# files

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; uniquify
;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 20000000) ;; https://github.com/lewang/flx

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

(require 'unicode-fonts)

(global-set-key (kbd "RET") 'newline-and-indent)

;; anzu
(require 'anzu)
(diminish 'anzu-mode)
(global-anzu-mode +1)

;; framemove
(windmove-default-keybindings 'ctrl)
(setq framemove-hook-into-windmove t)

;; saveplace
;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; popwin
(require 'popwin)
(popwin-mode 1)

;; smooth-scrolling
;; Keep cursor away from edges when scrolling up/down
(require 'smooth-scrolling)

;; winner-mode
;; (winner-mode 1)

;; zlc
(require 'zlc)
(zlc-mode t)

(let ((map minibuffer-local-map))
  ;; like menu select
  (define-key map (kbd "<down>")  'zlc-select-next-vertical)
  (define-key map (kbd "<up>")    'zlc-select-previous-vertical)
  (define-key map (kbd "<right>") 'zlc-select-next)
  (define-key map (kbd "<left>")  'zlc-select-previous)

  ;; reset selection
  (define-key map (kbd "C-c") 'zlc-reset))

(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

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

;; expand-region
(require 'expand-region)

;; multiple-cursors
(require 'multiple-cursors)

;; evil
(require 'evil)
(evil-mode 1)

;; evil-nerd-commenter
(evilnc-default-hotkeys)

;; evil-surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;; flx-ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; ido-ubiquitous
;; Inhibit "Warning: reference to free variable `ido-ubiquitous-debug-mode'"
;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/35
;; (defvar ido-ubiquitous-debug-mode nil)
;; (defvar ido-context-switch-command nil)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

(require 'helm-config)

(setq helm-buffers-fuzzy-matching t)
(setq helm-move-to-line-cycle-in-source t)

(helm-mode 1)

(setq-default dired-dwim-target t)

(require 'dired+)
(diredp-toggle-find-file-reuse-dir 1)

(require 'dired-details+)

;; ag
(setq ag-highlight-search t)
(setq ag-reuse-buffers t)

;; imenu
(setq imenu-auto-rescan t)

;; projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)

;; recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(add-hook 'after-init-hook #'global-flycheck-mode)

;; company-mode
(add-hook 'after-init-hook 'global-company-mode)

;; yasnippet
(require 'yasnippet)
;; Reduce console messages at start-up
;; "Log level for `yas--message' 4 means trace most anything, 0 means nothing."
(setq yas-verbosity 1)
(yas-global-mode 1)

;; smartparens
(smartparens-global-mode 1)
(require 'smartparens-config) ;; the default configuration

(setq git-messenger:show-detail t) ;; Always show detail message

(require 'init-r)

(setq org-confirm-babel-evaluate nil)
(setq org-src-tab-acts-natively t)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    (sh . t)))

(custom-set-variables
  '(custom-safe-themes '(
    "f41fd682a3cd1e16796068a2ca96e82cfd274e58b978156da0acce4d56f2b0d5" ;; base16-railscasts
    "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" ;; solarized-light
    default))

  ;; The built-in sh-mode
  ;; Indent shell scripts with 2 spaces, not 4
  '(sh-basic-offset 2)
  '(sh-indentation 2)
  )

(provide '.emacs)
