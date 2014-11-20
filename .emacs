
(require 'cl)

(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(defvar my-packages
  '(;; winner-mode
    ac-haskell-process
    ac-inf-ruby
    ac-js2
    ace-jump-mode
    ag
    anzu
    auctex
    auto-complete
    auto-complete-auctex
    base16-theme
    bundler
    coffee-mode
    color-theme-solarized ;; Replace bbatsov's version due to its unpleasant Ruby syntax highlighting
    diminish
    dired+
    dired-details+
    discover-my-major
    ;; elscreen
    ensime
    evil
    evil-leader
    evil-nerd-commenter
    evil-surround
    expand-region
    flx-ido
    flycheck
    flycheck-haskell
    flycheck-rust
    framemove
    fsharp-mode
    fuzzy
    git-messenger
    github-browse-file
    grizzl
    haskell-mode
    helm
    ido-ubiquitous
    idris-mode
    imenu-anywhere
    inf-ruby
    jquery-doc
    js2-mode
    js2-refactor
    less-css-mode
    magit
    monky
    multiple-cursors
    neotree
    org
    pandoc-mode
    popwin
    projectile
    projectile-rails
    rainbow-delimiters
    rainbow-mode
    rinari
    robe
    rspec-mode
    ruby-hash-syntax
    rust-mode
    sass-mode
    scala-mode2
    scss-mode
    shm
    sbt-mode
    skewer-mode
    slime
    slime-js
    smart-mode-line
    smartparens
    smex
    smooth-scrolling
    tern
    tern-auto-complete

    ;; If we include this, automatic package installation process will hang.
    ;; Install tuareg-mode manually.
    ;; tuareg

    utop
    web-mode
    yard-mode
    yari
    yasnippet
    zenburn-theme
    zlc ;; Zsh like completion system for Emacs
    ))

(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(defun my-packages-installed-p ()
  (every #'package-installed-p my-packages))

(unless (my-packages-installed-p)
  (package-refresh-contents)
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(global-evil-leader-mode)
(evil-leader/set-leader "SPC")
(evil-leader/set-key
  "a" 'mark-whole-buffer
  "b" 'helm-buffers-list
  "c" 'zhiyuan/config-emacs
  "d" 'dired-jump-other-window
  "e" 'helm-find-files
  "f" 'projectile-find-file
  "g" 'git-messenger:popup-message
  "i" 'imenu-anywhere
  "k" 'kill-this-buffer
  "m" 'delete-other-windows
  "n" 'make-frame-command
  "p" 'projectile-switch-project
  "q" 'delete-window
  "r" 'projectile-recentf
  "s" 'evil-window-split
  "t" 'neotree-toggle
  "v" 'evil-window-vsplit
  "w" 'save-buffer
  "x" 'delete-frame
  "=" 'align-regexp)

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


(global-set-key (kbd "<C-tab>") 'other-frame)

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

(after-load 'shm-case-split
  (define-key shm-map (kbd "C-c C-s") 'shm/case-split))

(add-hook 'ruby-mode-hook (lambda ()
  ;; LeWang:
  ;;
  ;;      I think `er/ruby-backward-up' and `er/ruby-forward-up' are nifty
  ;;      functions in their own right.
  ;;
  ;;      I would bind them to C-M-u and C-M-d respectively.
  (local-set-key (kbd "C-M-u") 'er/ruby-backward-up)
  (local-set-key (kbd "C-M-d") 'er/ruby-forward-up)
  (local-set-key (kbd "C-c C-c") 'inf-ruby-console-auto)
  (local-set-key (kbd "C-c C-h") 'ruby-toggle-hash-syntax)
  (local-set-key (kbd "C-c C-y") 'yari)))

;; A remedy for the default keybinding M-. being overwritten by Evil mode
(after-load 'robe
  (define-key robe-mode-map (kbd "C-c C-j") 'robe-jump))

(after-load 'tern
  (define-key tern-mode-keymap (kbd "C-c C-j") 'tern-find-definition)
  (define-key tern-mode-keymap (kbd "C-c C-k") 'tern-pop-find-definition))

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Frame-Parameters.html
(add-to-list 'default-frame-alist '(width  . 100))
(add-to-list 'default-frame-alist '(height . 45))
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-15"))
;; (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-11"))

(load-theme 'base16-railscasts t)

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

;; Tabs
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Use normal tabs and display each tab as 8 spaces in Makefiles
(add-hook 'makefile-mode-hook 'indent-tabs-mode)
(add-hook 'makefile-mode-hook (lambda () (setq tab-width 8)))

;; Truncate lines and don't use word-wrapping for code, but do the opposites for text.
(add-hook 'text-mode-hook (lambda ()
  (visual-line-mode 1)))

(add-hook 'prog-mode-hook (lambda ()
  (setq truncate-lines t
        word-wrap nil)))

;; Lines should be 80 characters wide, not 72
(setq-default fill-column 80)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

(defun zhiyuan/config-emacs ()
  "Open my .emacs.org."
  (interactive)
  (find-file (expand-file-name ".emacs.org" (getenv "DOTFILES"))))

;; Save the state of Emacs from one session to another
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
(desktop-save-mode 1)

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
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

(require 'helm-config)
;; (helm-mode 1)

(setq-default dired-dwim-target t)

(require 'dired+)
(diredp-toggle-find-file-reuse-dir 1)

(require 'dired-details+)

;; ag
(setq ag-highlight-search t)
(setq ag-reuse-buffers t)

;; imenu
(add-hook 'ruby-mode-hook 'imenu-add-menubar-index)
(setq imenu-auto-rescan t)

;; projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)

;; recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(add-hook 'after-init-hook #'global-flycheck-mode)

;; Make yasnippet and autocomplete work together on Emacs
;; http://truongtx.me/2013/01/06/config-yasnippet-and-autocomplete-on-emacs/

;; yasnippet
;; should be loaded before auto-complete so that they can work together
(require 'yasnippet)
(yas-global-mode 1)

;; auto-complete
;; should be loaded after yasnippet so that they can work together
(require 'auto-complete-config)
(ac-config-default)
(setq ac-use-fuzzy t)

;; https://github.com/purcell/ac-haskell-process
(defun set-auto-complete-as-completion-at-point-function ()
  (add-to-list 'completion-at-point-functions 'auto-complete))

;; smartparens
(smartparens-global-mode 1)
(require 'smartparens-config) ;; the default configuration

;; (elscreen-start)
;; (elscreen-set-prefix-key "\C-l")

(setq sml/no-confirm-load-theme t)
(sml/setup)

(setq git-messenger:show-detail t) ;; Always show detail message

(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
(add-hook 'typerex-mode-hook 'utop-setup-ocaml-buffer)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

(customize-set-variable 'haskell-interactive-popup-errors nil)
(customize-set-variable 'haskell-process-auto-import-loaded-modules t)
(customize-set-variable 'haskell-process-log t)
(customize-set-variable 'haskell-process-suggest-haskell-docs-imports t)
(customize-set-variable 'haskell-process-suggest-hoogle-imports t)
(customize-set-variable 'haskell-process-suggest-remove-import-lines t)
(customize-set-variable 'haskell-process-type 'cabal-repl)

(add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
(add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)

(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'haskell-interactive-mode))

(add-hook 'auto-complete-mode-hook       'set-auto-complete-as-completion-at-point-function)
(add-hook 'haskell-interactive-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'haskell-mode-hook             'set-auto-complete-as-completion-at-point-function)

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c C-d") 'ac-haskell-process-popup-doc))

(require 'shm)
(require 'shm-case-split)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)
(set-face-background 'shm-current-face "#eee8d5")
(set-face-background 'shm-quarantine-face "lemonchiffon")
;; (setq shm-idle-timeout 0)

(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)

(dolist (hook '(haskell-mode-hook inferior-haskell-mode-hook haskell-interactive-mode-hook))
  (add-hook hook 'turn-on-haskell-doc-mode)
  (add-hook hook (lambda () (subword-mode +1))))

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

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

(setq ruby-deep-indent-paren nil)

(after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
  (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command))

(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'inf-ruby-mode))
(add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)

(eval-after-load 'inf-ruby
  '(define-key inf-ruby-mode-map (kbd "TAB") 'auto-complete))

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)

(add-hook 'ruby-mode-hook 'yard-mode)

(require 'coffee-mode)
(customize-set-variable 'coffee-tab-width 2)

(setq js-indent-level 2)

;; https://github.com/swank-js/swank-js
(autoload 'js2-mode "js2-mode" nil t)

;; http://truongtx.me/2014/02/23/set-up-javascript-development-environment-in-emacs/
(setq js2-highlight-level 3)

;; https://github.com/lunaryorn/.emacs.d/blob/master/init.el
(setq-default js2-basic-offset 2)

(setq-default js2-mode-show-parse-errors nil
              js2-mode-show-strict-warnings nil)

;; A list of any extern names you'd like to consider always declared
;; http://howardabrams.com/projects/dot-files/emacs-javascript.html
(setq js2-global-externs '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))
(add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
;; (add-hook 'js2-mode-hook 'rainbow-delimiters-mode)
  ;; Javascript nests {} and () a lot, so I find this helpful

(add-hook 'js2-mode-hook 'ac-js2-mode)

;; js2-refactor
(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c C-m")

(skewer-setup)

(add-hook 'js2-mode-hook (lambda ()
  (tern-mode t)))

(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

;; Sometimes when you have just added .tern-project file or edit the
;; file but Tern does not auto reload, you need to manually kill
;; Tern server. This little piece of code does the trick.
;;
;; http://truongtx.me/2014/04/20/emacs-javascript-completion-and-refactoring/
(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

(require 'jquery-doc)
(add-hook 'js2-mode-hook 'jquery-doc-setup)

(add-hook 'projectile-mode-hook 'projectile-rails-on)

(require 'rinari)
(global-rinari-mode)

(eval-after-load 'rspec-mode
 '(rspec-install-snippets))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq css-indent-offset 2)
(setq scss-compile-at-save nil)

(add-hook 'css-mode-hook 'rainbow-mode)

(add-to-list 'auto-mode-alist '("\\.\\(md\\|markdown\\)\\'" . markdown-mode))

(setq-default TeX-PDF-mode t)

(require 'auto-complete-auctex)

(add-hook 'markdown-mode-hook 'turn-on-pandoc)

(setq org-confirm-babel-evaluate nil)
(setq org-src-tab-acts-natively t)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (clojure . t)
    (haskell . t)
    (scala . t)
    (python . t)
    (ruby . t)
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
