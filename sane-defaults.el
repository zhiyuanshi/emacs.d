;; Turn off mouse interface early in startup to avoid momentary display
(menu-bar-mode 1)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; No splash screen please ... jeez
(setq inhibit-startup-screen t)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
;; https://github.com/syl20bnr/spacemacs/blob/master/init.el
(setq gc-cons-threshold 100000000)

(setq frame-title-format
  '("" (:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name))
                  "%b")) " - Emacs"))

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Frame-Parameters.html
(add-to-list 'default-frame-alist '(width  . 100))
(add-to-list 'default-frame-alist '(height . 60))

;; Display of line numbers in the left margin
(global-linum-mode 1)

;; Always display line and column numbers
(line-number-mode 1)
(column-number-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(setq make-backup-files nil) ;; don't create backup~ files
(setq auto-save-default nil) ;; don't create #autosave# files

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; http://stackoverflow.com/questions/445873/how-can-i-make-emacs-mouse-scrolling-slower-and-smoother
;; http://www.emacswiki.org/emacs/SmoothScrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 40) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; Why anyone thinks that auto-vscrolling should be on by default is beyond me.
;; http://stackoverflow.com/questions/18386824/emacs-how-do-you-disable-auto-recentering
;; http://www.emacswiki.org/emacs/SmoothScrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Lines should be 80 characters wide, not 72
(setq-default fill-column 80)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; UTF-8 please
;; https://ghc.haskell.org/trac/ghc/wiki/Emacs#MakethequotesinGHCerrormessagesdisplaynicely
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;; Sanity restored.
(provide 'sane-defaults)
