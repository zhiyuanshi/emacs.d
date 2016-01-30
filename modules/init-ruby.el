;;; package --- Summary

;;; Commentary:

;; Requirements:

;;; Code:

;;;;;;;;;;;;
;; ruby-mode
;;;;;;;;;;;;

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

;; Avoid ridiculous Ruby indentation
(setq ruby-deep-indent-paren nil)

;; Sane defaults
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-ruby-mode.el#L12

(after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
  (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command))

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


;;;;;;;
;; robe
;;;;;;;

(add-hook 'ruby-mode-hook 'robe-mode)

;; A remedy for the default keybinding M-. being overwritten by Evil mode
(after-load 'robe
  (define-key robe-mode-map (kbd "C-c C-j") 'robe-jump))


;;;;;;;;;;;;
;; yard-mode
;;;;;;;;;;;;

(add-hook 'ruby-mode-hook 'yard-mode)


;;;;;;;;;;;;;;;;;;;
;; projectile-rails
;;;;;;;;;;;;;;;;;;;

(add-hook 'projectile-mode-hook 'projectile-rails-on)


;;;;;;;;;
;; rinari
;;;;;;;;;

(require 'rinari)
(global-rinari-mode)


;;;;;;;;;;;;;
;; rspec-mode
;;;;;;;;;;;;;

(eval-after-load 'rspec-mode
 '(rspec-install-snippets))


(provide 'init-ruby)
;;; init-ruby.el ends here
