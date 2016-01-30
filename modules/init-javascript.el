;;; package --- Summary

;;; Commentary:

;; Requirements:

;;; Code:

;;;;;;;;;;;;;;
;; coffee-mode
;;;;;;;;;;;;;;

(require 'coffee-mode)
(customize-set-variable 'coffee-tab-width 2)


;;;;;;;;;;
;; js-mode
;;;;;;;;;;

(setq js-indent-level 2)


;;;;;;;;;;;
;; js2-mode
;;;;;;;;;;;

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

;; js2-refactor
(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c C-m")


;;;;;;;;;
;; Skewer
;;;;;;;;;

(skewer-setup)


;;;;;;;
;; tern
;;;;;;;

(add-hook 'js2-mode-hook (lambda ()
  (tern-mode t)))

;; Sometimes when you have just added .tern-project file or edit the
;; file but Tern does not auto reload, you need to manually kill
;; Tern server. This little piece of code does the trick.
;;
;; http://truongtx.me/2014/04/20/emacs-javascript-completion-and-refactoring/
(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

(after-load 'tern
  (define-key tern-mode-keymap (kbd "C-c C-j") 'tern-find-definition)
  (define-key tern-mode-keymap (kbd "C-c C-k") 'tern-pop-find-definition))


;;;;;;;;;;;;;
;; jQuery doc
;;;;;;;;;;;;;

(require 'jquery-doc)
(add-hook 'js2-mode-hook 'jquery-doc-setup)


(provide 'init-javascript)
;;; init-javascript.el ends here
