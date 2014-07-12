(require 'package)

;; Add the original Emacs Lisp Package Archive
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
;; Add the user-contributed repository
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

; Activate all the packages (in particular autoloads)
(package-initialize)

(tool-bar-mode -1)

(setq default-frame-alist '((width . 130) (height . 50)))

(setq proof-three-window-mode-policy 'hybrid)

(require 'evil)
(evil-mode 1)
