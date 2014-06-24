(when (>= emacs-major-version 24)
  (require 'package)

  ;; Add the original Emacs Lisp Package Archive
  (add-to-list 'package-archives
               '("elpa" . "http://tromey.com/elpa/"))
  ;; Add the user-contributed repository
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))

  ; Activate all the packages (in particular autoloads)
  (package-initialize)
)
