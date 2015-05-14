 ;; AUCTeX configuration

;; If you want to make AUCTeX aware of style files and multi-file documents
;; right away, insert the following in your '.emacs' file.
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Use pdflatex by default
(setq-default TeX-PDF-mode t)

;; Decrease font size of section titles
;; The default factor is 1.1.
;; (setq font-latex-fontify-sectioning 1)

(require 'auto-complete-auctex)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; https://github.com/Malabarba/latex-extra
(add-hook 'LaTeX-mode-hook #'latex-extra-mode)

;; RefTeX mode
;; http://www.stefanom.org/setting-up-a-nice-auctex-environment-on-mac-os-x/
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(latex-preview-pane-enable)

(setq doc-view-continuous t)

;; http://www.stefanom.org/setting-up-a-nice-auctex-environment-on-mac-os-x/
;; http://stackoverflow.com/questions/7899845/emacs-synctex-skim-how-to-correctly-set-up-syncronization-none-of-the-exi
;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -synctex=1 -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
     '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

;; Add more environment options to C-c C-e
;; http://superuser.com/questions/122410/adding-autocomplete-options-to-auctex-c-c-c-e
;; http://www.gnu.org/software/auctex/manual/auctex/Adding-Environments.html
(add-hook 'LaTeX-mode-hook (lambda ()
  (LaTeX-add-environments
    '("lstlisting" LaTeX-env-label))))

(provide 'init-latex)
