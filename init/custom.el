(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-verbatim-environments (quote ("verbatim" "verbatim*" "lstlisting")))
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) -shell-escape %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo %(extraopts) --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "%(PDF)amstex %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "texexec --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "texexec %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
     ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
     ("Xindy" "texindy %s" TeX-run-command nil t :help "Run xindy to create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(ac-auto-show-menu 0.8)
 '(ac-delay 0.2)
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "6e92ca53a22d9b0577ad0b16e07e2e020c8b621197e39fec454048e51b7954cb" default)))
 '(fill-column 80)
 '(frame-brackground-mode (quote dark))
 '(global-auto-complete-mode t)
 '(magit-log-show-gpg-status t)
 '(ns-alternate-modifier (quote none))
 '(ns-command-modifier (quote meta))
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (virtualenvwrapper pytest auctex cpputils-cmake flymake-cursor exec-path-from-shell undo-tree switch-window smex smart-mode-line rust-mode outline-magic rainbow-mode rainbow-delimiters puppet-mode project-explorer projectile pkgbuild-mode password-store paredit nyan-mode mvn multiple-cursors markdown-mode+ magit iy-go-to-char ido-ubiquitous idle-highlight-mode hi2 haskell-mode go-mode flycheck flx-ido erlang dockerfile-mode confluence cider browse-kill-ring ack-and-a-half ace-jump-mode auto-complete)))
 '(require-final-newline (quote visit-save)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#2aa198"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#b58900"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#268bd2"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#dc322f"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#859900"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#268bd2"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#cb4b16"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#d33682"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#839496")))))
