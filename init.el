;; Configure package manager
(require 'package)

;; Add Marmalade repo
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; ... and melpa. Melpa packages that exist on marmalade will have
;; precendence.
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Elpy
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))

;; And load things!
(package-refresh-contents)
(package-initialize)

(defvar my-pkgs
  '(;; All packages
    ac-cider-compliment
    ace-jump-mode
    ack-and-a-half
    browse-kill-ring
    cider
    clojure-mode
    confluence
    dash
    dockerfile-mode
    erlang
    flx-ido
    flycheck
    go-mode
    haskell-mode
    hi2
    idle-highlight-mode
    ido-ubiquitous
    iy-go-to-char
    magit
    markdown-mode+
    multiple-cursors
    mvn
    nyan-mode
    paredit
    password-store
    pkgbuild-mode
    projectile
    puppet-mode
    rainbow-delimiters
    rainbow-mode
    outline-magic
    rust-mode
    markdown-mode
    s
    smart-mode-line
    smex
    switch-window
    undo-tree
    exec-path-from-shell
    flymake-cursor

                                        ; Clojure
    ac-cider-compliment
    cider
    clojure-mode

                                        ; C++
    cpputils-cmake
    
                                        ; Latex utilities
    auctex

                                        ; Python
    virtualenvwrapper
    elpy
    )
  "A list of packages to install at launch."
  )

(dolist (p my-pkgs)
  (when (not (package-installed-p p))
    (package-install p)))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Or on Linux?
(setq is-linux (equal system-type 'gnu/linux))

;; What's the home folder?
(defvar home-dir)
(setq home-dir (expand-file-name "~"))

(add-to-list 'load-path (concat user-emacs-directory "init"))

(mapc 'require '(functions
                 settings
                 modes
                 hooks
                 bindings
                 eshell-setup
                 latex-setup
                 outline-setup
                 org-setup
                 python-setup
                 ;;                 clojure
                 ;;                 haskell-setup
                 ))

(add-to-list 'load-path (concat user-emacs-directory "scripts"))
(add-to-list 'load-path (concat user-emacs-directory "other"))

(setq custom-file (concat user-emacs-directory "init/custom.el"))
(load custom-file)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(custom-download-script
 "https://gist.github.com/gongo/1789605/raw/526e3f21dc7d6cef20951cf0ce5d51b90b7821ff/json-reformat.el"
 "json-reformat.el")

(custom-download-script
 "http://accad.osu.edu/~smay/RManNotes/rsl-mode.el"
 "rsl-mode.el")

;; A file with machine specific settings.
(load-file-if-exists "~/.emacs.d/init-local.el")

;; IRC configuration
;; Actual servers and such are loaded from irc.el
(load-file-if-exists "~/.emacs.d/init-irc.el")

;; Load magnars' string manipulation library
(require 's)

(require 'ack-and-a-half)

;; Seed RNG
(random t)

;; SML should respect theme colours
;; (setq sml/theme 'black)
(sml/setup)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (virtualenvwrapper undo-tree switch-window smex smart-mode-line rust-mode rainbow-mode rainbow-delimiters puppet-mode projectile pkgbuild-mode password-store paredit outline-magic nyan-mode mvn multiple-cursors markdown-mode+ magit iy-go-to-char ido-ubiquitous idle-highlight-mode hi2 haskell-mode go-mode flymake-cursor flycheck flx-ido exec-path-from-shell erlang elpy dockerfile-mode cpputils-cmake confluence browse-kill-ring auctex ack-and-a-half ace-jump-mode ac-cider-compliment))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
