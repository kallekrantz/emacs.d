;; Configure package manager
(require 'package)

;; Add Marmalade repo
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; ... and melpa. Melpa packages that exist on marmalade will have
;; precendence.
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-pkgs
  '(ace-jump-mode
    browse-kill-ring
    clojure-mode
    flycheck
    flx-ido
    haskell-mode
    hi2
    idle-highlight-mode
    ido-ubiquitous
    iy-go-to-char
    leuven-theme
    magit
    magit
    markdown-mode
    multiple-cursors
    nrepl
    nyan-mode
    paredit
    projectile
    rainbow-delimiters
    rainbow-mode
    smex
    switch-window
    undo-tree
    geiser
    quack)
  "A list of packages to install at launch.")

(dolist (p my-pkgs)
  (when (not (package-installed-p p))
    (package-install p)))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

(add-to-list 'load-path user-emacs-directory)

(mapc 'require '(init-functions
                 init-settings
                 init-modes
                 init-bindings
                 init-eshell))

(add-to-list 'load-path "~/.emacs.d/scripts/")

(setq custom-file "~/.emacs.d/init-custom.el")
(load custom-file)

;; A file with machine specific settings.
(load-file-if-exists "~/.emacs.d/init-local.el")

;; IRC configuration (erc)
;; Actual servers and such are loaded from irc.el
(load-file-if-exists "~/.emacs.d/init-irc.el")

;; Load magnars' string manipulation library
(require 's)

;; Seed RNG
(random t)

;; Start server for emacsclient
(server-start)
