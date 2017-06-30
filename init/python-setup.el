(require 'elpy)
(require 'f)

(elpy-enable)
(require 'pyenv-mode-auto)
(add-hook 'before-save-hook 'whitespace-cleanup)

(provide 'python-setup)
;;; python-setup ends here
