(require 'elpy)

(when (require 'flycheck nil t)
  (setq elpy-default-minor-modes (delete 'flymake-mode elpy-modules))
  (add-to-list 'elpy-modules 'flycheck-mode))
(elpy-enable)

(provide 'python-setup)
;;; python-setup ends here
