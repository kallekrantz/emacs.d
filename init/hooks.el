;; Helper for compilation. Close the compilation window if
;; there was no error at all.
(setq compilation-finish-functions 'compile-autoclose)
(defun compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
         (bury-buffer "*compilation*")
         (winner-undo)
         (message "Build successful."))
        (t
         (message "Compilation exited abnormally: %s" string))))
;; Specify my function (maybe I should have done a lambda function)


;; for enabling cpputils
(add-hook 'c-mode-common-hook
          (lambda ()
            (if (derived-mode-p 'c-mode 'c++-mode)
                (cppcm-reload-all)
              )))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Set as a minor mode for Python
(require 'flycheck)
(add-hook 'python-mode-hook 'flycheck-mode)
(require 'project-explorer)
(provide 'hooks)
