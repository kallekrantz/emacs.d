(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "~/.virtualenvs/")

(require 'pytest)
(add-to-list 'pytest-project-names "runtests")
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key "\C-ca" 'pytest-all)
            (local-set-key "\C-cm" 'pytest-module)
            (local-set-key "\C-c." 'pytest-one)
            (local-set-key "\C-cd" 'pytest-directory)
            (local-set-key "\C-cpa" 'pytest-pdb-all)
            (local-set-key "\C-cpm" 'pytest-pdb-module)
            (local-set-key "\C-cp." 'pytest-pdb-one)))

(provide 'python-setup)
