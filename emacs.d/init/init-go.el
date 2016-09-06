(use-package go-mode
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

(provide 'init-go)
