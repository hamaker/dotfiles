(use-package go-mode
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (keys-l "e" 'go-test-current-test
          "r" 'go-test-current-file))

(provide 'init-go)
