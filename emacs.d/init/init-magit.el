(use-package magit
  :defer t
  :init
  (setq magit-display-buffer-function 'magit-buffer-full-screen)
  ;; (evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
  ;; (evil-add-hjkl-bindings magit-log-mode-map 'emacs)
  ;; (evil-add-hjkl-bindings magit-process-mode-map 'emacs)
  ;; (evil-add-hjkl-bindings magit-status-mode-map 'emacs)
  ;; (keys :states nil
  ;;       :keymaps 'magit-file-section-map
  ;;       "K" 'magit-discard)
  (evil-define-key 'normal magit-blame-mode-map (kbd "q") 'magit-blame-quit)
  (evil-define-key 'emacs magit-blame-mode-map (kbd "q") 'magit-blame-quit)
  (evil-define-key 'normal magit-blame-mode-map (kbd "RET") 'magit-show-commit)

  (keys-l :states 'normal
    "gs" 'magit-status
    "gl" 'magit-log
    "gb" 'magit-blame))

(provide 'init-magit)
