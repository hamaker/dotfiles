(use-package better-defaults
  :pin melpa-stable)

(use-package auto-complete
  :init
  (ac-config-default))

(use-package base16-theme
  :init
  (load-theme 'base16-default-dark t))

(use-package exec-path-from-shell
  :pin melpa-stable
  :config
  (exec-path-from-shell-initialize))

(use-package general
  :config
  (progn
    (setq general-default-states '(normal emacs motion))
    (general-create-definer keys-l :prefix ",")
    (defalias 'keys 'general-define-key)

    (keys-l :states 'normal
            :keymaps 'emacs-lisp-mode-map
            "e" 'eval-last-sexp
            "d" 'helm-apropos)

    (keys-l "hk" 'describe-key
            "hm" 'describe-mode
            "hf" 'describe-function
            "hv" 'describe-variable
            "," 'my-switch-to-other-buffer
            "k" 'kill-other-buffers
            "q" 'kill-buffer
            "w" 'delete-window
            "x" 'next-code-buffer
            "S" 'sf/focus-at-point
            "z" 'previous-code-buffer
            "v" 'open-emacs-config
            "o" 'other-frame
            "O" 'make-frame-command)))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode t))

(use-package ag
  :defer t
  :init
  (setq ag-reuse-buffers t)
  (keys-l "s" 'ag))

;; (use-package company
;;   :diminish company-mode
;;   :init
;;   (global-company-mode t)
;;   :config
;;   (define-key prog-mode-map (kbd "<tab>") 'company-complete)
;;   (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
;;   (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
;;   (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer))


(use-package project-explorer
  :defer t
  :init
  (setq pe/omit-gitignore t)
  (keys-l "a" 'project-explorer-toggle)
  :config
  (keys :keymaps 'project-explorer-mode-map
        "o" 'pe/return
        "r" 'pe/rename-file
        "TAB" 'pe/tab
        "q" 'pe/quit
        "c" 'pe/copy-file
        "R" 'revert-buffer
        "d" 'pe/delete-file
        "RET" 'pe/return))

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-side-window-max-width 0.7
        which-key-add-column-padding 1)
  (which-key-mode +1)
  (which-key-setup-side-window-right))

(use-package helm
  :init
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-projectile-fuzzy-match t
        helm-autoresize-mode t
        helm-apropos-fuzzy-match t
        helm-recentf-fuzzy-match t)
  :config
  (keys-l "b" 'helm-buffers-list
          "y" 'helm-show-kill-ring)
  (keys :states nil
        "M-x" 'helm-M-x))

(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-create-missing-test-files t
        projectile-completion-system 'helm
        projectile-switch-project-action 'helm-projectile-find-file)
  :config
  (keys-l "p" 'projectile-command-map)
  (projectile-global-mode t))

(use-package helm-projectile
  :config
  (keys-l "f" 'helm-projectile
          "F" 'helm-find-files))
(use-package rspec-mode
  :config
  (keys-l "e" 'rspec-verify-single)
  (keys-l "r" 'rspec-verify-all))

(use-package projectile-rails
  :init
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

(use-package ruby-test-mode
  :config
  (keys-l "R" 'ruby-test-run))

(provide 'init-editor-packages)
