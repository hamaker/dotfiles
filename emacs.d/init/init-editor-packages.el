(use-package better-defaults
  :pin melpa-stable)

(use-package company
  :diminish company-mode
  :init
  (global-company-mode t)
  :config
  ;; (define-key prog-mode-map (kbd "TAB") 'company-complete)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer))

(use-package base16-theme
  :init
  (load-theme 'base16-default-dark t))

(use-package exec-path-from-shell
  :pin melpa-stable
  :config
  (exec-path-from-shell-initialize))

(use-package general
  :config
  (setq general-default-states '(normal motion))
  (general-create-definer keys-l
    :keymaps 'global
    :prefix ",")
  (defalias 'keys 'general-define-key)

  (keys-l :states 'normal
    :keymaps 'emacs-lisp-mode-map
    "e" 'eval-last-sexp
    "d" 'helm-apropos)

  (keys-l :states 'normal
    "," 'my-switch-to-other-buffer
    "k" 'kill-other-buffers
    "q" 'my-kill-this-buffer
    "w" 'delete-window
    "x" 'next-code-buffer
    "S" 'sf/focus-at-point
    "z" 'previous-code-buffer
    "V" 'open-emacs-config
    "o" 'other-frame
    "O" 'make-frame-command
    "y" 'pbcopy
    "m" 'maximize-window-vertically
    "=" 'format-whole-buffer
    "SPC" 'toggle-maximize-buffer
    "hm" 'describe-mode
    "hv" 'describe-variable
    "hf" 'describe-function
    "hk" 'describe-key
    "v" 'customize-variable)
  )

(use-package ruby-mode
  :config
  (keys-l :states 'normal
    "S" 'ruby-symbol-to-string)
  (add-to-list 'auto-mode-alist
               '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . ruby-mode)))

(use-package jsx-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode)))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode t))

(use-package dired+
  :config
  (keys "C-h" 'evil-window-left
        "C-j" 'evil-window-down
        "C-k" 'evil-window-up
        "C-l" 'evil-window-right)
  (diredp-toggle-find-file-reuse-dir 1))

(use-package ag
  :defer t
  :init
  (setq ag-reuse-buffers t)
  (keys-l :states 'normal "s" 'ag))

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-side-window-max-width 0.7
        which-key-add-column-padding 1)
  (which-key-mode +1)
  (which-key-setup-side-window-right))

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode)
  (setq smooth-scroll-margin 4))

(use-package csharp-mode
  :config
  (setq tab-width 4)
  (setq c-basic-offset 4))

(use-package helm
  :init
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-projectile-fuzzy-match t
        helm-autoresize-mode t
        helm-apropos-fuzzy-match t
        helm-recentf-fuzzy-match t)
  :config
  (keys-l :states 'normal
          "b" 'helm-buffers-list
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
  (keys-l :states 'normal "p" 'projectile-command-map)
  (projectile-global-mode t))

(use-package helm-projectile
  :config
  (keys-l :states 'normal
    "f" 'helm-projectile
    "F" 'helm-find-files))

(use-package projectile-rails
  :init
  (add-hook 'projectile-mode-hook 'projectile-rails-on)
  :config
  (keys-l :states 'normal
    ".m" 'projectile-rails-find-model
    ".c" 'projectile-rails-find-controller)
  )

(use-package ruby-test-mode
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  (keys-l :states 'normal
          "e" 'save-and-ruby-test-run-at-point
          "r" 'save-and-ruby-test-run))

(use-package yafolding
  :config
  (keys-l :states 'normal
          "t" 'yafolding-toggle-element))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-enable-auto-pairing t))

(use-package rubocop
  :config
  (keys-l :states 'normal
    "R" 'rubocop-check-project))

(use-package csharp-mode
  :config
  (add-hook 'csharp-mode-hook (lambda () (setq c-basic-offset 4)))
  )

(provide 'init-editor-packages)
