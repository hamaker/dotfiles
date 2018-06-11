(use-package evil
  :init
  (setq evil-intercept-esc 'always
        evil-want-fine-undo 'fine
        evil-shift-width 2)
  (modify-syntax-entry ?_ "w")
  :config
  (evil-mode t)
  (evil-ex-define-cmd "W" "w")
  (evil-ex-define-cmd "A" '(lambda ()
                             (interactive)
                             (projectile-rails-find-current-spec)))
  (evil-ex-define-cmd "AV" '(lambda ()
                              (interactive)
                              (evil-window-vsplit)
                              (windmove-right)
                              (projectile-rails-find-current-spec)))
  (define-key evil-insert-state-map (kbd "M-RET") 'evil-open-below)
  (define-key evil-insert-state-map (read-kbd-macro "<S-return>") 'evil-open-above)
  (define-key evil-normal-state-map (kbd "M-RET") 'evil-open-below)
  (define-key evil-normal-state-map (read-kbd-macro "<S-return>") 'evil-open-above)
  (define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)
  (define-key evil-motion-state-map "\C-y" nil) ;; unbind this keybinding so I can use emacs yank
  (add-hook 'magit-blame-mode-hook '(lambda () (evil-normalize-keymaps)))

  (keys :states nil
        :keymaps '(minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-isearch-map)
        [escape] 'minibuffer-keyboard-quit)



  (keys "C-n" 'next-error
        "C-o" 'evil-jump-backward
        "C-S-o" 'evil-jump-forward
        "C-p" 'previous-error
        "C-h" 'evil-window-left
        "C-j" 'evil-window-down
        "C-k" 'evil-window-up
        "C-l" 'evil-window-right
        "C-," 'evil-repeat-find-char-reverse
        "[ SPC" 'insert-newline-above
        "] SPC" 'insert-newline-below)

  (keys :states 'normal
        "-" 'dired-current
        "#" 'evil-search-word-forward)

  (keys-l :states 'visual
    "Y" 'pbcopy)


  (lexical-let ((default-color (cons (face-background 'mode-line)
                                     (face-foreground 'mode-line))))
    (add-hook 'post-command-hook
              (lambda ()
                (let ((color (cond ((minibufferp) default-color)
                                   ((evil-insert-state-p) '("#2010ff" . "#ffffff"))
                                   ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                                   ((buffer-modified-p)   '("#d01f00" . "#ffffff"))
                                   (t default-color))))
                  (set-face-background 'mode-line (car color))
                  (set-face-foreground 'mode-line (cdr color)))))))

(use-package evil-surround
  :config
  (global-evil-surround-mode t))

(use-package evil-nerd-commenter
  :defer t
  :init
  (keys-l
   :states '(normal visual)
   "c" 'evilnc-comment-or-uncomment-lines))

(use-package evil-search-highlight-persist
  :init
  (global-evil-search-highlight-persist t)
  :config
  (keys :keymaps 'prog-mode-map
        :states '(normal)
        "RET" 'evil-search-highlight-persist-remove-all))

(use-package evil-string-inflection
  :ensure t)

(use-package evil-numbers
  :config
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

(provide 'init-evil)
