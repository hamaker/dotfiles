;; Show column and linum in mode-line
(line-number-mode 1)

;; Nowrap
(setq-default truncate-lines t)

;; Prevent the cursor from blinking
(blink-cursor-mode 0)

;; Don't use messages that you don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

;; Spaces instead of tabs
(setq c-basic-offset 2
      tab-width 2
      indent-tabs-mode nil)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Disable backup files
(setq make-backup-files nil)

;; Deal with temp files
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; just type y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; ;; adjustable text-size
;; (global-set-key (kbd "C-=") 'text-scale-increase)
;; (global-set-key (kbd "C--") 'text-scale-decrease)

(setq window-min-height 10)
;; use system clipboard
(setq x-select-enable-clipboard t)

;; Skip messages in next-buffer command
(defadvice next-buffer (after avoid-messages-buffer-in-next-buffer)
  (when (string= "*Messages*" (buffer-name))
    (next-buffer)))

;; Skip messages in prev-buffer command
(defadvice previous-buffer (after avoid-messages-buffer-in-next-buffer)
  (when (string= "*Messages*" (buffer-name))
    (previous-buffer)))

;; Automaticaly add newline at and of document
(setq require-final-newline t)

;; Disable highlighting of matching par
(setq show-paren-mode t)

;; avoid hiding with M-h
(setq mac-pass-command-to-system nil)

;; disable visible-bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; cmd key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;; Encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; (setq ruby-end-insert-newline nil)

;; font settings
(set-face-attribute 'default nil :height 150)
(setq-default line-spacing 4)

;; Automatically remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'comint-exec-hook 'evil-normal-state)
(add-hook 'comint-exec-hook 'visual-line-mode)
(add-hook 'comint-exec-hook (lambda () (linum-mode 0)))
(add-hook 'ag-mode-hook (lambda () (linum-mode 0)))
(add-hook 'prog-mode-hook (lambda () (linum-mode 1)))
(add-hook 'rubocop-mode-hook (lambda () (linum-mode 0)))
(add-hook 'rubocop-mode-hook (lambda () (visual-line-mode 1)))

;; Less intriguing colors for isearch
(custom-set-faces
 '(isearch ((t (:background "#fff" :foreground "#555"))))
 '(isearch-lazy-highlight-face ((t (:background "#fff" :foreground "#333"))))
 `(evil-search-highlight-persist-highlight-face ((t (:background "#fff" :foreground "#333"))))
 '(isearch-fail ((t (:background "#fff" :foreground "red")))))

;; enabled line numbers
(linum-mode)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Use aspell for spell checking: brew install aspell --lang=en
(setq ispell-program-name "/usr/local/bin/aspell")

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Save cursor position for files.
(save-place-mode 1)

(bind-keys :map dired-mode-map
           ("-" . dired-up-directory))

(setq js-indent-level 2)
(global-hl-line-mode)
(xterm-mouse-mode)

(set-face-foreground 'mode-line "#ffffff")
(set-face-background 'mode-line "#2c2c2c")
(set-face-background 'mode-line-inactive "#1d1d1d")
(setq path-to-ctags "/usr/local/bin/ctags")
(provide 'init-editor-custom)
