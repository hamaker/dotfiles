(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq exec-path-from-shell-check-startup-files nil)
(savehist-mode 1)

(setq insert-directory-program "/usr/local/bin/gls")

(setq package-enable-at-startup nil)
(package-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/init"))

(require 'init-use-package)
(require 'init-functions)
(require 'init-editor-packages)
(require 'init-editor-custom)
(require 'init-evil)
(require 'init-magit)
(require 'init-org)
; (require 'init-lisp)
; (require 'init-go)

;; (require 'init-clojure)
; (require 'symbol-focus)
(add-hook 'after-init-hook 'global-company-mode)

(unless window-system
  (add-hook 'linum-before-numbering-hook
	    (lambda ()
	      (setq-local linum-format-fmt
			  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
			    (concat "%" (number-to-string w) "d"))))))

(defun linum-format-func (line)
  (concat
   (propertize (format linum-format-fmt line) 'face 'linum)
   (propertize "\u2502" 'face 'linum)))

(unless window-system
  (setq linum-format 'linum-format-func))

;; Start emacs in dired mode.
(dired ".")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-backends
   (quote
    (company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
                  (company-dabbrev-code company-gtags company-etags company-keywords company-files)
                  company-oddmuse company-dabbrev)))
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 0)
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "2a5be663818e1e23fd2175cc8dac8a2015dcde6b2e07536712451b14658bbf68" "0b6645497e51d80eda1d337d6cabe31814d6c381e69491931a688836c16137ed" default)))
 '(global-linum-mode t)
 '(package-selected-packages
   (quote
    (helm-ag feature-mode gherkin-mode evil-repeat smooth-scrolling rubocop dired-sort fuzzy dired+ move-text gotest yaml-mode discover yafolding git-gutter web-mode minitest ruby-end evil-numbers ruby-test-mode hlinum auto-complete go-mode coffee-mode powerline-evil smart-mode-line-powerline-theme haml-mode rspec-mode projectile-rails ## alchemist evil-cleverparens indent-guide aggressive-indent paredit magit evil-search-highlight-persist evil-nerd-commenter evil-surround evil which-key use-package undo-tree helm-projectile general exec-path-from-shell better-defaults base16-theme ag)))
 '(rspec-command-options "--format p")
 '(ruby-test-rspec-options (quote ("--format p")))
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(sml/mode-width
   (if
       (eq
        (powerline-current-separator)
        (quote arrow))
       (quote right)
     (quote full)))
 '(sml/pos-id-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote powerline-active2))))
     (:propertize " " face powerline-active2))))
 '(sml/pos-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote sml/global))))
     (:propertize " " face sml/global))))
 '(sml/pre-id-separator
   (quote
    (""
     (:propertize " " face sml/global)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   (quote sml/global)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active2)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-search-highlight-persist-highlight-face ((t (:background "#fff" :foreground "#333"))))
 '(font-lock-comment-face ((t (:foreground "color-243"))))
 '(isearch ((t (:background "#fff" :foreground "#555"))))
 '(isearch-fail ((t (:background "#fff" :foreground "red"))))
 '(lazy-highlight ((t (:background "#fff" :foreground "#333")))))
