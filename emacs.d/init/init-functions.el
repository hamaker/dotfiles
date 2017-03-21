(defun my-switch-to-other-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun open-emacs-config ()
  "Open the emacs packages file for quick changes."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun circle-code-buffers (circle-fn)
  (let ((bread-crumb (buffer-name)))
    (funcall circle-fn)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (string-match-p "dired-mode" (buffer-local-value 'major-mode (get-buffer buffer-name)))
         (not (equal bread-crumb (buffer-name)))
         (buffer-file-name (buffer-name)))
      (funcall circle-fn))))

(defun next-code-buffer ()
  "Open next active buffer, ignoring non-code related buffers."
  (interactive)
  (circle-code-buffers 'next-buffer))

(defun previous-code-buffer ()
  "Open next active buffer, ignoring non-code related buffers."
  (interactive)
  (circle-code-buffers 'previous-buffer))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(defun minibuffer-keyboard-quit ()
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun display-buffer-full-screen (buffer alist)
  (delete-other-windows)
  (set-window-dedicated-p nil nil)
  (set-window-buffer nil buffer)
  (get-buffer-window buffer))

(defun magit-buffer-full-screen (buffer)
  (if magit-display-buffer-noselect
      (magit-display-buffer-traditional buffer)
    (display-buffer buffer '(display-buffer-full-screen))))

(defun dired-current ()
  (interactive)
  (dired "."))

(defun pbcopy ()
  (interactive)
  (let ((deactivate-mark t))
    (call-process-region (point) (mark) "pbcopy")))

(defun pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

(defun save-and-ruby-test-run ()
  (interactive)
  (save-buffer)
  (ruby-test-run))

(defun save-and-ruby-test-run-at-point ()
  (interactive)
  (save-buffer)
  (ruby-test-run-at-point))

(defun toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(defun maximize-window-vertically ()
  "set the window to its maximal vertical size"
  (interactive)
  (window-resize nil (window-max-delta)))

(defun insert-newline-above ()
  "Inserts a new line above point and places point in that line
w.r.t. indentation."
  (interactive)
  (beginning-of-line)
  (newline)
  )

(defun insert-newline-below ()
  "Inserts a new line below point and places point in that line
w.r.t. indentation."
  (interactive)
  (end-of-line)
  (newline))

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
  (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name)))
  )

(defun rspec-assign-to-let ()
  "changes an assignment to a let statement"
  (interactive)
  (move-beginning-of-line nil)
  (re-search-forward "\\(\\w+\\) = \\(.*\\)$" nil t)
  (replace-match "let(:\\1) { \\2 }" ))

(provide 'init-functions)
