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

(defun my-kill-this-buffer ()
  "Kill the current buffer"
  (interactive)
  (kill-buffer (current-buffer)))

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
    (call-process-region (point) (mark) "xclip" nil nil nil "-selection" "c")))


(defun pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "xclip" t t nil "-selection" "c" "-o"))

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
  (indent-for-tab-command)
  (back-to-indentation)
  )

(defun insert-newline-below ()
  "Inserts a new line below point and places point in that line
w.r.t. indentation."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command)
  (back-to-indentation)
  )

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (let* ((patterns '(choises.js *.sql *.css *.htnl *.min.js *.svg *.json *.js))
         (exclude-strings (mapcar
                           (lambda (s) (format "--exclude=%s "  (shell-quote-argument (symbol-name s))))
                           patterns))
         (exclude-string (apply 'concat exclude-strings))
         )

    (shell-command
     (format "%s -f %s/TAGS %s -e -R %s"
             path-to-ctags
             (directory-file-name dir-name)
             exclude-string
             (directory-file-name dir-name)))
    (visit-tags-table (format "%s/TAGS" (directory-file-name dir-name)))
    )
  )

(defun rspec-assign-to-let ()
  "changes an assignment to a let statement"
  (interactive)
  (move-beginning-of-line nil)
  (re-search-forward "\\([^[:space:]]+\\) = \\(.*\\)$" nil t)
  (replace-match "let(:\\1) { \\2 }" ))

(defun ruby-symbol-to-string ()
  "changes an assignment to a let statement"
  (interactive)
  (re-search-backward ":")
  (re-search-forward ":\\(\\w+\\)" nil t)
  (replace-match "'\\1'" ))

(defun reload-firefox ()
  "reloads the current page in all Firefox windows"
  (interactive)
  (shell-command  "xdotool search --name \"Mozilla Firefox\" | xargs -n1 xvkbd -text \"\\[F5]\" -window" nil nil))

(defun my-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun my-decrement-number-decimal (&optional arg)
  (interactive "p*")
  (my-increment-number-decimal (if arg (- arg) -1)))

(defun incrementer-mode ()
  "add increment number shortcut to local keymap"
  (interactive)
  (keys :keymap (current-local-map)
        "C-M-a" 'my-increment-number-decimal
        "C-M-d" 'my-decrement-number-decimal
        )
  )

(defun format-whole-buffer ()
  "formats the whole buffer"
  (interactive)
  (indent-region  (point-min) (point-max)))

(defun increase-font-size (step)
  "increase the font size by 8"
  (interactive "p")
  (set-face-attribute 'default nil :height (+ (face-attribute 'default :height) 5))
  )

(defun decrease-font-size (step)
  "increase the font size by step or 5"
  (interactive "p")
  (set-face-attribute 'default nil :height (- (face-attribute 'default :height) (if step step 5)))
  )

(defun kmacro-reset-counter ()
  "resets the macro counter to 0"
  (interactive)
  (kmacro-set-counter 0)
  )

(defun rotate-windows (arg)
  "Rotate your windows; use the prefix argument to rotate the other direction"
  (interactive "P")
  (if (not (> (count-windows) 1))
      (message "You can't rotate a single window!")
    (let* ((rotate-times (prefix-numeric-value arg))
           (direction (if (or (< rotate-times 0) (equal arg '(4)))
                          'reverse 'identity)))
      (dotimes (_ (abs rotate-times))
        (dotimes (i (- (count-windows) 1))
          (let* ((w1 (elt (funcall direction (window-list)) i))
                 (w2 (elt (funcall direction (window-list)) (+ i 1)))
                 (b1 (window-buffer w1))
                 (b2 (window-buffer w2))
                 (s1 (window-start w1))
                 (s2 (window-start w2))
                 (p1 (window-point w1))
                 (p2 (window-point w2)))
            (set-window-buffer-start-and-point w1 b2 s2 p2)
            (set-window-buffer-start-and-point w2 b1 s1 p1)))))))
(provide 'init-functions)
