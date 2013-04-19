;; -*-emacs-lisp-*-

(defun semicolon-move-end-of-line ()
  (interactive)
  (skip-syntax-forward "^<" (line-end-position))
  (insert ";")
  (indent-according-to-mode))

(defun semicolon-anyway ()
  (interactive)
  (insert ";"))

;(local-set-key (kbd ";") 'semicolon-move-end-of-line)
;(local-set-key (kbd "C-;") 'semicolon-anyway)

(provide 'semicolon-move-end-of-line)

