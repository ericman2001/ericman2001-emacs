;; -*-emacs-lisp-*-

(defun string-starts-with (s arg)
      "returns non-nil if string S starts with ARG.  Else nil."
      (cond ((>= (length s) (length arg))
             (string-equal (substring s 0 (length arg)) arg))
            (t nil)))

(defun string-ends-with (s ending)
      "return non-nil if string S ends with ENDING."
      (let ((elength (length ending)))
        (string= (substring s (- 0 elength)) ending)))

(defun chomp (str)
      "Chomp leading and tailing whitespace from STR."
      (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                           str)
        (setq str (replace-match "" t t str)))
      str)

(defun semicolon-move-end-of-line ()
  (interactive)
  (cond
   ((string-starts-with (chomp (thing-at-point 'line)) "for")
	(progn (insert ";")
	(indent-according-to-mode)))
	(t (progn (skip-syntax-forward "^<" (line-end-position))
		  (insert ";")
		  (indent-according-to-mode)))
   )
)

(defun semicolon-anyway ()
  (interactive)
  (insert ";"))

;(local-set-key (kbd ";") 'semicolon-move-end-of-line)
;(local-set-key (kbd "C-;") 'semicolon-anyway)

(provide 'semicolon-move-end-of-line)
