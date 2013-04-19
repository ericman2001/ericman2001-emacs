;;; notepad++-like-beginning-of-line --- Moves your insertion point to the begining of a line like in notepad++
;; commentary:
;; code:
(defun notepad++-like-beginning-of-line()
  "Reproduce Notepad++ begining of line functionality.
Go to the position of the first non-whitespace character.
If there, go to the actual line begining.
This code is shamelessly taken from Ryan Wersal (ryanwersal.com)"
  (interactive)
  (let ((col (current-column)))
	(back-to-indentation)
	(if (= col (current-column)) (move-beginning-of-line nil))))

(provide 'notepad++-like-beginning-of-line)
