;; -*-emacs-lisp-*-

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration Menu Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bell-volume 0)
 '(c-basic-offset 4)
 '(c-default-style (quote ((c-mode . "bsd") (c++-mode . "bsd") (java-mode . "java") (other . "bsd"))))
 '(column-number-mode t)
 '(get-frame-for-buffer-default-instance-limit nil)
 '(gutter-buffers-tab-visible-p nil)
 '(make-backup-files nil)
 '(paren-mode (quote paren) nil (paren))
 '(show-paren-mode t)
 '(sound-load-list nil)
 '(tool-bar-mode nil)
 '(visible-bell t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions and variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar using-windows-p (eq system-type 'windows-nt))
(defvar using-osx-p (eq system-type 'darwin))
(defvar using-linux-p (eq system-type 'gnu/linux))

(defvar office-email-address "eric.wahle@zuerchertech.com")
(defvar home-email-address "ericwahle@snappyl.com")

(defvar default-font-name
  (cond (using-windows-p "Bitstream Vera Sans Mono-12")
		(using-osx-p "Bitstream Vera Sans Mono-14")
		("Bitstream Vera Sans Mono-10")))

(defun libdir-file (file)
  (concat (expand-file-name "~/.emacs.d") "/" file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup ELPA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add all the libs to the load path.
(mapcar #'(lambda (path) (add-to-list 'load-path (libdir-file path))) '("themes"))
(add-to-list 'load-path (libdir-file "ericman2001"))

;; ELPA package archive system.
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						 ("marmalade" . "http://marmalade-repo.org/packages/")
						 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use a decent font.
;; Evidently there are weird display issues if this is set after color theme is loaded?
;; Spawning a new frame resulted in very strange colors applied to all buffers it contained.
(add-to-list 'default-frame-alist `(font . ,default-font-name))

;; Maxmize the window.
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

;; Enable syntax highlighting.
(require 'tomorrow-night-bright-theme)

;; Enable yasnippet for fancy templates.
(require 'yasnippet)
(yas-global-mode t)
(yas/load-directory (libdir-file "elpa/yasnippet-20130218.2229/snippets"))
(yas/load-directory (libdir-file "snippets"))

;; Enable fancy window switching.
(require 'switch-window)

;; Enable fast in-buffer navigation.
(require 'ace-jump-mode)
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; Enable IDO to handle buffer switching and such.
(require 'ido)
(ido-mode t)
(setq ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" "^\*scratch" ".*Completion" "^\*Ido") ; ignore these
	  ido-everywhere t            ; use for many file dialogs
	  ido-case-fold  t            ; be case-insensitive
	  ido-enable-flex-matching t  ; be flexible
	  ido-max-prospects 5         ; don't spam my minibuffer
	  ido-confirm-unique-completion t ; wait for RET, even with unique completion
	  ido-auto-merge-werk-directories-length -1) ; new file if no match

;; Turn off fuzzy matching on large search spaces (TAGS) because it's too slow.
(defadvice ido-set-matches-1 (around my-ido-set-matches-1 activate)
  (let ((ido-enable-flex-matching (< (* (length (ad-get-arg 0)) (length ido-text)) 10000)))
	ad-do-it))

;; Also use IDO to search for files in a TAGS file.
(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
	(let ((enable-recursive-minibuffers t))
	  (visit-tags-table-buffer))
	(find-file
	 (expand-file-name
	  (ido-completing-read
	   "Project file: " (tags-table-files) nil t)))))
(global-set-key (kbd "C-x C-t") 'ido-find-file-in-tag-files)

;; Enable fancy autocompletion in code.
(require 'auto-complete-config)
(ac-config-default)
(setq ac-use-menu-map t)
(define-key ac-menu-map (kbd "C-n") 'ac-next)
(define-key ac-menu-map (kbd "C-p") 'ac-previous)
(setq ac-auto-show-menu 0.1)

;; Better highlighting functionality for symbol at point.
(require 'highlight-symbol)
(global-set-key (kbd "<C-f3>") 'highlight-symbol-at-point)
(global-set-key (kbd "<M-f3>") 'highlight-symbol-remove-all)
(global-set-key (kbd "<f3>") 'highlight-symbol-next)
(global-set-key (kbd "<S-f3>") 'highlight-symbol-prev)
(global-set-key (kbd "<C-M-f3>") 'highlight-symbol-query-replace)

;; Automatically add closing character for characters that are commonly paired.
(electric-pair-mode t)

;; Ensure the Command key is Meta on OSX.
(if using-osx-p
	(progn
	  (setq mac-option-key-is-meta nil)
	  (setq mac-command-key-is-meta t)
	  (setq mac-command-modifier 'meta)
	  (setq mac-option-modifier nil)))

;; Tabs cause nothing but headaches but I'm forced to use them at work.
(setq-default tab-width 4)
(setq-default indent-tabs-mode using-windows-p)

;; Always show the buffer name in the title bar.
(setq-default
 frame-title-format
 (list '((buffer-file-name
		  "Emacs - %f"
		  (dired-directory
		   dired-directory
		   (revert-buffer-function " %b" ("%b - Dir:  " default-directory)))))))

(setq-default
 icon-title-format
 (list '((buffer-file-name
		  "Emacs - %f"
		  (dired-directory
		   dired-directory
		   (revert-buffer-function " %b"("%b - Dir:  " default-directory)))))))

;; Set an appropriate email address depending on whether I'm at work or home.
;; If I'm on Windows it's because I'm doing "real work", so use my work email address.
(setq user-mail-address (if using-windows-p office-email-address home-email-address))

;; Don't show the damn splash screen.
(setq inhibit-splash-screen t)

;; Typing replaces the selected region.
(delete-selection-mode t)

;; Don't make me type out 'yes' and 'no'.
(fset 'yes-or-no-p 'y-or-n-p)

;; Don't prompt me when killing buffers with active processes.
(setq kill-buffer-query-functions
	  (remq 'process-kill-buffer-query-function
			kill-buffer-query-functions))

;; Default to 'string' mode when using re-builder.
(setq reb-re-syntax 'string)

;; don't use line numbers. This is broken sometimes and it makes me angry!
;; use M-g g to get around or check status line
(global-linum-mode 0)

;; I write C++ so default to the correct mode based on the filetypes I commonly use.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))

;; Qt's .pro and .pri files use IDL mode by default, make them not do that.
;; Also make .conf files use shell script mode.
(add-to-list 'auto-mode-alist '("\\.pro\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.pri\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\'" . shell-script-mode))

;; Use aweseom js2 mode for editing Javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; You can actually use a real terminal from within Emacs on Linux once the
;; PATH environment variable is set correctly.
;; Also make sure that we use a colorized prompt.
(if using-linux-p
	(progn
	  (setenv "PATH" (concat (getenv "PATH") ":~/bin"))
	  (setenv "color_prompt" "yes")
	  (setq exec-path (append exec-path '("~/bin")))))

;; Close Emacs with confirmation.
(defun confirm-exit-from-emacs ()
  (interactive)
  (if (yes-or-no-p "Do you want to exit? ")
	  (save-buffers-kill-emacs)))
(global-set-key (kbd "C-x C-c") 'confirm-exit-from-emacs)

;; Starts an MSYS shell, useful when working on Windows.
;; FIXME: Someday I'll get this working correctly.
(defun start-msys-shell ()
  (interactive)
  (setq shell-file-name "bash.exe")
  (setq explicit-shell-file-name shell-file-name)
  (setenv "SHELL" explicit-shell-file-name)
  (setq w32-quote-process-args t)
  (setq explicit-bash.exe-args '("--login" "-i"))
  (setq shell-command-switch "-c")
  (shell))

;; A handy function for starting a shell buffer in a new window or visiting
;; an existing shell buffer.
;; MSYS still doesn't work well enough to replace shell on Windows, unfortunately.
(defun start-shell ()
  (interactive)
  (let ((shell-buffer-name (if using-windows-p "*shell*" "*ansi-term*")))
	(if (not (get-buffer shell-buffer-name))
		(progn
		  (split-window-sensibly (selected-window))
		  (other-window 1)
		  (if using-windows-p (shell) (ansi-term (getenv "SHELL"))))
	  (switch-to-buffer-other-window shell-buffer-name))))
(global-set-key (kbd "<f5>") 'start-shell)

;;work in progress for semicolons
;;enable manually
(require 'semicolon-move-end-of-line)

;;home key behavior modification
(require 'notepad++-like-beginning-of-line)
(global-set-key [home] 'notepad++-like-beginning-of-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode Hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C mode specific stuff.
(add-hook 'c-mode-common-hook
		  (lambda ()
			;; Make these patterns more evident in code.
			(font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\|NOTE\\):" 1 font-lock-warning-face t)))
			;; Handy for jumping between .h and .cpp files.
			(local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;; Python mode specific stuff.
(add-hook 'python-mode-hook
		  (lambda ()
			;; Setup the style based one whether I'm at home or the office.
			(setq tab-width 4
				  py-indent-offset 4
				  indent-tabs-mode using-windows-p
				  py-smart-indentation (not using-windows-p)
				  python-indent 4)))

;; Clojure mode specific stuff.
(add-hook 'clojure-mode-hook
		  (lambda ()
			(paredit-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start Emacs Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(server-start)
