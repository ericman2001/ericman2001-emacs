;; -*-emacs-lisp-*-

(require 'cl)

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

;; Windows isn't welcome in my home so this is an easy way to tell if I'm working at the office.
(defvar at-the-office-p (eq system-type 'windows-nt))

(defvar office-email-address "justin.hipple@zuerchertech.com")
(defvar home-email-address "brokenreality@gmail.com")

(defvar libdir (expand-file-name "~/.emacs.d"))
(defun libdir-file (file) (concat libdir "/" file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup Font
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evidently there are weird display issues if this is set after color theme is loaded?
;; Spawning a new frame resulted in very strange colors applied to all buffers it contained.
(defvar my-font
  (if at-the-office-p
      "-outline-Bitstream Vera Sans Mono-normal-normal-normal-mono-11-*-*-*-c-*-iso8859-1"
    "-apple-Bitstream_Vera_Sans_Mono-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1"))
(setq initial-frame-alist `((font . ,my-font)))
(setq default-frame-alist `((font . ,my-font)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup ELPA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add all the libs to the load path.
;; Ideally, ELPA would be the only package we would load manually
;; and all other packages would be managed through ELPA.
;; Unfortunately a handful of packages aren't in repositories so
;; we still have to load them manually for now.
(mapcar #'(lambda (path) (add-to-list 'load-path (libdir-file path))) '("elpa" "themes" "pymacs"))

;; ELPA package archive system.
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Maxmize the window and start with 50/50 vertical split.
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)
(add-hook 'window-setup-hook 'split-window-horizontally)

;; Enable syntax highlighting.
(require 'tomorrow-night-bright-theme)

;; Enable yasnippet for fancy templates.
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (libdir-file "elpa/yasnippet-0.6.1/snippets"))

;; Enable fancy window switching.
(require 'switch-window)

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

;; Allow IDO to do searching for files in a TAGS file.
(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
	(let ((enable-recursive-minibuffers t))
	  (visit-tags-table-buffer))
	(find-file
	 (expand-file-name
	  (ido-completing-read
	   "Project file: " (tags-table-files) nil t)))))

;; Turn off fuzzy matching on large search spaces (TAGS) because it's too slow.
(defvar af-ido-flex-fuzzy-limit (* 2000 5))
(defadvice ido-set-matches-1 (around my-ido-set-matches-1 activate)
  (let ((ido-enable-flex-matching (< (* (length (ad-get-arg 0)) (length ido-text))
                                     af-ido-flex-fuzzy-limit)))
	ad-do-it))

(global-set-key (kbd "C-x C-t") 'ido-find-file-in-tag-files)

;; Pymacs
;; NOTE: The Pymacs in ELPA is old.
;; I like to have the available all the time, even when not working in Python mode.
(require 'pymacs)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")

;; Ropemacs
(setq ropemacs-enable-shortcuts nil)
(setq ropemacs-enable-autoimport t)

;; Enable fancy autocompletion in code.
(require 'auto-complete-config)
(ac-config-default)
(setq ac-use-menu-map t)
(define-key ac-menu-map (kbd "C-n") 'ac-next)
(define-key ac-menu-map (kbd "C-p") 'ac-previous)
(setq ac-auto-show-menu 0.1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ensure the Command key is Meta on OSX.
(if (not at-the-office-p)
    (progn
      (setq mac-option-key-is-meta nil)
      (setq mac-command-key-is-meta t)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)))

;; Tabs cause nothing but headaches but I'm forced to use them at work.
(setq-default tab-width 4)
(setq-default indent-tabs-mode at-the-office-p)

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
(setq user-mail-address (if at-the-office-p office-email-address home-email-address))

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

;; Always show line numbers.
(global-linum-mode t)

;; I write C++ so default to the correct mode based on the filetypes I commonly use.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))

;; Qt's .pro and .pri files use IDL mode by default, make them not do that.
;; Also make .conf files use shell script mode.
(add-to-list 'auto-mode-alist '("\\.pro\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.pri\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\'" . shell-script-mode))

;; FIXME: This is still a work in progress...
;; Use Cygwin as the shell on Windows.
;; (if at-the-office-p
;; 	(progn
;; 	  ;;(setenv "PATH" (concat "C:/msys/home/justin.hipple/bin/:" (getenv "PATH")))
;; 	  ;;(setenv "HOME" (getenv (concat "/c/msys/home/" (getenv "USER")))) ;; Hack to get paths working properly...
;; 	  (setenv "PS1" "\\e[0;32m\\u@\\h \\e[0;33m\\W \\e[m\\n\\$ ")
;; 	  (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
;; 	  (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt nil t)
;; 	  (setq comint-prompt-read-only t)
;; 	  (setq explicit-shell-file-name "C:/cygwin/bin/bash")
;; 	  (setq shell-file-name explicit-shell-file-name)
;; 	  (setq explicit-bash-args '("--login" "-i"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Key Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Prevent accidentally killing emacs.
(defun confirm-exit-from-emacs()
  (interactive)
  (if (yes-or-no-p "Do you want to exit? ")
      (save-buffers-kill-emacs)))
(global-set-key (kbd "C-x C-c") 'confirm-exit-from-emacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C mode specific stuff.
(add-hook 'c-mode-common-hook
          (lambda()
			(local-set-key  (kbd "C-c o") 'ff-find-other-file)
			(font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\|NOTE\\):" 1 font-lock-warning-face t)))))

;; Python mode specific stuff.
(add-hook 'python-mode-hook
		  (lambda()
			(setq tab-width 4
				  py-indent-offset 4
				  indent-tabs-mode at-the-office-p
				  py-smart-indentation (not at-the-office-p)
				  python-indent 4)
			(ac-ropemacs-setup)
			(ropemacs-mode t)))

;; Clojure mode specific stuff.
(add-hook 'clojure-mode-hook
          (lambda()
            (paredit-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start Emacs Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(server-start)
