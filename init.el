(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized")
(add-to-list 'load-path "/usr/local/share/emacs/site-list")
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(add-to-list 'load-path "~/.emacs.d/evil")

; no splash screen
(setq inhibit-startup-message t)

; empty scrath buffer
(setq initial-scratch-message "")

(require 'info+)
(require 'undo-tree)
(require 'dirtree)
(require 'evil)
(evil-mode 1)

;(require 'color-theme-solarized)
(require 'use-package)
(require 'flymake-clang-c++)
(add-hook 'c++-mode-hook 'flymake-clang-c++-load)

;; Auto complete
(add-to-list 'load-path "~/.emacs.d/auto-complete-1.3.1")
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

(require 'xivo-cpp-style)
(require 'asterisk-c-style)
(require 'xcscope)

;; Project specific configuration
(load "projects.el")

;; Start in server mode
(server-start)

;; Make pages 80 columns width
(setq-default fill-column 80)

;; Interactive search
(ido-mode t)
(defalias 'list-buffers 'ibuffer)

;; Use M-<up> and M-<down> to move the current line up or down
(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(global-set-key [\M-up] 'move-text-up)
(global-set-key [\M-down] 'move-text-down)

;; Use Ctrl-F11 to recompile
(global-set-key [\C-f11] 'recompile)

(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")
  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion
      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))
      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )
      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let
  ;; put the point in the lowest line and return
  (next-line arg))

(global-set-key (kbd "C-S-y") 'duplicate-line)

;; Append a directory part separated by a | to buffer name with non-unique names
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator "|")

;; Window movement using Shift-arrow
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; Disable all flymake extensions
(setq flymake-allowed-file-name-masks (list))

(use-package cc-mode
  :config
  (progn
    (setq c-default-style '((c-mode . "linux")))))

(use-package color-theme
  :init
  (progn
    ;(color-theme-initialize)				;; Load color themes
    ;(color-theme-solarized-dark)			;; Set a color theme
    (set-face-attribute 'default nil :height 100)	;; Set font size
    (column-number-mode t)				;; Show column number
    (setq visible-bell nil)				;; Turn off visual bell
    (setq ring-bell-function 'ignore)			;; Turn off audible bell
    (transient-mark-mode 0)				;; No selection hl
    (tool-bar-mode 0)					;; No tool bar
    (menu-bar-mode 0)					;; No menu bar
    (scroll-bar-mode 0)
    (show-paren-mode t)				;; Show matching paren
    (global-hl-line-mode t)				;; Hightlight current line
))

;; org-mode
(setq org-todo-keywords
       '((sequence "TODO" "IN PROGRESS" "|" "DONE")))

(use-package speedbar
  :commands (speedbar)
  :init (global-set-key (kbd "<f7>") 'speedbar))

(add-hook
 'find-file-hook
 (lambda ()
   (progn
     (define-coding-system-alias 'utf8 'utf-8)
     (define-coding-system-alias 'UTF-8 'utf-8))))

(set-face-attribute
 'default nil
 :font (concat "Droid sans mono-" "10"))

;; Key bindings
(global-set-key (kbd "C-z") 'undo)

(use-package whitespace
  :commands whitespace-mode
  :init
  (progn
    (setq whitespace-line-column 120)
    (add-hook 'find-file-hook
	      (lambda ()
		(whitespace-mode t))))
  :config
  (progn
    (delete 'space-mark whitespace-style) ;; Remove space markers
    (delete 'newline-mark whitespace-style) ;; Remove newline markers
    (delete 'spaces whitespace-style) ;; Remove space background
    (delete 'lines whitespace-style) ;; Remove long lines color
    (delete 'indentation whitespace-style))) ;; Remove tabs color

(use-package feature-mode
  :mode ("\\.feature$" . feature-mode))

(use-package php-mode
  :mode (("\\.php$" . php-mode)
	 ("\\.inc$" . php-mode)))

(use-package python
  :bind ("C-c C-j" . run-nosetests)
  :mode ("\\.py$" . python-mode)
  :init
  (progn
    (message "Initializing python..."))
  :config
  (progn
    (message "Configuring python...")
    (setq python-python-command "python")
    (setq py-pychecker-command "pyflakespep8.py")
    (setq py-pychecker-command-args (quote ("")))
    (defun run-nosetests ()
      "Runs nosetests with the project defined pythonpath"
      (interactive)
      (let ((command (concat (get-nosetests-command) " " (get-test-target))))
	(message command)
	(compile (concat command))))
    (defun get-test-target ()
      "Returns the name of the test module of an empty string"
      (if (buffer-file-name)
	  (if (equal "test" (substring (file-name-nondirectory (buffer-file-name)) 0 4))
	      (buffer-file-name)
	    "")
	""))
    (defun get-nosetests-command ()
      "Returns [PYTHONPATH=[project-pythonpath][:global-pythonpath]] nosetests"
      (let ((python-pythonpath (build-pythonpath)))
	(if (and python-pythonpath (> (length python-pythonpath) 0))
	    (concat "PYTHONPATH=" python-pythonpath " nosetests")
	  "nosetests")))
    (defun get-project-pythonpath ()
      "Returns a list of paths to include in the pythonpath"
      (when (and (boundp 'project-pythonpath) (> (length project-pythonpath) 0))
	project-pythonpath))
    (defun get-global-pythonpath ()
      "Returns a list of paths included in the environment variable PYTHONPATH"
      (let ((path (getenv "PYTHONPATH")))
	(when (and (boundp 'path) (> (length path) 0)
		   (split-string path ":")))))
    (defun build-pythonpath ()
      "Returns the union of {global,project}-pythonpath as string"
      (mapconcat 'identity
		 (delq nil (delete-dups
			    (append
			     (get-global-pythonpath)
			     (get-project-pythonpath)))) ":"))
    (defadvice python-calculate-indentation (around outdent-closing-brackets)
      "Handle lines beginning with a closing bracket and indent them so that
       they line up with the line containing the corresponding opening bracket."
      (save-excursion
	(beginning-of-line)
	(let ((syntax (syntax-ppss)))
	  (if (and (not (eq 'string (syntax-ppss-context syntax)))
		   (python-continuation-line-p)
		   (cadr syntax)
		   (skip-syntax-forward "-")
		   (looking-at "\\s)"))
	      (progn
		(forward-char 1)
		(ignore-errors (backward-sexp))
		(setq ad-return-value (current-indentation)))
	    ad-do-it))))
    (ad-activate 'python-calculate-indentation)))

(use-package pymacs
  :init
  (progn
    (message "Initializing pymacs...")
    (setenv "PYMACS_PYTHON" "python"))
  :config
  (progn
    (message "Configuring pymacs...")
    (pymacs-load "ropemacs" "rope-")))

;; Pyflakes for python
(when (load "flymake" t)
  (defun flymake-pychecker-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "flake8" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pychecker-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

;; Show flymake errors in mini-buffer
(defun my-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
   (let ((help (get-char-property (point) 'help-echo)))
    (if help (message "%s" help)))))

(add-hook 'post-command-hook 'my-flymake-show-help)

;; Magit
(use-package magit
  :commands (magit-status)
  :load-path "~/.emacs.d/magit/"
  :init  (global-set-key (kbd "C-x g") 'magit-status))

;; Lua
(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;; Enable narrowing of region
(put 'narrow-to-region 'disabled nil)

;; Mouse middle-click yanking no position
(setq mouse-yank-at-point t)

;; C-x C-m and C-c C-m == M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Enable snippets
(require 'yasnippet)
(yas-global-mode 1)

;; golang
(add-to-list 'load-path "/usr/local/go/misc/emacs" t)
(require 'go-mode-load)

(defun get-project-include-path ()
  "Returns a string containing every include paths of a project prefixed with -I"
  (if (boundp 'project-include-path)
    (add-list-prefix "-I" project-include-path)
    (list "")))

(defun add-list-prefix (prefix l)
  "Returns a list with all items in l prefixed with 'prefix'"
  (let ((result))
    (dolist (item l)
      (setq result (cons (concat prefix item) result)))
    result))

(defun run-project-init ()
  "Run project-init if it exists"
  (when (boundp 'project-init)
    (funcall project-init)))

;; Runs project init when a file is loaded
(add-hook 'find-file-hook 'run-project-init)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(rst-level-face-base-color ""))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
