(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "/usr/local/share/emacs/site-list")
(setq enable-local-eval t)  ;; Allow eval in dir-locals.el

;(setq flymake-log-level 3) ;; enable flymake logging

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(ido-mode)
(load-theme 'tango-dark)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq venv-location "~/.virtualenvs")

(require 'iso-transl)

(defun my-jedi-reset ()
  (when (fboundp 'jedi:stop-server)
    (jedi:stop-server))
  (jedi:setup))
(add-hook 'python-mode-hook 'my-jedi-reset)
(add-hook 'venv-postactivate-hook 'my-jedi-reset)
(add-hook 'venv-postdeactivate-hook 'my-jedi-reset)
(setq jedi:complete-on-dot t)

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

; no splash screen
(setq inhibit-startup-message t)

; empty scrath buffer
(setq initial-scratch-message "")

(require 'info+)
(require 'undo-tree)
(require 'dirtree)

(require 'use-package)
(require 'flymake-clang-c++)
(defun pcm:post-dir-locals-mode-hook ()
  (when (derived-mode-p 'c++mode)
    (flymake-clang-c++-load)))
(add-hook 'hack-local-variable-hook #'pcm:post-dir-locals-mode-hook)

;; Auto complete
;; (add-to-list 'load-path "~/.emacs.d/auto-complete-1.3.1")
;; (require 'auto-complete)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (global-auto-complete-mode t)

(require 'xivo-cpp-style)
(require 'asterisk-c-style)

;; Project specific configuration
(load "projects.el")

;; Start in server mode
(server-start)

;; Make pages 80 columns width
(setq-default fill-column 80)

;; ;; Interactive search
;; (ido-mode t)
;; (defalias 'list-buffers 'ibuffer)

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

(set-face-attribute 'default nil :height 100)  ;; Set font size
(column-number-mode t)  ;; Show column number
(setq visible-bell nil)  ;; Turn off visual bell
(setq ring-bell-function 'ignore)  ;; Turn off audible bell
(tool-bar-mode 0) ;; No tool bar
(menu-bar-mode 0) ;; No menu bar
(scroll-bar-mode 0)
(show-paren-mode t)
(global-linum-mode t)

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

;; (set-face-attribute
;;  'default nil
;;  :font (concat "Droid sans mono-" "10"))

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
  :mode ("\\.py$" . python-mode)
  :config
  (progn
    (setq python-python-command "python")
    (setq py-pychecker-command "flakes8.py")
    (setq py-pychecker-command-args (quote ("")))
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

;; Lua
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

(setq auto-mode-alist (cons '("\\.qml$" . javascript-mode) auto-mode-alist))

;; Enable narrowing of region
(put 'narrow-to-region 'disabled nil)

;; Mouse middle-click yanking no position
(setq mouse-yank-at-point t)

;; C-x C-m and C-c C-m == M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(defun get-project-include-path ()
  "Returns a string containing every include paths of a project prefixed with -I"
  (if (boundp 'pcm:project-include-path)
    (add-list-prefix "-I" pcm:project-include-path)
    (list "")))

(defun add-list-prefix (prefix l)
  "Returns a list with all items in l prefixed with 'prefix'"
  (let ((result))
    (dolist (item l)
      (setq result (cons (concat prefix item) result)))
    result))

(defun pcm:run-project-init ()
  "Run project-init if it exists"
  (when (boundp 'pcm:project-init)
    (funcall pcm:project-init)))

;; Runs project init when a file is loaded
(add-hook 'find-file-hook 'pcm:run-project-init)

(add-hook 'after-init-hook 'post-init-stuff)
(defun post-init-stuff ()
  ;; Things that require all packages from package to be loaded
  ;; should happen here.
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (ido-vertical-mode)
  (projectile-global-mode)
)

(put 'my-project-venv 'safe-local-variable #'stringp)
(put 'pcm:project-include-path 'safe-local-variable #'listp)
(put 'pcm:project-init 'safe-local-variable #'functionp)
