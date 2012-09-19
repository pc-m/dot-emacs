(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "/usr/local/share/emacs/site-list")

(require 'use-package)

;; Start in server mode
(server-start)

;; Avoid being prompted for variables set in .dir-locals.el
(setq safe-local-variable-values
      (quote ((eval progn
		    (setq project-pythonpath
			  (quote ("/home/pcm/dev/xivo/skaro/lib-python"
				  "/home/pcm/dev/xivo/dird/xivo-dird"))))
	      (eval progn
		    (setq project-pythonpath
			  (quote ("/home/pcm/dev/xivo/skaro/lib-python"))))
	      (eval progn
		    (setq project-pythonpath
			  (quote ("/home/pcm/dev/xivo/stat/xivo-stat"
				  "/home/pcm/dev/xivo/dao/xivo-dao")))))))

;; Disable all flymake extensions
(setq flymake-allowed-file-name-masks (list))

(use-package cc-mode
  :config
  (progn
    (setq c-default-style '((c-mode . "k&r")))))

(use-package color-theme
  :init
  (progn
    (message "Customizing UI")
    (color-theme-initialize)				;; Load color themes
    (color-theme-charcoal-black)			;; Set a color theme
    (set-face-attribute 'default nil :height 100)	;; Set font size
    (column-number-mode t)				;; Show column number
    (setq visible-bell nil)				;; Turn off visual bell
    (setq ring-bell-function 'ignore)			;; Turn off audible bell
    (transient-mark-mode 0)				;; No selection hl
    (tool-bar-mode 0)					;; No tool bar
    (show-paren-mode t)				;; Show matching paren
))

(use-package scrollbar-mode
  :commands (scroll-bar-mode)
  :init
  (progn
    (scroll-bar-mode 0)))

(use-package speedbar
  :commands (speedbar)
  :init (global-set-key (kbd "<f7>") 'speedbar))

(add-hook
 'find-file-hook
 (lambda () (define-coding-system-alias 'UTF-8 'utf-8)))

;; Key bindings
(global-set-key (kbd "C-z") 'undo)

(use-package whitespace
  :commands whitespace-mode
  :init
  (progn
    (setq whitespace-line-column 120)
    (add-hook 'find-file-hook
	      (lambda ()
		(whitespace-mode t)))
    (setq whitespace-display-mappings
	  '((space-mark 32 [32] [46])
	    (space-mark 160 [164] [95])
	    (space-mark 2208 [2212] [95])
	    (space-mark 2336 [2340] [95])
	    (space-mark 3616 [3620] [95])
	    (space-mark 3872 [3876] [95])
	    (newline-mark 10 [36 10])
	    (tab-mark 9 [8617 9] [92 9])))))

(use-package php+-mode
  :mode (("\\.inc$" . php+-mode)
	 ("\\.php$" . php+-mode)))

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
      (compile (get-nosetests-command)))
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
			     (get-project-pythonpath)))) ":"))))

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
      (list "pyflakespep8.py" (list local-file))))
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

(use-package yasnippet
  :if (not noninteractive)
  :diminish yas/minor-mode
  :commands (yas/minor-mode yas/expand)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :load-path "~/.emacs.d/plugins/yasnippet"
  :init
  (progn
    (message "Initializing yasnippet...")
    (add-hook 'python-mode-hook
	      (lambda ()
		(yas/minor-mode 1))))
  :config
  (progn
    (message "Configuring yasnippet...")
    (yas/initialize)
    (yas/load-directory (expand-file-name "snippets/" user-emacs-directory))
    (bind-key "<tab>" 'yas/next-field-or-maybe-expand yas/keymap)))

;; Lua
(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
