;; Set project specific variables

;; PATH
(defconst home-path "/home/pcm")
(defconst dev-path (concat home-path "/dev"))
(defconst xivo-path (concat dev-path "/xivo"))
(defconst xivo-agent-root (concat dev-path "/xivo-agent"))
(defconst xivo-agid-root (concat xivo-path "/xivo-agid"))
(defconst xivo-dao-root (concat xivo-path "/xivo-dao"))
(defconst xivo-client-root (concat xivo-path "/xivo-client-qt"))
(defconst xivo-client-baselib (concat xivo-client-root "/baselib"))
(defconst xivo-client-xivoclient (concat xivo-client-root "/xivoclient"))
(defconst xivo-dird-root (concat dev-path "/xivo-dird"))
(defconst xivo-libpython-root (concat dev-path "/xivo-lib-python"))
(defconst xivo-libsccp-root (concat xivo-path "/xivo-libsccp"))
(defconst xivo-webi-root (concat xivo-path "/xivo-web-interface"))

;; PYTHON ROOT
(defconst xivo-dao-python-root (concat xivo-dao-root "/xivo-dao"))
(defconst xivo-libpython-python-root (concat xivo-libpython-root "/xivo-lib-python"))
(defconst xivo-dird-python-root (concat xivo-dird-root "/xivo-dird"))
(defconst xivo-agent-python-root (concat xivo-agent-root "/xivo-agent"))

;; PYTHONPATHS
(defconst xivo-agid-pythonpath (list xivo-dird-python-root
                                     xivo-libpython-root))

(defun xivo-client-qt-init ()
  "Runs some commands to get the XiVO client project properly setup"
  (message "XiVO client Qt init")
  (flymake-clang-c++-init)
  (flymake-clang-c++-load))

;; Class variables
(dir-locals-set-class-variables
 'xivo-agid-project
 `((nil . ((compile-command . "xm agi.unittest")
	   (tags-file-name . ,(concat xivo-agid-root "/TAGS"))))
   (python-mode . ((project-pythonpath . ,xivo-agid-pythonpath)))))

(dir-locals-set-class-variables
 'xivo-dao-project
 `((nil . ((compile-command . "xm dao.unittest")
	   (tags-file-name . ,(concat xivo-dao-root "/TAGS"))))))

(dir-locals-set-class-variables
 'xivo-libsccp-project
 `((nil . ((fill-column . 90)
	   (compile-command . "xm sccp.sync")
	   (tags-file-name . ,(concat xivo-libsccp-root "/TAGS"))
	   (cscope-initial-directory . ,xivo-libsccp-root)))
   (c-mode . ((c-file-style . "asterisk")))))

(dir-locals-set-class-variables
 'xivo-webi-project
 `((nil . ((compile-command . "xm webi.sync")
	   (tags-file-name . ,(concat xivo-webi-root "/TAGS"))))))

;; Directory to class variable assignment
(dir-locals-set-directory-class
 xivo-agid-root 'xivo-agid-project)

(dir-locals-set-directory-class
 xivo-dao-root 'xivo-dao-project)

(dir-locals-set-directory-class
 xivo-libsccp-root 'xivo-libsccp-project)

(dir-locals-set-directory-class
 xivo-webi-root 'xivo-webi-project)

;; Safe evals and variables
(setq safe-local-variable-values
      `((c-basic-indent . 4)
        (c-basic-indent)
        (project-pythonpath . ,xivo-agid-pythonpath)
	(tags-file-name . #'stringp)
	(cscope-initial-directory . ,xivo-libsccp-root)))
