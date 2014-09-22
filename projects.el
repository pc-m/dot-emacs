;; Set project specific variables

;; PATH
(defconst home-path "/home/pcm")
(defconst dev-path (concat home-path "/dev"))
(defconst xivo-path (getenv "XIVO_PATH"))
(defconst xivo-agent-root (concat dev-path "/xivo-agent"))
(defconst xivo-agid-root (concat xivo-path "/xivo-agid"))
(defconst xivo-dao-root (concat xivo-path "/xivo-dao"))
(defconst xivo-client-root (concat dev-path "/xivo-client-qt"))
(defconst xivo-client-baselib (concat xivo-client-root "/baselib"))
(defconst xivo-client-xivoclient (concat xivo-client-root "/xivoclient"))
(defconst xivo-dird-root (concat dev-path "/xivo-dird"))
(defconst xivo-libpython-root (concat dev-path "/xivo-lib-python"))
(defconst xivo-libsccp-root (concat xivo-path "/xivo-libsccp"))
(defconst xivo-webi-root (concat xivo-path "/xivo-web-interface"))
(defconst xivo-client-include-path
  (list "/usr/share/qt4/mkspecs/linux-g++-64"
        xivo-client-baselib
        xivo-client-xivoclient
        "/usr/include/qt4/QtCore"
        "/usr/include/qt4/QtNetwork"
        "/usr/include/qt4/QtGui"
        "/usr/include/qt4"
        (concat xivo-client-baselib "/json_jsonqt/lib")
        (concat xivo-client-baselib "/src")
        (concat xivo-client-baselib "/src/storage")
        (concat xivo-client-xivoclient "/src")
        (concat xivo-client-xivoclient "/src/xletlib")
        (concat xivo-client-xivoclient "/obj")))

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
  (setq project-include-path xivo-client-include-path)
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
 'xivo-client-qt-project
 `((nil . ((fill-column . 80)))
   (c++-mode . ((c-file-style . "xivo")
                (project-init . xivo-client-qt-init)))
   (c-mode . ((mode . c++)
	      (c-file-style . "xivo")
              (project-init . xivo-client-qt-init)))))

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
 xivo-client-root 'xivo-client-qt-project)

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
	(tags-file-name . ,(concat xivo-ctid-root "/TAGS"))
	(tags-file-name . ,(concat xivo-agid-root "/TAGS"))
	(tags-file-name . ,(concat xivo-dao-root "/TAGS"))
	(tags-file-name . ,(concat xivo-libsccp-root "/TAGS"))
	(tags-file-name . ,(concat xivo-webi-root "/TAGS"))
	(cscope-initial-directory . ,xivo-libsccp-root)
        (project-init . xivo-client-qt-init)))
