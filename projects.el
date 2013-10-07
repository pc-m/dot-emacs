;; Set project specific variables

;; PATH
(defconst home-path "/home/pcm")
(defconst dev-path (concat home-path "/dev"))
(defconst xivo-path (getenv "XIVO_PATH"))
(defconst xivo-agent-root (concat dev-path "/xivo-agent"))
(defconst xivo-agid-root (concat xivo-path "/xivo-agid"))
(defconst xivo-ctid-root (concat xivo-path "/xivo-ctid"))
(defconst xivo-dao-root (concat dev-path "/xivo-dao"))
(defconst xivo-client-root (concat dev-path "/xivo-client-qt"))
(defconst xivo-client-baselib (concat xivo-client-root "/baselib"))
(defconst xivo-client-xivoclient (concat xivo-client-root "/xivoclient"))
(defconst xivo-dird-root (concat dev-path "/xivo-dird"))
(defconst xivo-libpython-root (concat dev-path "/xivo-lib-python"))
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
(defconst xivo-ctid-pythonpath (list xivo-dao-python-root
                                     xivo-dird-python-root
                                     xivo-libpython-python-root
                                     xivo-agent-python-root))
(defconst xivo-dao-pythonpath (list xivo-libpython-python-root))
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
 'xivo-ctid-project
 `((nil . ((compile-command . "xm cti.unittest")
	   (tags-file-name . ,(concat xivo-ctid-root "/TAGS"))))
   (python-mode . ((project-pythonpath . ,xivo-ctid-pythonpath)))))

(dir-locals-set-class-variables
 'xivo-agid-project
 `((nil . ((compile-command . "xm agi.unittest")
	   (tags-file-name . ,(concat xivo-agid-root "/TAGS"))))
   (python-mode . ((project-pythonpath . ,xivo-agid-pythonpath)))))

(dir-locals-set-class-variables
 'xivo-dao-project
 `((python-mode . ((project-pythonpath . ,xivo-dao-pythonpath)))))

(dir-locals-set-class-variables
 'xivo-client-qt-project
 `((nil . ((fill-column . 80)))
   (c++-mode . ((c-file-style . "xivo")
                (project-init . xivo-client-qt-init)))
   (c-mode . ((mode . c++)
	      (c-file-style . "xivo")
              (project-init . xivo-client-qt-init)))))

;; Directory to class variable assignment
(dir-locals-set-directory-class
 xivo-client-root 'xivo-client-qt-project)

(dir-locals-set-directory-class
 xivo-ctid-root 'xivo-ctid-project)

(dir-locals-set-directory-class
 xivo-agid-root 'xivo-agid-project)

(dir-locals-set-directory-class
 xivo-dao-root 'xivo-dao-project)

;; Safe evals and variables
(setq safe-local-variable-values
      `((c-basic-indent . 4)
        (c-basic-indent)
        (project-pythonpath . ,xivo-ctid-pythonpath)
        (project-pythonpath . ,xivo-agid-pythonpath)
        (project-pythonpath . ,xivo-dao-pythonpath)
	(tags-file-name . ,(concat xivo-ctid-root "/TAGS"))
	(tags-file-name . ,(concat xivo-agid-root "/TAGS"))
        (project-init . xivo-client-qt-init)))
