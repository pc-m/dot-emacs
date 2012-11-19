;; Set project specific variables

;; PATH
(defconst home-path "/home/pcm")
(defconst dev-path (concat home-path "/dev"))
(defconst xivo-path (concat dev-path "/xivo"))
(defconst xivo-agid-root (concat xivo-path "/agid"))
(defconst xivo-ctid-root (concat xivo-path "/ctid"))
(defconst xivo-dao-root (concat xivo-path "/dao"))
(defconst xivo-client-root (concat xivo-path "/client-qt"))
(defconst xivo-dird-root (concat xivo-path "/dird"))
(defconst xivo-libpython-root (concat xivo-path "/lib-python"))

;; PYTHON ROOT
(defconst xivo-dao-python-root (concat xivo-dao-root "/xivo-dao"))
(defconst xivo-libpython-python-root (concat xivo-libpython-root "/xivo-lib-python"))
(defconst xivo-dird-python-root (concat xivo-dird-root "/xivo-dird"))

;; PYTHONPATHS
(defconst xivo-ctid-pythonpath (list xivo-dao-python-root
				     xivo-dird-python-root
				     xivo-libpython-python-root))
(defconst xivo-dao-pythonpath (list xivo-libpython-root))
(defconst xivo-agid-pythonpath (list xivo-dird-python-root
				     xivo-libpython-root))

;; Class variables
(dir-locals-set-class-variables
 'xivo-ctid-project
 `((python-mode . ((project-pythonpath . ,xivo-ctid-pythonpath)))))

(dir-locals-set-class-variables
 'xivo-agid-project
 `((python-mode . ((project-pythonpath . ,xivo-agid-pythonpath)))))

(dir-locals-set-class-variables
 'xivo-dao-project
 `((python-mode . ((project-pythonpath . ,xivo-dao-pythonpath)))))

(dir-locals-set-class-variables
 'xivo-client-qt-project
 '((nil . ((fill-column . 80)))
   (c++-mode . ((c-file-style . "stroustrup")
		(c-basic-indent . 4)
		(indent-tabs-mode . nil)))
   (c-mode . ((mode . c++)
	      (c-basic-indent . 4)
	      (indent-tabs-mode . nil)))))

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
	(project-pythonpath . ,xivo-dao-pythonpath)))
