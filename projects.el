;; Set project specific variables

;; PATH
(defconst home-path "/home/pcm")
(defconst dev-path (concat home-path "/dev"))
(defconst xivo-path (concat dev-path "/xivo"))
(defconst xivo-ctid-root (concat xivo-path "/ctid"))
(defconst xivo-dao-root (concat xivo-path "/dao"))
(defconst xivo-client-root (concat xivo-path "/client-qt"))
(defconst xivo-dird-root (concat xivo-path "/dird"))
(defconst xivo-libpython-root (concat xivo-path "/skaro/lib-python"))

;; PYTHON ROOT
(defconst xivo-dao-python-root (concat xivo-dao-root "/xivo-dao"))
(defconst xivo-libpython-python-root xivo-libpython-root)
(defconst xivo-dird-python-root (concat xivo-dird-root "/xivo-dird"))

;; PYTHONPATHS
(defconst xivo-ctid-pythonpath (list xivo-dao-python-root
				     xivo-dird-python-root
				     xivo-libpython-root))

;; Class variables
(dir-locals-set-class-variables
 'xivo-ctid-project
 `((nil . ((fill-column . 100)))
   (python-mode . ((project-pythonpath . ,xivo-ctid-pythonpath)))))

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

;; Safe evals and variables
(setq safe-local-variable-values
      '((c-basic-indent . 4)
	(c-basic-indent)
	(project-pythonpath "/home/pcm/dev/xivo/dao/xivo-dao"
			    "/home/pcm/dev/xivo/dird/xivo-dird"
			    "/home/pcm/dev/xivo/skaro/lib-python")))
