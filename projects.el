;; Set project specific variables

;; Safe evals and variables
(setq safe-local-variable-values
      (quote ((c-basic-indent . 4)
	      (c-basic-indent)
	      (eval progn (setq project-pythonpath
				(quote ("/home/pcm/dev/xivo/skaro/lib-python"
					"/home/pcm/dev/xivo/dird/xivo-dird"))))
	      (eval progn (setq project-pythonpath (quote ("/home/pcm/dev/xivo/skaro/lib-python"))))
	      (eval progn (setq project-pythonpath (quote ("/home/pcm/dev/xivo/stat/xivo-stat"
							   "/home/pcm/dev/xivo/dao/xivo-dao")))))))

(dir-locals-set-class-variables
 'xivo-client-qt-project
 '((nil . ((fill-column . 80)))
   (c++-mode . ((c-file-style . "stroustrup")
		(c-basic-indent . 4)
		(indent-tabs-mode . nil)))))

(dir-locals-set-directory-class
 "/home/pcm/dev/xivo/client-qt" 'xivo-client-qt-project)
