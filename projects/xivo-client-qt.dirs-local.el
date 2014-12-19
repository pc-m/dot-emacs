((nil . ((fill-column . 80)
         (eval . (setq pcm:project-include-path (list "/usr/include/x86_64-linux-gnu/qt5"
                                                      "/usr/include/x86_64-linux-gnu/qt5/QtCore"
                                                      "/usr/include/x86_64-linux-gnu/qt5/QtNetwork"
                                                      "/usr/include/x86_64-linux-gnu/qt5/QtGui"
                                                      "/usr/include/x86_64-linux-gnu/qt5/QtWidgets"
                                                      xivo-client-baselib
                                                      xivo-client-xivoclient
                                                      (concat xivo-client-baselib "/src")
                                                      (concat xivo-client-baselib "/src/storage")
                                                      (concat xivo-client-xivoclient "/src")
                                                      (concat xivo-client-xivoclient "/src/xletlib")
                                                      (concat xivo-client-xivoclient "/obj"))))
         (pcm:project-init . xivo-client-qt-init)))
   (c++-mode . ((c-file-style . "xivo")))
   (c-mode . ((mode . c++)
              (c-file-style . "xivo"))))
