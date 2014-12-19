((nil . ((fill-column . 80)
         (eval . (setq pcm:project-include-path (list "/usr/include/x86_64-linux-gnu/qt5"
                                                      "/usr/include/x86_64-linux-gnu/qt5/QtCore"
                                                      "/usr/include/x86_64-linux-gnu/qt5/QtNetwork"
                                                      "/usr/include/x86_64-linux-gnu/qt5/QtGui"
                                                      "/usr/include/x86_64-linux-gnu/qt5/QtWidgets"
                                                      (concat (projectile-project-root) "/baselib")
                                                      (concat (projectile-project-root) "/baselib/src")
                                                      (concat (projectile-project-root) "/baselib/src/storage")
                                                      xivo-client-xivoclient
                                                      (concat xivo-client-xivoclient "/src")
                                                      (concat xivo-client-xivoclient "/src/xletlib")
                                                      (concat xivo-client-xivoclient "/obj"))))
         (pcm:project-init . (lambda ()
                               (flymake-clang-c++-init)
                               (flymake-clang-c++-load)))))
   (c++-mode . ((c-file-style . "xivo")))
   (c-mode . ((mode . c++)
              (c-file-style . "xivo"))))
