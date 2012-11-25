(defun flymake-clang-c++-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (get-clang-command local-file)))

(defun get-clang-command (file)
  "Returns a list containing the program to run and it's arguments"
  (message "In get-clang-command")
  (let* ((a1 '("-fsyntax-only" "-fno-color-diagnostics" "-Wno-c++11-extensions"))
         (a2 (get-project-include-path))
         (a3 (list local-file)))
    (message (mapconcat 'identity a1 " "))
    (message (mapconcat 'identity a2 " "))
    (message (mapconcat 'identity a3 " "))
    (let ((args (apply 'append `(,a1 ,a2 ,a3))))
      (list "clang++" args))))

(defun flymake-clang-c++-load ()
  (interactive)
  (unless (eq buffer-file-name nil)
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.cpp\\'" flymake-clang-c++-init))
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.cc\\'" flymake-clang-c++-init))
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.h\\'" flymake-clang-c++-init)))
  (flymake-mode t))

(provide 'flymake-clang-c++)
