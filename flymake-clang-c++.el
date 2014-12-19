(defun flymake-clang-c++-init ()
  (let* ((a1 '("-fsyntax-only" "-fno-color-diagnostics" "--std=c++11" "-fPIC"))
         (a2 (get-project-include-path))
         (a3 (list (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))))
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
