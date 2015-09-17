;; Basic C++ style for the XiVO client
(defconst xivo-cpp-style
  '((c-basic-offset . 4)
    (c-basic-indent . 4)
    (indent-tabs-mode . nil)
    (c-comment-only-line-offset . 0)
    (c-offsets-alist . ((statement-block-intro . +)
			(substatement-open . 0)
			(substatement-label . 0)
			(access-label . 0)
			(label . 0)
			(statement-cont . +)
			(topmost-intro . 0)
			(case-label . 0))))
  "XiVO client C++ Programming Style")

(c-add-style "XiVO" xivo-cpp-style)

(provide 'xivo-cpp-style)
