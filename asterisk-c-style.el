;; c style for asterisk development
(defconst asterisk-c-style
  '((c-basic-offset . 4)
    (c-basic-indent . 4)
    (tab-width . 4)
    (tab-stop-list . (number-sequence 4 200 4))
    (c-comment-only-line-offset . 0)
    (c-offsets-alist . ((statement-block-intro . +)
			(substatement-open . 0)
			(substatement-label . 0)
			(access-label . 0)
			(topmost-intro . +)
			(label . 0)
			(statement-cont . +)
			(case-label . 0))))
  "Asterisk coding style")

(c-add-style "asterisk" asterisk-c-style)

(provide 'asterisk-c-style)