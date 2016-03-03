;; ERC configuration for IRC

;; ERC modules to load
(add-to-list 'erc-modules 'notifications)
(add-to-list 'erc-modules 'autojoin)

;; ERC channels to join
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#xivo" "#xivo-pv" "#emacs" "#asterisk-dev")))

;; Use C-c e f to connect to IRC
(global-set-key "\C-cef" (lambda () (interactive)
                           (erc :server "irc.freenode.net" :port "6667"
                                :nick "pc-m")))

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)

;; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)

;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)
