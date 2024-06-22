;;;Winner-mode
;;可以使用 Ctrl-c ← （就是向左的箭头键）组合键，退回你的上一个窗口设置。）
;;可以使用 Ctrl-c → （前进一个窗口设置。）
(when (fboundp 'winner-mode)
  (winner-mode)
  (windmove-default-keybindings))

;;;windmove-mode
(when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings)
    (global-set-key (kbd "C-c j")  'windmove-left)
    (global-set-key (kbd "C-c l") 'windmove-right)
    (global-set-key (kbd "C-c i")    'windmove-up)
    (global-set-key (kbd "C-c k")  'windmove-down))

(provide 'init-wind)
