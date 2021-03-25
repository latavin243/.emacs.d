(use-package window-numbering)

;; jump window like tmux prefix-q
(use-package ace-window
  :after (evil-leader)
  :bind (
    ("M-q" . ace-window)
         )
  :config
  (evil-leader/set-key
    "qq" 'ace-window
    "ww" 'ace-window
    ))

(provide 'init-window)
