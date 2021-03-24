;; org mode
;; org code blocks highlight
(use-package org
  :config
  (setq org-src-fontify-natively t)
  (setq org-agenda-files '("~/org"))
  (global-set-key (kbd "C-c a") 'org-agenda)
  (setq org-capture-templates
    '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Work Schedule")
	"* TODO [#B] %?\n  %i\n"
	:empty-lines 1))
  )
  ;; org capture kbd
  (global-set-key (kbd "C-c r") 'org-capture)
 )

(use-package org-pomodoro)

(provide 'init-org)
