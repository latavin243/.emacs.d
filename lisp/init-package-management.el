;; auto update packages
(use-package auto-package-update
  :custom
  (auto-package-update-delete-old-versions t)
  )

;; ensure system package
(use-package use-package-ensure-system-package)

(provide 'init-package-management)
