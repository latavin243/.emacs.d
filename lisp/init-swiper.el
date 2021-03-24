;; swiper config
(use-package counsel)

(use-package ivy
  :bind (
         "C-c C-r" . 'ivy-resume
         )
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  )

(use-package swiper
  :after (counsel ivy)
  )

(use-package ivy-xref
  :after (ivy)
  )

(use-package ivy-hydra
  :after (ivy)
  )

(provide 'init-swiper)
