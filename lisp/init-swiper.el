;; swiper config
(use-package counsel)

(use-package ivy
  :bind (
    ("C-c C-r" . 'ivy-resume)
  )
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  )

;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "C-h f") 'counsel-describe-function)
;; (global-set-key (kbd "C-h v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
;; ;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; ;; e(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c k") 'counsel-ag)
;; (global-set-key (kbd "C-x l") 'counsel-locate)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;; (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)


(use-package swiper
  :after (counsel ivy)
  :bind (
     ("C-s" . swiper)
    )
  )

(use-package ivy-xref
  :after (ivy)
  )

(use-package ivy-hydra
  :after (ivy)
  )

(provide 'init-swiper)
