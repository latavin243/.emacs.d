;; init-ppt.el
(use-package ox-reveal
  :ensure t
  :config
  (setq
   org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"
   org-reveal-mathjax t)
  (global-set-key [(f12)] 'org-reveal-export-to-html-and-browse)
  (reveal-mode 1)
  )

(use-package htmlize
  :ensure t)

(provide 'init-ppt)
