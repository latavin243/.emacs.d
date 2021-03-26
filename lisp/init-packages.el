;;; init-package.el -- package management
;;; Commentary:
;;; package config
;;; Code:

;; important packages

;; others
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'reveal-in-osx-finder)

;; cl - Common Lisp Extension
(require 'cl)
(require 'loadhist)
(file-dependents (feature-file 'cl))

;; exec path from shell
;; Find Executable Path on OS X
(when (memq window-system '(mac ns)) (exec-path-from-shell-initialize))
(defvar exec-path-from-shell-check-startup-files nil)


(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

;; flycheck
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t)
  )


;; (use-package flyspell
;;   :ensure t
;;   )

;; (use-package wucuo
;;   :ensure t
;;   :config
;;   (add-hook 'prog-mode-hook #'wucuo-start)
;;   (add-hook 'text-mode-hook #'wucuo-start)
;;   )

(provide 'init-packages)
;;; init-packages.el ends here
