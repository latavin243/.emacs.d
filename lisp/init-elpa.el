;;; init-package.el -- package management
;;; Commentary:
;;; elpa config
;;; Code:

;; === straight-use-package
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; use-package
(straight-use-package 'use-package)
(defvar use-package-always-ensure t)
(custom-set-variables '(package-selected-packages (quote (use-package))))

(straight-use-package 'use-package-ensure-system-package)
(use-package use-package-ensure-system-package
  :ensure t
  )

;; add sources to package-archives
(add-to-list 'package-archives '(
                                 ("org" . "http://orgmode.org/elpa/")
                                 ("gnu"   . "http://elpa.emacs-china.org/gnu/")
                                 ("melpa" . "http://elpa.emacs-china.org/melpa/")
                                 ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                                 ))

(provide 'init-elpa)
;;; init-elpa.el ends here
