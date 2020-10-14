
(add-to-list 'load-path "~/.emacs.d/config/")

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

;; open config file command
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; indent buffer
(defun indent-buffer ()
  "Auto init current buffer."
  (interactive)
  (save-excursion
    ((indent-region (point-min) (point-max))
     (message "buffer indented"))))

;; === load config files
(require 'init-packages)
(require 'init-ui)
(require 'init-better-defaults)
(require 'init-org)
(require 'init-keybindings)

;;; init.el ends here
