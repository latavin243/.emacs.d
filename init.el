(add-to-list 'load-path "~/.emacs.d/config/")

;; === custom functions

;; open config file command
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; === load config files
(require 'init-packages)
(require 'init-ui)
(require 'init-better-defaults)
(require 'init-org)
(require 'init-keybindings)

(setq custom-file (expand-file-name "config/custom.el" user-emacs-directory))
(load custom-file 'no-error 'no-message)
