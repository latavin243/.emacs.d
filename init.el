;;; package --- Summary
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; === load config files
(require 'init-elpa)
(require 'init-better-defaults)
(require 'init-packages)
(require 'init-ui)
(require 'init-org)
(require 'init-keybindings)

;;; init.el ends here
