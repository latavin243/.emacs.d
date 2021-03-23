;;; package --- Summary
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; === load config files
(require 'init-elpa)

(require 'init-better-defaults)
;; (require 'init-ui)
;; (require 'init-keybindings)

(require 'init-evil)
;; (require 'init-org)

;; to be split
;; (require 'init-packages)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(powerline-evil evil-nerd-commenter string-inflection evil-surround evil-leader use-package-ensure-system-package evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
