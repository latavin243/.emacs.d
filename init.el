;;; package --- Summary
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (add-to-list 'load-path (expand-file-name (concat user-emacs-directory "lisp")))

;; === load config files
(require 'init-elpa)

(require 'init-better-defaults)
(require 'init-ui)
;; (require 'init-keybindings)
(require 'init-benchmark)

;; core packages
(require 'init-evil)
(require 'init-lsp)
(require 'init-swiper)

;; tools
(require 'init-window) ;; window
(require 'init-treemacs) ;; file-explorer
(require 'init-code-jump) ;; code jump
(require 'init-dashboard) ;; startup dashboard
(require 'init-mode-line) ;; mode line

;; language or filetype
(require 'init-lang-go)
(require 'init-org)

;; theme
(require 'init-theme)

;; to be split
;; (require 'init-packages)

;; load custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
