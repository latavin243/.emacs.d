;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'package)
(add-to-list 'package-archives
             '("Melpa" . "https://melpa.org/packages/") t)

(require 'use-package)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (add-to-list 'load-path (expand-file-name (concat user-emacs-directory "lisp")))

;; === load config files
(require 'init-elpa)
(require 'init-defaults)

(require 'init-basic)
(require 'init-apperance)
;; (require 'init-keybindings)
;; (require 'init-benchmark)

(require 'init-evil)
(require 'init-lsp)

;; navigation
(require 'init-navigation)
(require 'init-treemacs)
(require 'init-swiper)

;; dev
(require 'init-git)
(require 'init-org)
(require 'init-language)

;; to be split
;; (require 'init-packages)

;; load custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
