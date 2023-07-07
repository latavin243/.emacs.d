;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'package)
(add-to-list 'package-archives
             '("Melpa" . "https://melpa.org/packages/") t)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/use-package")
(require 'use-package)

;; (with-eval-after-load 'info
;;  (info-initialize)
;;  (add-to-list 'Info-directory-list
;;               "~/.emacs.d/site-lisp/use-package/"))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (add-to-list 'load-path (expand-file-name (concat user-emacs-directory "lisp")))

;; === load config files
(require 'init-elpa)

(require 'init-defaults)
(require 'init-ui)
;; (require 'init-keybindings)
;; (require 'init-benchmark)

(require 'init-evil)
(require 'init-lsp)

;; swiper
(require 'init-swiper)

;; language
(require 'init-golang)
(require 'init-org)

;; theme
(require 'init-theme)

;; to be split
;; (require 'init-packages)

;; load custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
