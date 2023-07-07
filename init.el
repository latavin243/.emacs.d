;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'package)
(add-to-list 'package-archives
             '("Melpa" . "https://melpa.org/packages/") t)

(require 'use-package)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; === load config files

;; package manager
(require 'init-elpa)

;; basic
(require 'init-defaults)
(require 'init-basic)

;; apperance
(require 'init-apperance)

;; editing (vim)
(require 'init-evil)

;; navigation
(require 'init-navigation)
(require 'init-treemacs)
(require 'init-swiper)

;; coding (language, spell check, snippet)
(require 'init-language)

;; dev tool (git, note)
(require 'init-git)
(require 'init-org)

;; to be split
;; (require 'init-packages)

;; load custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
