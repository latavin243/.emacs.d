;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'package)
(add-to-list 'package-archives
             '("Melpa" . "https://melpa.org/packages/") t)

;; * 0. package manager
;; ** use-package
(require 'use-package)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-elpa)

;; * 1. basic
(require 'init-defaults)
(require 'init-basic)

;; * 2. ui
;; ** welcome page
;; ** floatwindow
;; ** bottom bar
;; ** theme
(require 'init-ui)

;; * 3. navigation
(require 'init-navigation)
(require 'init-treemacs)
(require 'init-swiper)

;; * 4. editing
(require 'init-editing)

;; * 5. coding & language
(require 'init-coding)
(require 'init-org)

;; * 6. dev tools
;; ** git
;; ** terminal
;; ** copilot
(require 'init-git)

;; TODO
;; (require 'init-packages)

;; load custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
