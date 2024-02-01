;;; init.el -*- lexical-binding: t no-byte-compile: t -*-
;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'cl)

(setq package-check-signature nil)

;; accelerate startup
;; Don't pass case-insensitive to `auto-mode-alist'
(setq gc-cons-threshold most-positive-fixnum)

(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Suppress file handlers operations at startup
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101)))


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
