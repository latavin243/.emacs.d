;;; init-package.el -- package management
;;; Commentary:
;;; elpa config
;;; Code:

;; add sources to package-archives
(setq package-archives '(
  ("org" . "http://orgmode.org/elpa/")
  ("melpa" . "https://melpa.org/packages/")
  ("melpa-stable" . "https://stable.melpa.org/packages/")
  ;; ("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
  ;; ("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
  ))

;; init
(setq package-check-signature nil)

(require 'package)

;; init package manager
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; refresh package source
(unless package-archive-contents
  (package-refresh-contents))

;; use package config
(eval-and-compile
    (setq use-package-always-ensure t)
    ;; (setq use-package-always-defer t)
    (setq use-package-always-demand nil)
    (setq use-package-expand-minimally t)
    (setq use-package-verbose t))

;; install use-package-ensure-system-package
(use-package use-package-ensure-system-package)

(provide 'init-elpa)
;;; init-elpa.el ends here
