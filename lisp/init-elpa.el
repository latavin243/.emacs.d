;;; init-package.el -- package management
;;; Commentary:
;;; elpa config
;;; Code:

;; init
(require 'package)
(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; init package manager
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; refresh package source
(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))
(eval-when-compile
  (require 'use-package))

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
