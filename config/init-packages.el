;; package management
(require 'package)
(package-initialize)
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			 ("melpa" . "http://elpa.emacs-china.org/melpa/")))

;; cl - Common Lisp Extension
(require 'cl)
(setq byte-compile-warnings '(cl-functions))

;; Add Packages
(defvar my/packages
  '(
    ;; --- Auto-completion ---
    company
    ;; --- Better Editor ---
    hungry-delete
    swiper
    counsel
    smartparens
    popwin
    ;; --- Major Mode ---
    js2-mode
    ;; --- Minor Mode ---
    nodejs-repl
    exec-path-from-shell
    ;; --- Themes ---
    monokai-theme
    ;; --- Others ---
    reveal-in-osx-finder
    ) "Default packages")

(setq package-selected-packages my/packages)

(defun my/packages-installed-p ()
  (loop for pkg in my/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; exec path from shell
;; Find Executable Path on OS X
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(setq exec-path-from-shell-check-startup-files nil)

;; hungry delete package config
(require 'hungry-delete)
(global-hungry-delete-mode)

;; swiper config
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)

;; smartparens config
;; (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(smartparens-global-mode t)
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

;; js files config
(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode))
       auto-mode-alist))



;; company mode
(global-company-mode t)

;; theme
(load-theme 'monokai t)

;; popwin config
(require 'popwin)
(popwin-mode t)

;; file fin
(provide 'init-packages)
