;; ** code syntax, completion
;; ** spell check
;; ** code format
;; ** snippets
;; ** playground
;; ** runner

;; code syntax & completion
;; lsp
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  )
;; company

;; === golang
(use-package go-mode
  :after (lsp-mode)
  :mode ("\\.go\\'" . go-mode)
  ;; :ensure-system-package (
  ;;   (goimports . "go get -u golang.org/x/tools/cmd/goimports")
  ;;   (godef . "go get -u github.com/rogpeppe/godef"))
  :init
  (setq lsp-go-use-gofumpt t)
  (lsp-register-custom-settings '(("gopls.gofumpt" t)))
  ;; (setq gofmt-command "goimports")
  (defvar tab-width 4)
  (defvar indent-tabs-mode t)
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports)
         (before-save . gofmt-before-save))
  :config
  (evil-leader/set-key
    "gr" 'go-guru-referrers
    )
  ;; :bind (:map go-mode-map
  ;;             ("\C-c \C-c" . compile)
  ;;             ("\C-c \C-g" . go-goto-imports)
  ;;             ("\C-c \C-k" . godoc)
  ;;             ("M-j" . godef-jump)))
  )

(use-package gotest
  :after (go-mode)
  :bind (:map go-mode-map
              ("C-c C-f" . go-test-current-file)
              ("C-c C-t" . go-test-current-test)
              ("C-c C-p" . go-test-current-project)
              ("C-c C-b" . go-test-current-benchmark)
              ("C-x x" . go-run))
  :config
  (setq go-test-verbose t)
  )

(use-package go-tag
  :after (go-mode)
  )

;; === json
(use-package json-mode
  :ensure t
  )

;; === yaml
(use-package yaml-mode
  :ensure t
  )

;; === protobuf
(use-package protobuf-mode
  :ensure t
  )


;; spell check
;; camelCase check
;; ignore some words

;; code format
;; json format

;; snippets

;; playground

;; runner

;; org

;; end
(provide 'init-coding)
