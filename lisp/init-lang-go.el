(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  ;; :ensure-system-package (
  ;;   (goimports . "go get -u golang.org/x/tools/cmd/goimports")
  ;;   (godef . "go get -u github.com/rogpeppe/godef"))
  :init
  (setq gofmt-command "goimports")
  (defvar tab-width 4)
  (defvar indent-tabs-mode t)
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
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

(provide 'init-lang-go)
