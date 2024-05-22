;; Requires the installation of a backend servers
;; This is nice, but it's a day job
;; So, some of it is probably not necessary, like configuration files
(use-package eglot
  :ensure t
  :hook ((go-mode . eglot-ensure)
        (json-ts-mode . eglot-ensure)
        (yaml-ts-mode . eglot-ensure)
        (python-mode . eglot-ensure)
        ;; https://github.com/joaotavora/eglot/issues/123#issuecomment-444104870
        (eglot--managed-mode . (lambda ()
                                 (eldoc-mode -1)
                                 (flymake-mode -1))))
  :config
  (setq-default eglot-workspace-configuration
        '(:gopls (:usePlaceholders t))))

;; https://www.reddit.com/r/emacs/comments/1crtk5g/sluggish_with_eglot/
;; https://github.com/jdtsmith/eglot-booster
(use-package eglot-booster
 :after eglot
 :config
 (eglot-booster-mode))

(use-package eldoc
  :ensure t
  :bind ("C-h e" . eldoc-doc-buffer)
  :config
  (setq eldoc-echo-area-use-multiline-p nil
        eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

(use-package flycheck
  :ensure t
  :hook ((python-ts-mode . flycheck-mode)
         (go-ts-mode . flycheck-mode)
         (ledger-mode . flycheck-mode))
  :config
  ;; (add-to-list 'flycheck-checkers 'python-pyflakes)
  ;; (add-to-list 'flycheck-disabled-checkers 'python-flake8)
  ;; (add-to-list 'flycheck-disabled-checkers 'python-pylint)

  ;; (use-package flycheck-pyflakes
  ;;   :after flycheck
  ;;   :ensure t
  ;;   :config
  ;;   (setq flycheck-python-pyflakes-executable "pyflakes"))

  (use-package flycheck-color-mode-line
    :after flycheck
    :ensure t
    :hook (flycheck-mode . flycheck-color-mode-line-mode))

  (use-package flycheck-inline
    :after flycheck
    :ensure t
    :hook (flycheck-mode . flycheck-inline-mode)
    :config
    (set-face-attribute 'flycheck-inline-error nil
                        :weight 'thin
                        :foreground "firebrick1"
                        :height 190))

  (use-package flycheck-ledger
    :after flycheck
    :ensure t))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  (treesit-font-lock-level '4)
  :config
  (setq treesit-language-source-alist
       '(;; (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
         ;; (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
         ;; (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
         (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
         (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
         ;; (toml "https://github.com/tree-sitter/tree-sitter-toml")
         ;; (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
         ;; (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
         (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.19.0"))
         (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod" "v1.0.2"))
         (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))

  (setq treesit-auto-langs
        '(; https://github.com/tree-sitter/tree-sitter-python
          python
          ;; https://github.com/tree-sitter/tree-sitter-go
          go
          ;; https://github.com/camdencheek/tree-sitter-go-mod
          gomod
          yaml
          json))
          ;; ; https://github.com/camdencheek/tree-sitter-dockerfile
          ;; docker))
  ;; https://github.com/renzmann/treesit-auto?tab=readme-ov-file#what-this-package-does
  (treesit-auto-add-to-auto-mode-alist '(python gomod go json yaml))
  (global-treesit-auto-mode))
