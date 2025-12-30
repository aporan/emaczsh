;; Requires the installation of a backend servers
;; This is nice, but it's a day job
;; So, some of it is probably not necessary, like configuration files
(use-package eglot
  :ensure t
  :hook ((python-mode . eglot-ensure)
        ;; (go-mode . eglot-ensure)
        (json-ts-mode . eglot-ensure)
        (yaml-ts-mode . eglot-ensure)
        ;; (rust-mode . eglot-ensure)
        (terraform-ts-mode . eglot-ensure)
        ;; Uncouple flymake from eglot
        ;; https://github.com/joaotavora/eglot/issues/123#issuecomment-444104870
        (eglot--managed-mode . (lambda ()
                                 (eldoc-mode -1)
                                 (flymake-mode -1))))
  :config
  (setq-default eglot-workspace-configuration
                '(:gopls (:usePlaceholders t)))

  (add-to-list 'eglot-server-programs
               '(terraform-ts-mode . ("terraform-ls" "serve"))))

;; There are changes from emacs-30; it DOES require installing emacs-lsp-booster
;; https://www.reddit.com/r/emacs/comments/1crtk5g/sluggish_with_eglot/
;; https://github.com/jdtsmith/eglot-booster
(use-package eglot-booster
 :after eglot
 :config
 (setq eglot-booster-io-only t)
 (eglot-booster-mode))

(use-package eldoc
  :ensure t
  :bind ("C-h e" . eldoc-doc-buffer)
  :config
  (setq eldoc-echo-area-use-multiline-p nil
        eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

(use-package flycheck
  :ensure t
  ;; Add the mode where you want to enable flycheck
  ;; It will use the eglot backend where available
  :hook (;;(python-ts-mode . flycheck-mode)
         ;; (go-ts-mode . flycheck-mode)
         (terraform-ts-mode . flycheck-mode)
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
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))


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
         ;; (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.19.1"))
         ;; (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod" "v1.0.2"))
         ;; (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.0"))
         (puppet . ("https://github.com/smoeding/tree-sitter-puppet"))
         (terraform . ("https://github.com/tree-sitter-grammars/tree-sitter-hcl" "v1.2.0" "dialects/terraform/src"))
         (docker . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
         (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
         (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))

  (setq treesit-auto-langs
        '(python yaml json docker terraform puppet ruby))

  ;; https://github.com/renzmann/treesit-auto?tab=readme-ov-file#what-this-package-does
  (treesit-auto-add-to-auto-mode-alist '(python json yaml terraform puppet))

  ;; (delete '(rust docker go) treesit-auto-langs)
  (global-treesit-auto-mode))


(use-package outline
  ; built-in
  :ensure nil)

(use-package bicycle
  :ensure t
  :after outline
  :bind
  ((:map outline-minor-mode-map
         ([C-tab] . bicycle-cycle)
         ([S-tab] . bicycle-cycle-global))))

(use-package prog-mode
  :config
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (add-hook 'prog-mode-hook 'hs-minor-mode))
