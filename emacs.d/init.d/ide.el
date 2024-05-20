;; Requires the installation of a backend servers
;; This is nice, but it's a day job
;; So, some of it is probably not necessary, like configuration files
(use-package eglot 
  :ensure t
  :hook (go-mode-hook . eglot-ensure)
        (json-ts-mode . eglot-ensure)
        (yaml-ts-mode . eglot-ensure)
        (python-mode-hook . eglot-ensure))

;; https://www.reddit.com/r/emacs/comments/1crtk5g/sluggish_with_eglot/
;; (use-package eglot-booster
;;  :after eglot
;;  :config (eglot-booster-mode))

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
