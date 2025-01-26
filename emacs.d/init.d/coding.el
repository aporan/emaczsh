(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))


(use-package python-mode
  :ensure t)

(use-package go-ts-mode
  :ensure t
  :config
  (setq tab-width 4
        gofmt-args '("-s=true")
        go-ts-mode-indent-offset 4))
