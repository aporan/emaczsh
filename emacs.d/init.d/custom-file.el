(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(bicycle cape color-theme-modern corfu darkroom dimmer eglot-booster
             embark-consult evil exec-path-from-shell flycheck-color-mode-line
             flycheck-eglot flycheck-inline flycheck-ledger groovy-mode
             jenkinsfile-mode ledger-mode magit marginalia markdown-mode
             orderless projectile puppet-ts-mode python-mode
             rainbow-delimiters ripgrep rust-mode telephone-line
             terraform-mode terraform-ts-mode treesit-auto vertico zk))
 '(package-vc-selected-packages
   '((eglot-booster :vc-backend Git :url
                    "https://github.com/jdtsmith/eglot-booster"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package buffer-box
  ;; the ts-mode grammar association is downloaded manually
  :load-path "~/.emacs.d/custom/buffer-box/")
