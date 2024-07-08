(set-face-attribute 'vertical-border nil                     ;; vertical border face is similar to background
                    :foreground "Gray2"
                    :background "Gray2")

(set-face-attribute 'default nil                             ;; font face and style
                    :family "Input Mono"
                    ;; :foundry "DAMA"
                    :weight 'thin
                    :height 140)

(use-package telephone-line
  :ensure t
  :config
  (setq telephone-line-lhs
        '((evil . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-buffer-modified-segment))
          (nil . (telephone-line-atom-encoding-segment))
          (accent . (telephone-line-projectile-segment))
          (nil . (telephone-line-buffer-name-segment))
          (accent . (telephone-line-position-segment))
          (nil . (telephone-line-filesize-segment))
          (accent . (telephone-line-vc-segment))))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-narrow-segment
                     telephone-line-flycheck-segment
                     telephone-line-process-segment))
          (accent . (telephone-line-major-mode-segment))))
  (setq telephone-line-evil-use-short-tag t)
  (telephone-line-mode t))


(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (text-mode . rainbow-delimiters-mode)))


(use-package dimmer
  :ensure t
  :init
  (dimmer-mode t)
  :config
  (setq dimmer-fraction 0.4)
  (dimmer-configure-which-key))


(use-package whitespace
  :ensure t
  :hook ((prog-mode . whitespace-mode)
         (ng2-html-mode . whitespace-mode)
         (yaml-mode . whitespace-mode)
         (docker-compose-mode . whitespace-mode)
         (org-mode . whitespace-mode))
  :init
  (setq whitespace-line-column 90
        whitespace-style '(face indentation::tab empty trailing)
        show-trailing-whitespace t
        whitespace-display-mappings
        '(
          (space-mark 32 [183] [46])
          (newline-mark 10 [36 10])
          (tab-mark 9 [187 9] [92 9])
          ))
  :config
  (set-face-attribute 'whitespace-line nil
                    :foreground "SlateGray3"
                    :background 'unspecified)

  (set-face-attribute 'whitespace-empty nil
                    :foreground nil
                    :background "firebrick")

  (set-face-attribute 'whitespace-indentation nil
                    :foreground nil
                    :background "#0d2d4d"))
