;; This file contains packages that are purely used for browsing and
;; navigating around emacs, including but not limited to: searching,
;; narrowing, project navigation, use

(use-package bind-key
  :ensure t)


(use-package ripgrep
  :ensure t)


(use-package vertico                                           ;; vertical completion menu
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-resize t))


(use-package orderless                                         ;; fuzzy search completion
  :ensure t
  :config
  (setq completion-styles
          '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides
          '((file (styles basic partial-completion)))))


(use-package consult                                           ;; search techniques applied to different items
  :ensure t
  :bind (
         ;; C-x bindings
         ("C-x b" . consult-buffer)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi))
  :config
  (setq consult-narrow-key "<"))


(use-package marginalia                                        ;; side load information each functions
  :ensure t
  :config
  (marginalia-mode))


(use-package corfu                                             ;; auto-completion matches using orderless
  :ensure t
  :hook ((ledger-mode . corfu-mode)
         (go-ts-mode . corfu-mode)
         (python-ts-mode . corfu-mode))
  :config
  (setq corfu-auto t
        corfu-quit-no-match t))

(use-package cape                                              ;; extension for completion engines
  :ensure t
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-keyword))
  ;;(add-hook 'completion-at-point-functions #'cape-tex)
  ;;(add-hook 'completion-at-point-functions #'cape-sgml)
  ;;(add-hook 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-hook 'completion-at-point-functions #'cape-abbrev)
  ;;(add-hook 'completion-at-point-functions #'cape-dict)
  ;;(add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-hook 'completion-at-point-functions #'cape-line)


(use-package projectile
             :ensure t
             :bind ("C-c p" . projectile-command-map)
             :config
             (projectile-mode t)
             ;; (setq projectile-completion-system 'ivy)
             (setq projectile-enable-caching t)
             (setq projectile-indexing-method 'native))


(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  (zk-setup-embark)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
