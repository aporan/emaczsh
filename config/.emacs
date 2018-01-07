(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(setq
 package-enable-at-startup nil
 inhibit-startup-screen t
 ansi-color-faces-vector [default default default italic underline success warning error])

(setq-default
 indent-tabs-mode nil
 truncate-lines t
 show-trailing-whitespace t
 c-basic-offset 4)

(load-theme 'tango-dark t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)
(global-auto-revert-mode t)
(set-face-attribute 'default nil
                    :family "Ubuntu Mono"
                    :foundry "DAMA"
                    :slant 'normal
                    :height 200
                    :weight 'normal
                    :width 'normal)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package org
             :ensure t
             :mode ("\\.org\\'" . org-mode)
             :bind ("C-c a" . org-agenda)
             :init
             (progn
               (setq visual-line-mode t)
               (add-hook 'org-mode-hook 'visual-line-mode))
             :config
             (progn
                (defun calendar-org-skip-subtree-if-priority (priority)
                  "Skip an agenda subtree if it has a priority of PRIORITY.

                   PRIORITY may be one of the characters ?A, ?B, or ?C."
                  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
                        (pri-value (* 1000 (- org-lowest-priority priority)))
                        (pri-current (org-get-priority (thing-at-point 'line t))))
                    (if (= pri-value pri-current)
                        subtree-end
                    nil)))

                (setq org-agenda-files '("~/Gitlab/organizer/tasks"))
                (setq org-hide-leading-stars t)
                (setq org-todo-keywords
                       '((sequence "TODO(t)" "UPCOMING(u)" "WAITING(w@)" "IN-PROGRESS(p)" "|" "DONE(d!)" "CANCELLED(c@)" "ASSIGNED(a@)")))

                (setq org-enforce-todo-dependencies t)
                (setq org-agenda-window-setup 'only-window)
                (setq org-log-into-drawer t)

                (setq org-default-priority ?Z)

                (setq org-agenda-custom-commands
                      '(("w" "Custom Compact View"
                         ((tags "PRIORITY=\"A\""
                                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                 (org-agenda-overriding-header "high-priority tasks:")))
                          (tags "PRIORITY=\"B\""
                                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                 (org-agenda-overriding-header "reasonable-priority tasks:")))
                          (agenda "" ((org-agenda-ndays 2)))
                          (alltodo ""
                                   ((org-agenda-skip-function
                                     '(or (calendar-org-skip-subtree-if-priority ?A)
                                          (calendar-org-skip-subtree-if-priority ?B)
                                          (org-agenda-skip-if nil '(scheduled deadline))))
                                    (org-agenda-overriding-header "everything else:")))))))

                (require 'org-habit)))

(use-package adaptive-wrap
             :ensure t
             :init
             (add-hook 'org-mode-hook 'adaptive-wrap-prefix-mode))

(use-package multiple-cursors
             :ensure t
             :config
             (progn
               (global-set-key (kbd "C-. C-n") 'mc/mark-next-like-this-word)
               (global-set-key (kbd "C-. C-p") 'mc/mark-previous-like-this-word)
               (global-set-key (kbd "C-. C-a") 'mc/mark-all-like-this)
               (global-set-key (kbd "C-. C-e") 'mc/edit-lines)))

(use-package ripgrep
             :ensure t)

(use-package counsel
             :ensure t)

(use-package swiper
             :ensure t
             :config
             (progn
               (ivy-mode 1)

	       (setq ivy-display-style 'fancy)
               (setq ivy-use-virtual-buffers t)
               (setq enable-recursive-minibuffers t)

               (global-set-key "\C-s" 'swiper)
               (global-set-key (kbd "C-c C-r") 'ivy-resume)
               (global-set-key (kbd "<f6>") 'ivy-resume)
               (global-set-key (kbd "M-x") 'counsel-M-x)
               (global-set-key (kbd "C-x C-f") 'counsel-find-file)
               (global-set-key (kbd "<f1> f") 'counsel-describe-function)
               (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
               (global-set-key (kbd "<f1> l") 'counsel-find-library)
               (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
               (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
               (global-set-key (kbd "C-c g") 'counsel-git)
               (global-set-key (kbd "C-c j") 'counsel-git-grep)
               (global-set-key (kbd "C-c k") 'counsel-ag)
               (global-set-key (kbd "C-x l") 'counsel-locate)
               (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)))
