(require 'package)                                                                          ;; load package file
(add-to-list 'package-archives                                                              ;; add melpa to package archives for more packages
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)                                                                        ;; load packages explicitly in the init file

(setq                                                                           ;; SET default global variables
 package-enable-at-startup nil                                                              ;; disable package loading after the init file
 inhibit-startup-screen t                                                                   ;; disable emacs startup splash screen
 ansi-color-faces-vector [default default default italic underline success warning error])

(setq-default                                                                   ;; SET default local variables set globally
 indent-tabs-mode nil                                                                       ;; uses spaces instead of tabs
 truncate-lines t                                                                           ;; prevent texts from bleeding over the edge
 show-trailing-whitespace t                                                                 ;; display trailing whitespace with a color
                                                                                      ;; prog
 c-basic-offset 4)                                                                          ;; uses 4 spaces for indentation in a c/cpp based file

                                                                                ;; COMMANDS
                                                                                      ;; functs
(global-auto-revert-mode t)                                                                 ;; auto load buffer after file is changed
(put 'narrow-to-region 'disabled nil)                                                       ;; allow narrow to region
                                                                                      ;; visual
(load-theme 'tango-dark t)                                                                  ;; load built-in theme
(tool-bar-mode -1)                                                                          ;; remove tool bar from display
(scroll-bar-mode -1)                                                                        ;; remove scroll bar from display
(show-paren-mode t)                                                                         ;; highlight paranthesis pair
(set-face-attribute 'default nil                                                            ;; font face and style
                    :family "Ubuntu Mono"
                    :foundry "DAMA"
                    :slant 'normal
                    :height 200
                    :weight 'normal
                    :width 'normal)

(unless (package-installed-p 'use-package)                                                  ;; install `use-package` if its not installed
  (package-refresh-contents)
  (package-install 'use-package))


(eval-when-compile                                                              ;; REQUIRES
  (require 'use-package))                                                                   ;; reduces load time

(use-package bind-key
             :ensure t)

(use-package org
             :ensure t
             :mode ("\\.org\\'" . org-mode)
             :bind (("C-c a" . org-agenda)
                    :map org-mode-map
                    ("C-c i" . org-set-tags-command))
             :init
             (progn
               (setq
                visual-line-mode t                                                          ;; use visual line mode in org
                org-src-fontify-natively t)                                                 ;; syntax highlighting for code block
               (add-hook 'org-mode-hook 'visual-line-mode)
               (unbind-key "C-c C-q" org-mode-map))
             :config
             (progn
                (defun calendar-org-skip-subtree-if-priority (priority)                     ;; REFER: https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
                  "Skip an agenda subtree if it has a priority of PRIORITY.

                   PRIORITY may be one of the characters ?A, ?B, or ?C."
                  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
                        (pri-value (* 1000 (- org-lowest-priority priority)))
                        (pri-current (org-get-priority (thing-at-point 'line t))))
                    (if (= pri-value pri-current)
                        subtree-end
                    nil)))

                (setq org-priority-faces
                      '((?A . (:foreground "DeepPink" :weight 'bold))
                        (?B . (:foreground "tomato"))
                        (?C . (:foreground "turquoise"))
                        (?D . (:foreground "pale turquoise"))))

                (setq org-todo-keywords
                      '((sequence "TODO(t)" "UPCOMING(u)" "WAITING(w@)" "IN-PROGRESS(p)" "|" "DONE(d!)" "CANCELLED(c@)" "ASSIGNED(a@)")))

                (setq org-tag-alist '(("academia" . ?a)                                     ;; categorize header tag list
                                      ("clean" . ?c)
                                      ("giving" . ?g)
                                      ("health" . ?h)
                                      ("knowledge" . ?k)
                                      ("money" . ?m)
                                      ("order" . ?o)
                                      ("promise" . ?p)
                                      ("vocation" . ?v)))

                (setq org-hide-leading-stars t
                      org-enforce-todo-dependencies t                                       ;; enforce children dependencies on parents for todo's
                      org-log-into-drawer t                                                 ;; log finished tasks into drawers
                      org-lowest-priority 68                                                ;; change lowest priority number to extend priority values
                      org-default-priority ?D)                                              ;; change default priority

                (setq org-agenda-files '("~/Gitlab/organizer/tasks")
                      org-agenda-block-separator ?-                                         ;; separator between different org agenda sections
                      org-agenda-window-setup 'only-window                                  ;; open org-agenda in a new window
                      org-agenda-tags-column -135)

                (setq org-agenda-custom-commands                                            ;; custom org-agenda view with 3 sections filtered according to priorities
                      '(("w" "Custom Compact View"
                         ((tags "PRIORITY=\"A\""
                                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                 (org-agenda-overriding-header "high-priority tasks:")))
                          (tags "PRIORITY=\"B\""
                                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                 (org-agenda-overriding-header "reasonable-priority tasks:")))
                          (agenda "" ((org-agenda-ndays 1)))
                          (alltodo ""
                                   ((org-agenda-skip-function
                                     '(or (calendar-org-skip-subtree-if-priority ?A)
                                          (calendar-org-skip-subtree-if-priority ?B)
                                          (org-agenda-skip-if nil '(scheduled deadline))))
                                    (org-agenda-overriding-header "everything-else:")))))))

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

	       (setq ivy-display-style 'fancy)                                              ;; highlight typed words in the selection
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

(use-package editorconfig
             :ensure t
             :config
             (editorconfig-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (editorconfig counsel ripgrep multiple-cursors adaptive-wrap use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
