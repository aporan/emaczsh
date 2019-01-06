(require 'package)                                                                            ;; load package file

(add-to-list 'package-archives                                                                ;; add melpa to package archives for more packages
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)                                                                          ;; load packages explicitly in the init file

(setq                                                                             ;; SET default global variables
                                                                                        ;; visual
 package-enable-at-startup nil                                                                ;; disable package loading after the init file
 inhibit-startup-screen t                                                                     ;; disable emacs startup splash screen
 default-frame-alist '((fullscreen . fullboth))                                               ;; always open emacs in fullscreen
 ansi-color-faces-vector [default default default italic underline success warning error])

(setq-default                                                                     ;; SET default local variables set globally
 indent-tabs-mode nil                                                                         ;; uses spaces instead of tabs
 truncate-lines t                                                                             ;; prevent texts from bleeding over the edge
                                                                                        ;; prog
 c-basic-offset 4)                                                                            ;; uses 4 spaces for indentation in a c/cpp based file

                                                                                  ;; COMMANDS
                                                                                        ;; functs
(global-auto-revert-mode t)                                                                   ;; auto load buffer after file is changed
(put 'narrow-to-region 'disabled nil)                                                         ;; allow narrow to region
                                                                                        ;; visual
(load-theme 'tango-dark t)                                                                    ;; load built-in theme
(menu-bar-mode -1)                                                                            ;; remove menu bar from display
(tool-bar-mode -1)                                                                            ;; remove tool bar from display
(scroll-bar-mode -1)                                                                          ;; remove scroll bar from display
(show-paren-mode t)                                                                           ;; highlight paranthesis pair
(set-face-attribute 'default nil                                                              ;; font face and style
                    :family "Ubuntu Mono"
                    :foundry "DAMA"
                    :slant 'normal
                    :height 200
                    :weight 'normal
                    :width 'normal)


(unless (package-installed-p 'use-package)                                                    ;; install `use-package` if its not installed
  (package-refresh-contents)
  (package-install 'use-package))


(eval-when-compile                                                                ;; REQUIRES
  (require 'use-package))                                                                     ;; reduces load time

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
               (setq visual-line-mode t                                                       ;; use visual line mode in org
                     org-src-fontify-natively t)                                              ;; syntax highlighting for code block

               (add-hook 'org-mode-hook 'visual-line-mode)
               (unbind-key "C-c C-q" org-mode-map))
             :config
             (progn
               (global-set-key (kbd "C-c c") 'org-capture)                                    ;; org capture

               (add-to-list 'org-structure-template-alist                                     ;; add blog easy template for .org files
                            '("B" "#+TITLE: ?\n#+PART: Nil\n#+DATE:\n#+UPDATE:\n\n"))

               (defun aporan/org-skip-subtree-if-priority (priority)                          ;; REFER: https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
                 "Skip an agenda subtree if it has a priority of PRIORITY.

                  PRIORITY may be one of the characters ?A, ?B, or ?C."
                 (let ((subtree-end (save-excursion (org-end-of-subtree t)))
                       (pri-value (* 1000 (- org-lowest-priority priority)))
                       (pri-current (org-get-priority (thing-at-point 'line t))))
                   (if (= pri-value pri-current)
                       subtree-end
                     nil)))

               (defun aporan/org-agenda-skip-tag (tag &optional others)                       ;; REFER: https://stackoverflow.com/a/10091330
                 "Skip all entries that correspond to TAG.

                  If OTHERS is true, skip all entries that do not correspond to TAG."
                 (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
                       (current-headline (or (and (org-at-heading-p)
                                                  (point))
                                             (save-excursion (org-back-to-heading)))))
                   (if others
                       (if (not (member tag (org-get-tags-at current-headline)))
                           next-headline
                         nil)
                     (if (member tag (org-get-tags-at current-headline))
                         next-headline
                       nil))))

               (defun aporan/agenda-prefix ()                                                 ;; REFER: https://emacs.stackexchange.com/questions/9735/org-agenda-tags-todo-hierarchy-weirdness
                 (format "%s" (aporan/agenda-indent-string (org-current-level) (org-get-priority (thing-at-point 'line t)))))

               (defun aporan/agenda-indent-string (level priority)
                 (if (= level 1)
                     ""
                   (if (= priority 0)
                       (let ((str ""))
                         (while (> level 2)
                           (setq level (1- level)
                                 str (concat str " ")))
                         (concat str "  ↯ "))
                     (let ((str ""))
                       (concat str "↬ ")))))

               (setq org-capture-templates
                     '(
                       ("t"                                                                  ;; tasks which needs to be done eventually
                        "New Todo" entry (file "")
                        "* TODO %?%^g \n  :LOGBOOK: \n   - State \"TODO\" set at %U \n  :END: \n\n  %i\n" :empty-lines 1)
                       ("u"                                                                  ;; tasks with schedule / deadline
                        "Upcoming Tasks" entry (file "")
                        "* UPCOMING %?%^g \n  SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+5d\"))\n  :LOGBOOK: \n    - State \"UPCOMING\" set at %U \n  :END: \n\n  %i\n" :empty-lines 1)))

               (setq org-priority-faces
                     '((?A . (:foreground "DeepPink" :weight 'bold))
                       (?B . (:foreground "tomato"))
                       (?C . (:foreground "turquoise"))
                       (?D . (:foreground "pale turquoise"))))

               (setq org-todo-keywords
                     '((sequence
                        "TODO(t)"                                                            ;; tasks which needs to be done
                        "UPCOMING(u)"                                                        ;; tasks with schedule / deadlines
                        "WAITING(w@)"                                                        ;; waiting for someone / something
                        "IN-PROGRESS(p)" "|"                                                 ;; tasks which are under way
                        "DONE(d!)"                                                           ;; completed tasks
                        "CANCELLED(c@)"                                                      ;; tasks which are no longer relevant
                        "ASSIGNED(a@)")))                                                    ;; delegated items to someone

               (setq org-tag-alist '(("academia" . ?a)                                       ;; categorize header tag list
                                     ("blog" . ?b)                                           ;; related to blogging
                                     ("errands" . ?e)                                        ;; related repeated / one time chores / errands
                                     ("leisure" . ?l)                                        ;; related to casual / reading / enjoyment
                                     ("vocation" . ?v)))                                     ;; career and life goal

               (setq org-hide-leading-stars t
                     org-enforce-todo-dependencies t                                         ;; enforce children dependencies on parents for todo's
                     org-lowest-priority 68                                                  ;; change lowest priority number to extend priority values
                     org-deadline-warning-days 7                                             ;; change early warning days
                     org-default-priority ?D                                                 ;; change default priority
                     org-refile-targets '((org-agenda-files :level . 1))                     ;; refile to all available agenda files
                     org-refile-use-outline-path 'file                                       ;; refile using filenames as top level
                     org-refile-allow-creating-parent-nodes 'confirm                         ;; prompt when creating top level refiles
                     org-outline-path-complete-in-steps nil                                  ;; use ivy to select names instead of stepping through
                     org-log-into-drawer t                                                   ;; log finished tasks into drawers
                     org-log-reschedule 'time                                                ;; make drawer notes when scheduled time is updated
                     org-log-redeadline 'time)                                               ;; make drawer notes when deadline is updated

               (setq org-agenda-files '("~/Gitlab/organizer/tasks/" "~/Gitlab/organizer/tasks/office")
                     org-default-notes-file "~/Gitlab/organizer/tasks/orgnotes.org"
                     org-agenda-block-separator ?                                            ;; 'empty' separator between different org agenda sections
                     org-agenda-window-setup 'only-window                                    ;; open org-agenda in a new window
                     org-agenda-skip-scheduled-if-deadline-is-shown t                        ;; skip scheduled if deadline is present
                     org-agenda-hide-tags-regexp "blog\\|errands\\|leisure\\|vocation"       ;; hide these tags in agenda view
                     org-agenda-prefix-format '(                                             ;; agenda view display category and filename
                                                (agenda . " %i %?-12t % s")
                                                (todo . " %i %(aporan/agenda-prefix)")
                                                (tags . " %i ")
                                                (search . " %i ")))


               (setq org-agenda-custom-commands                                              ;; custom org-agenda view with 3 sections filtered according to priorities
                     '(("o" "Office View"
                        ((tags "PRIORITY=\"A\""
                               ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                (org-agenda-overriding-header "High-Priority ⮔ ┻━┻ ︵ヽ(`Д´)ﾉ︵ ┻━┻")))
                         (tags "PRIORITY=\"B\""
                               ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                (org-agenda-overriding-header "Next IN Queue ⮔ (︺︹︺)")))
                         (agenda "" ((org-agenda-span 1)))
                         (alltodo ""
                                  ((org-agenda-skip-function
                                    '(or (aporan/org-skip-subtree-if-priority ?A)
                                         (aporan/org-skip-subtree-if-priority ?B)
                                         (org-agenda-skip-if nil '(scheduled deadline))))
                                   (org-agenda-overriding-header "Everything-Else ⮔ (╯°□°）╯︵ ┻━┻"))))
                        ((org-agenda-files '("~/Gitlab/organizer/tasks/office"))))

                       ("p" "Personal View"
                        ((tags "PRIORITY=\"A\""
                               ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                (org-agenda-overriding-header "High-Priority ⮔ ﴾͡๏̯͡๏﴿ O'RLY?")))
                         (tags "vocation"
                               ((org-agenda-skip-function
                                 '(or (org-agenda-skip-entry-if 'todo 'done)
                                      (org-agenda-skip-if nil '(scheduled))
                                      (aporan/org-skip-subtree-if-priority ?A)))
                                (org-agenda-prefix-format '((tags . " %i %(aporan/agenda-prefix)")))
                                (org-agenda-overriding-header "Vocation ⮔ ¯\\_(ツ)_/¯")))
                         (tags "leisure"
                               ((org-agenda-skip-function
                                 '(or (org-agenda-skip-entry-if 'todo 'done)
                                      (org-agenda-skip-if nil '(scheduled))
                                      (aporan/org-skip-subtree-if-priority ?A)))
                                (org-agenda-prefix-format '((tags . " %i %(aporan/agenda-prefix)")))
                                (org-agenda-overriding-header "Leisure ⮔ ♪~ ᕕ(ᐛ)ᕗ")))
                         (tags "errands"
                               ((org-agenda-skip-function
                                 '(or (org-agenda-skip-entry-if 'todo 'done)
                                      (org-agenda-skip-if nil '(scheduled))
                                      (aporan/org-skip-subtree-if-priority ?A)))
                                (org-agenda-prefix-format '((tags . " %i %(aporan/agenda-prefix)")))
                                (org-agenda-overriding-header "Errands ⮔ ᕙ(⇀‸↼‶)ᕗ")))
                         (tags "blog"
                               ((org-agenda-skip-function
                                 '(or (org-agenda-skip-entry-if 'todo 'done)
                                      (org-agenda-skip-if nil '(scheduled))
                                      (aporan/org-skip-subtree-if-priority ?A)))
                                (org-agenda-prefix-format '((tags . " %i %(aporan/agenda-prefix)")))
                                (org-agenda-overriding-header "Makog ⮔ (⌐■_■)")))
                         (agenda "" ((org-agenda-span 2)))
                         (alltodo ""
                                  ((org-agenda-skip-function
                                    '(or (aporan/org-skip-subtree-if-priority ?A)
                                         (aporan/org-skip-subtree-if-priority ?B)
                                         (aporan/org-agenda-skip-tag "vocation")
                                         (aporan/org-agenda-skip-tag "leisure")
                                         (aporan/org-agenda-skip-tag "blog")
                                         (aporan/org-agenda-skip-tag "errands")
                                         (aporan/org-agenda-skip-tag "leisure")
                                         (org-agenda-skip-if nil '(scheduled deadline))))
                                   (org-agenda-overriding-header "EVERYTHING-ELSE ⮔ (╯°□°）╯︵ ┻━┻"))))
                        ((org-agenda-files '("~/Gitlab/organizer/tasks/"))))))

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

(use-package ivy
             :ensure t
             :config
             (progn
               (ivy-mode 1)

               (setq ivy-display-style 'fancy)                                               ;; highlight typed words in the selection
               (setq ivy-use-virtual-buffers t)

               (global-set-key (kbd "C-c C-s") 'ivy-resume)))


(use-package counsel
             :ensure t
             :config
             (progn
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
               (global-set-key (kbd "C-c l") 'counsel-locate)

               (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)))

(use-package swiper
             :ensure t
             :config
             (progn
               (global-set-key "\C-s" 'swiper)))

(use-package editorconfig
             :ensure t
             :config
             (editorconfig-mode 1))

(use-package ng2-mode
             :ensure t
             :config
             (require 'ng2-mode))

(use-package dockerfile-mode
             :ensure t)

(use-package docker-compose-mode
             :ensure t)

(use-package restclient
             :ensure t)

(use-package dotenv-mode
             :ensure t)

(use-package vue-mode
             :ensure t)

(use-package magit
             :ensure t)

;;(use-package dart-mode
;;             :ensure t
;;             :config
;;             (progn
;;               (setq dart-sdk-path "~/Github/flutter/bin/cache/dart-sdk/")))

;; enable when configurations
;; are understood
;; (use-package eyebrowse
;;              :ensure t)

(use-package whitespace
             :ensure t
             :init
             (progn
               (setq
                whitespace-line-column 80
                whitespace-style '(face lines-tail)
                show-trailing-whitespace t)

               (add-hook 'prog-mode-hook 'whitespace-mode)
               (add-hook 'org-mode-hook 'whitespace-mode)

               (custom-set-faces
                '(whitespace-line ((t (:foreground "SlateGray3")))))))

(use-package markdown-mode
             :ensure t)
             :commands (markdown-mode gfm-mode)
             :mode  (("\\.md\\'" . gfm-mode))
             :init (setq markdown-command "multimarkdown"))

(use-package try
             :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (markdown-mode dart-mode eyebrowse editorconfig counsel ripgrep multiple-cursors adaptive-wrap use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-line ((t (:foreground "SlateGray3")))))
(put 'dired-find-alternate-file 'disabled nil)
